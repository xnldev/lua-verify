let ( let* ) = Result.bind

type constant = 
  | Nil
  | Boolean of bool
  | Number of float
  | String of string

type local_var = {
  var_name : string;
  start_pc : int;
  end_pc : int;
}

type t = {
    source : string option;
    line_defined : int;
    last_line_defined : int;
    num_upvalues : int;
    num_params : int;
    is_vararg : int;
    max_stack_size : int;
    code : Instruction.t array;
    constants : constant array;
    sub_protos : t array;
    source_lines : int array;
    local_vars : local_var list;
    upvalue_names : string list;
}

type error =
    | Reader_error of Reader.error
    | Instruction_error of Instruction.error
    | Unknown_constant_tag of int

let lift_reader = function
  | Ok v -> Ok v
  | Error e -> Error (Reader_error e)

let lift_insn = function
  | Ok v -> Ok v
  | Error e -> Error (Instruction_error e)

(** helper to read an integer (sizeof(int) = 4 bytes) *)
let read_int (r : Reader.t) (header : Header.t) : (int, error)
result =
  let* n = lift_reader (Reader.read_int32 r header.endianness) in
  Ok (Int32.to_int n)

(** reads a list of [n] items using item reader [f] *)
let rec read_list n f =
  if n <= 0 then Ok []
  else
    let* head = f () in
    let* tail = read_list (n - 1) f in
    Ok (head :: tail)

let read_code (r : Reader.t) (header : Header.t) : (Instruction.t array, error) result =
  let* n = read_int r header in
  let* insns =
    read_list n (fun () ->
      let* raw_insn = lift_reader (Reader.read_int32 r header.endianness) in
      lift_insn (Instruction.decode raw_insn))
    in
    Ok (Array.of_list insns)

let read_constant (r : Reader.t) (header : Header.t) : (constant, error) result =
  let* tag = lift_reader (Reader.read_byte r) in
  match tag with
  | 0 -> Ok Nil
  | 1 -> 
    let* b = lift_reader (Reader.read_byte r) in
    Ok (Boolean (b <> 0))
  | 3 -> 
    let* num = lift_reader (Reader.read_lua_number r header.endianness) in
    Ok (Number num)
  | 4 ->
    let* str_opt = lift_reader (Reader.read_string r header.config header.endianness) in
    Ok (String (Option.value ~default:"" str_opt))
  | other ->
    Error (Unknown_constant_tag other)

let read_constants (r : Reader.t) (header : Header.t) : (constant array, error) result = 
  let* n = read_int r header in
  let* consts = read_list n (fun () -> read_constant r header) in
  Ok (Array.of_list consts)

let rec read (r : Reader.t) (header : Header.t) : (t, error) result =
  (* metadata *)
  let* source = lift_reader (Reader.read_string r header.config header.endianness) in
  let* line_defined = read_int r header in
  let* last_line_defined = read_int r header in
  let* num_upvalues = lift_reader (Reader.read_byte r) in
  let* num_params = lift_reader (Reader.read_byte r) in
  let* is_vararg = lift_reader (Reader.read_byte r) in
  let* max_stack_size = lift_reader (Reader.read_byte r) in

  (* instructions code *)
  let* code = read_code r header in

  (* constants *)
  let* constants = read_constants r header in

  (* sub-prototypes *)
  let* num_sub_protos = read_int r header in
  let* sub_protos_list = read_list num_sub_protos (fun () -> read r header) in
  let sub_protos = Array.of_list sub_protos_list in

  (* debug info: line numbers *)
  let* num_lines = read_int r header in
  let* lines = read_list num_lines (fun () -> read_int r header) in
  let source_lines = Array.of_list lines in

  (* debug info: local variables *)
  let* num_locals = read_int r header in
  let* local_vars =
    read_list num_locals (fun () -> 
      let* var_name_opt = lift_reader (Reader.read_string r header.config header.endianness) in
      let var_name = Option.value ~default:"" var_name_opt in
      let* start_pc = read_int r header in
      let* end_pc = read_int r header in
      Ok { var_name; start_pc; end_pc})
  in

  (* debug info: upvalue names *)
  let* num_upvalue_names = read_int r header in
  let* upvalue_names =
    read_list num_upvalue_names (fun () ->
      let* name_opt = lift_reader (Reader.read_string r header.config header.endianness) in
      Ok (Option.value ~default:"" name_opt))
    in

    Ok {
      source;
      line_defined;
      last_line_defined;
      num_upvalues;
      num_params;
      is_vararg;
      max_stack_size;
      code;
      constants;
      sub_protos;
      source_lines;
      local_vars;
      upvalue_names;
    }

let pp ppf (proto : t) =
  Format.fprintf ppf "Proto: params=%d, stack=%d, insns=%d, consts=%d"
    proto.num_params  proto.max_stack_size
    (Array.length proto.code) (Array.length proto.constants)