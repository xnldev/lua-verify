(** [Proto] models a Lua 5.1 function prototype (code, constants, debug info) *)

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

(**[read : Reader.t -> Header.t -> (t, error) result] parses a complete prototype tree *)
val read : Reader.t -> Header.t -> (t, error) result

(** [pp : Format.formatter -> t -> unit] pretty-prints prototype complete prototype instructions and constants *)
val pp : Format.formatter -> t -> unit