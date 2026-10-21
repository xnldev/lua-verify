open Lua_bytecode
open Lua_analysis

let make_dummy_proto name (code : Instruction.t array) : Proto.t =
  {
    source = Some ("@" ^ name ^ ".lua");
    line_defined = 1;
    last_line_defined = 10;
    num_upvalues = 0;
    num_params = 0;
    is_vararg = 0;
    max_stack_size = 2;
    code;
    constants = [||];
    sub_protos = [||];
    source_lines = [||];
    local_vars = [];
    upvalue_names = [];
  }

let make_insn op a b c bx sbx : Instruction.t = 
  { op; a; b; c; bx; sbx;}

let () =
  Format.printf " Testing Lua 5.1 Bytecode Jump Checker \n\n";

  (* valid prototype *)
  let valid_code = [|
    make_insn Instruction.OP_LOADK    0 0 0 0 0;  (* pc 0 *)
    make_insn Instruction.OP_JMP      0 0 0 1 1;  (* pc 1: jumps to 1 + 1 + 1 = 3 *)
    make_insn Instruction.OP_LOADNIL  0 1 0 0 0;  (* pc 2 *)
    make_insn Instruction.OP_RETURN   0 1 0 0 0;  (* pc 3 *)
  |] in
  let valid_proto = make_dummy_proto "valid_function" valid_code in
  let reports_valid = Jump_checker.check_proto ~name:"valid_func" valid_proto in
  List.iter (fun r -> Format.printf "%a" Jump_checker.pp_report r) reports_valid;

  (* malicious prototype (out of bounds and missing target) *)
  let malicious_code = [|
    make_insn Instruction.OP_LOADK    0 0 0 0 0;    (* pc 0 *)
    make_insn Instruction.OP_JMP      0 0 0 99 99;  (* pc 1: jumps to 1 + 1 + 99 = 101 (OOB) *)
    make_insn Instruction.OP_EQ       0 1 2 0 0;    (* pc 2: condition at the very end of code *)
  |] in
  let malicious_proto = make_dummy_proto "exploit_attempt" malicious_code in
  let reports_malicious = Jump_checker.check_proto ~name:"exploit_func" malicious_proto in
  List.iter (fun r -> Format.printf "%a" Jump_checker.pp_report r) reports_malicious;

  Format.printf "Verification summary: valid_func is %s, exploit_func is %s\n\n"
    (if Jump_checker.is_valid reports_valid then "VALID" else "INVALID")
    (if Jump_checker.is_valid reports_malicious then "VALID" else "INVALID")

let () =
  Format.printf "Testing Lua 5.1 Binary Reader \n\n";
  (* Simulated raw binary stream: 
  - 12-byte header: "\027Lua" +  version 0x51 + format 0x00 + LE 0x01
  + int(4) + size_t(8) + insn(4) + num(8) + float(0)
  - String "hello": 8-byte length (6) + "hello\000"
  - NULL string:    8-byte length (0)
  *)

  let raw_chunk =
    "\027Lua\x51\x00\x01\x04\x08\x04\x08\x00" ^
    "\x06\x00\x00\x00\x00\x00\x00\x00hello\000" ^
    "\x00\x00\x00\x00\x00\x00\x00\x00"
  in
  
  let r = Reader.of_string raw_chunk in

  (* read and validate header*)
  match Reader.read_header r with
  | Error err  ->
    Format.printf "Failed to parse header: %a\n" Reader.pp_error err
  | Ok header ->
    Format.printf "Header parsed successfully!\n";
    Format.printf " Lua version : %d.%d\n" header.version_major header.version_minor;
    Format.printf " Endianness  : %s\n"
      (match header.endianness with Header.Little_endian -> "Little-endian" | Header.Big_endian -> "Big-endian");
    Format.printf " size_t size : %d bytes\n\n" header.config.size_t_size;
  
  (* read first string ("hello") *)
  match Reader.read_string r header.config header.endianness with
  | Error err ->
    Format.printf "Failed to read string 1: %a\n" Reader.pp_error err
  | Ok (Some str) ->
    Format.printf "String 1 parsed: %S (length: %d)\n" str (String.length str)
  | Ok None ->
    Format.printf "String 1 is NULL\n";

  (* read second string (NULL) *)
  match Reader.read_string r header.config header.endianness with
  | Error err ->
    Format.printf "Failed to read string 2: %a\n" Reader.pp_error err
  | Ok (Some str) ->
    Format.printf "String 2 parsed: %S\n" str
  | Ok None ->
    Format.printf "String 2 is NULL (as expected)\n\n";
  
  Format.printf "All binary primitives verified successfully! Reader pos: %d%d bytes\n"
  (Reader.pos r) (Reader.length r)


let test_header name (h : Header.t) =
  Format.printf "Testing %s \n" name;
  match Header.validate_header h with
  | Ok _ ->
    Format.printf "Status: OK (valid Lua 5.1 header)\n\n"
  | Error e ->
    Format.printf "Status: FAILED -> %a\n\n" Header.pp_error e

let () =
  (*Test 1: Valid header for x86_64 *)
  let valid_h : Header.t = {
    version_major = 5;
    version_minor = 1;
    format_version = 0;
    endianness = Header.Little_endian;
    config = Header.default_config_64;
  } in
  test_header "Valid x86_64" valid_h;

  (*Test 2: Header from and unsupported Lua 5.2version *)
  let invalid_version_h : Header.t = {
    valid_h with
    version_major = 5;
    version_minor = 2;
  } in
  test_header "Invalid Version (5.2)"  invalid_version_h;

  (* Test 3: Header with invalid instruction size *)
  let invalid_insn_h : Header.t = {
    valid_h with
    config = { valid_h.config with instruction_size = 2};
  } in
  test_header "Invalid Instruction Size (2)" invalid_insn_h


let () =
  Format.printf " Testing Control Flow Graph (CFG) and DOT Export \n\n";
  (* bytecode representing:
    pc 0: LOADK R0 K0
    pc 1: EQ 0, R0, R1      -- if R0 == R1 skips pc 2 (jumps to pc 3)
    pc 2: JMP 2             -- else jumps to pc 5
    pc 3: LOADNIL R0, R1    -- then-branch
    pc 4: JMP 1             -- jumps to pc 6
    pc 5: LOADBOOL R0, 1, 0 -- else-branch
    pc 6: RETURN R0, 1      -- exit
  *)

  let if_else_code = [|
    make_insn Instruction.OP_LOADK    0 0 0 0 0;    (* 0 *)
    make_insn Instruction.OP_EQ       0 0 1 0 0;    (* 1 *)
    make_insn Instruction.OP_JMP      0 0 0 2 2;    (* 2: target = 2 + 1 + 2 = 5 *)
    make_insn Instruction.OP_LOADNIL  0 1 0 0 0;    (* 3 *)
    make_insn Instruction.OP_JMP      0 0 0 1 1;    (* 4: target = 4 + 1 + 1 = 6 *)
    make_insn Instruction.OP_LOADBOOL 0 1 0 0 0;    (* 5 *)
    make_insn Instruction.OP_RETURN   0 1 0 0 0;    (* 6 *)
  |] in

  let proto = make_dummy_proto "branching_demo" if_else_code in
  let cfg = Cfg.build ~name:"branching_demo" proto in

  Format.printf "CFG constructed: %d basic blocks, %d edges\n\n"
    (List.length cfg.blocks) (List.length cfg.edges);
  
  Format.printf "Graphviz DOT Output\n";
  Format.printf "%s\n" (Cfg.to_dot cfg)

let () =
  Format.printf "Testing Dataflow Register Liveness Verification\n\n";

  (* safe prototype: 1 parameter R(0), safe operations *)
  let safe_code = [|
    make_insn Instruction.OP_MOVE       1 0 0 0 0;      (* R(1) := R(0) *)
    make_insn Instruction.OP_LOADK      2 0 0 0 0;      (* R(2) := K(0) *)
    make_insn Instruction.OP_ADD        3 1 2 0 0;      (* R(3) := R(1) + R(2) *)
    make_insn Instruction.OP_RETURN     3 2 0 0 0;      (* return R(3) *)
  |] in
  let safe_proto = { (make_dummy_proto "safe_fn" safe_code) with num_params = 1 } in
  let safe_report = Liveness.verify_registers ~name:"safe_fn" safe_proto in
  Format.printf "%a" Liveness.pp_report safe_report;

  (* direct uninitialized read: reads R(5) without writing *)
  let direct_bug_code = [|
    make_insn Instruction.OP_MOVE       0 5 0 0 0;      (* R(0) := R(5) (R(5) is UNINITIALIZED!) *)
    make_insn Instruction.OP_RETURN     0 2 0 0 0;
  |] in
  let direct_bug_proto = make_dummy_proto "direct_bug_fn" direct_bug_code in
  let direct_report = Liveness.verify_registers ~name:"direct_bug_fn" direct_bug_proto in
  Format.printf "%a" Liveness.pp_report direct_report;

  (* conditional branch bug: R(1) only initialized in 'then' branch *)
  let branch_bug_code = [|
    make_insn Instruction.OP_EQ         0 0 256 0 0;      (* pc 0: if R(0) == R(1) skips next *)
    make_insn Instruction.OP_JMP        0 0 0 2 2;      (* pc 1: else jumps to pc 4 *)
    make_insn Instruction.OP_LOADK      1 0 0 0 0;      (* pc 2 (then): R(1) := K(0) *)
    make_insn Instruction.OP_JMP        0 0 0 1 1;      (* pc 3: jumps to pc 5 *)
    make_insn Instruction.OP_LOADK      2 0 0 0 0;      (* pc 4 (else): R(2) := K(0) (R(1) NOT set!) *)
    make_insn Instruction.OP_RETURN     1 2 0 0 0;      (* pc 5: return R(1) (UNINITIALIZED on 'else' path!) *)
  |] in
  let branch_bug_proto = { (make_dummy_proto "branch_bug_fn" branch_bug_code) with num_params = 1 } in
  let branch_report = Liveness.verify_registers ~name:"branch_bug_fn" branch_bug_proto in
  Format.printf "%a" Liveness.pp_report branch_report