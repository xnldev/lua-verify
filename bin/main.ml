open Lua_bytecode
open Lua_analysis

(* Self-test helpers*)

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
  { op; a; b; c; bx; sbx }

let run_self_tests () =
  Format.printf " Running Built-in Self Tests \n\n";

  (* reader and header test *)
  let raw_chunk =
    "\027Lua\x51\x00\x01\x04\x08\x04\x08\x00" ^
    "\x06\x00\x00\x00\x00\x00\x00\x00hello\000" ^
    "\x00\x00\x00\x00\x00\x00\x00\x00"
  in
  
let r = Reader.of_string raw_chunk in
  (match Reader.read_header r with
  | Ok h -> Format.printf "[+] Header deserialization: PASSED (Lua %d.%d)\n" h.version_major h.version_minor
  | Error e -> Format.printf "[-] Header deserialization: FAILED (%a)\n" Reader.pp_error e);

  (* Jump checker exploit test *)
  let exploit_code = [|
    make_insn Instruction.OP_LOADK    0 0 0 0 0;
    make_insn Instruction.OP_JMP      0 0 0 99 99;
    make_insn Instruction.OP_EQ       0 1 2 0 0;
  |] in
  let exploit_reports = Jump_checker.check_proto (make_dummy_proto "exploit" exploit_code) in
  if not (Jump_checker.is_valid exploit_reports) then
    Format.printf "[+] Jump bounds checker: PASSED (successfully blocked out-of-bounds jumps)\n"
  else
    Format.printf "[-] Jump bounds checker: FAILED\n";

    (* liveness analysis test *)
    let uninit_code = [|
      make_insn Instruction.OP_MOVE   0 5 0 0 0;
      make_insn Instruction.OP_RETURN 0 2 0 0 0;
    |] in
    let uninit_report = Liveness.verify_registers (make_dummy_proto "uninit" uninit_code) in
    if uninit_report.violations <> [] then
      Format.printf "[+] Register liveness analyzer: PASSED (successfully detected uninitialized reads)\n\n"
    else
      Format.printf "[-] Register liveness analyzer: FAILED\n\n";
    
    Format.printf "All built-in verification suites executed.\n"

    (* verification of an external file *)

    let verify_file (filepath : string) (dot_out : string option) =
      if not (Sys.file_exists filepath) then begin
        Format.eprintf "Error: file not found: %s\n" filepath;
        exit 2
      end;

      Format.printf "Analyzing: %s\n\n" filepath;
      let r = Reader.of_file filepath in

      (* header *)
      let header =
        match Reader.read_header r with
        | Ok h ->
          Format.printf "[+] Header: valid Lua %d.%d (%s, size_t: %d bytes)\n"
          h.version_major h.version_minor
          (match h.endianness with Header.Little_endian -> "Little-endian" | Header.Big_endian -> "Big-endian") h.config.size_t_size;
          h
        | Error err ->
          Format.eprintf "[-] Header verification failed: %a\n" Reader.pp_error err;
        exit 1
      in

      (* prototype AST *)
      let proto =
        match Proto.read r header with
        | Ok p ->
          Format.printf "[+] Prototype: %d instructions, %d constants, %d sub-functions\n\n"
            (Array.length p.code) (Array.length p.constants) (Array.length p.sub_protos);
          p
        | Error _ ->
          Format.eprintf "[-] Prototype parsing failed\n";
          exit 1
    in

    (* jump verif *)
    let jump_reports = Jump_checker.check_proto ~name:"main" proto in
    List.iter (fun rep -> Format.printf "%a" Jump_checker.pp_report rep) jump_reports;

    (* register dataflow verification *)
    let liveness_report = Liveness.verify_registers ~name:"main" proto in
    Format.printf "%a" Liveness.pp_report liveness_report;

    (* CFG and Graphviz DOT export *)
    let cfg = Cfg.build ~name:(Filename.basename filepath) proto in (match dot_out with
    | Some out_path ->
      let dot_content = Cfg.to_dot cfg in
      Out_channel.with_open_text out_path (fun ch ->
        Out_channel.output_string ch dot_content);
      Format.printf "[+] CFG successfully exported to: %s\n" out_path;
      Format.printf "  (render to image: dot -Tpng %s -o graph.png)\n\n" out_path
    | None -> ());

    let jumps_ok = Jump_checker.is_valid jump_reports in
    let liveness_ok = liveness_report.violations = [] in

    if jumps_ok && liveness_ok then begin
      Format.printf "[✓] VERIFICATION PASSED: bytecode is formally safe.\n";
      exit 0
    end else begin
      Format.eprintf "[✗] VERIFICATION FAILED: bytecode contains security violations.\n";
      exit 1
    end

    (* entry point and CLI parsing *)

    let () =
    let input_file = ref "" in
    let dot_output = ref "" in
    let run_tests = ref false in

    let speclists = [
      ("-o", Arg.Set_string dot_output, " Path to save the Graphviz (.dot) CFG graph");
      ("--output", Arg.Set_string dot_output, " Path to save the Graphviz (.dot) CFG graph");
      ("--test", Arg.Set run_tests, " Run built-in verifier self-tests");
    ] in

    let usage_msg = "Usage: lua_verify [options] <bytecode.luac>\nOptions:" in

    Arg.parse speclists (fun anon -> input_file := anon) usage_msg;

    if !run_tests then
      run_self_tests ()
    else if !input_file = "" then begin
      Arg.usage speclists usage_msg;
      Format.printf "\nHint: to run self-tests, use: lua-verify --test\n";
      exit 0
    end else
      let dot_opt = if !dot_output = "" then None else Some !dot_output in
      verify_file !input_file dot_opt