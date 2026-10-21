open Lua_bytecode

type jump_info = {
    source_pc : int;
    target_pc : int;
    opcode : Instruction.opcode;
    sbx : int;
}

type violation = 
    | Out_of_bounds of {
        source_pc : int;
        target_pc : int;
        code_len : int;
        opcode : Instruction.opcode;
        sbx : int;
    }
    | Condition_missing_target of {
        test_pc : int;
    }

type proto_report = {
    proto_id : string;
    total_instructions : int;
    valid_jumps : jump_info list;
    violations : violation list;
}

(** checks a sigle prototpe's code array without recursion *)
let check_single (name : string) (p : Proto.t) : proto_report =
  let code_len = Array.length p.code in
  let valid_jumps = ref [] in
  let violations = ref [] in

  for pc = 0 to code_len - 1 do 
    let insn = p.code.(pc) in
    match insn.op with
    | Instruction.OP_JMP
    | Instruction.OP_FORPREP
    | Instruction.OP_FORLOOP ->
      let target_pc = pc + 1 + insn.sbx in
      if target_pc >= 0 && target_pc < code_len then
        valid_jumps := {
          source_pc = pc;
          target_pc;
          opcode = insn.op;
          sbx = insn.sbx;
        } :: !valid_jumps
      else
        violations := Out_of_bounds {
          source_pc = pc;
          target_pc;
          code_len;
          opcode = insn.op;
          sbx = insn.sbx;
        } :: !violations

      | Instruction.OP_EQ
      | Instruction.OP_LT
      | Instruction.OP_LE
      | Instruction.OP_TEST
      | Instruction.OP_TESTSET ->
        if pc + 1 >= code_len then
          violations := Condition_missing_target { test_pc = pc } :: !violations
      | _ -> ()
    done;

    {
      proto_id = name;
      total_instructions = code_len;
      valid_jumps = List.rev !valid_jumps;
      violations = List.rev !violations;
    }

(** recursively checks the prototype and all its nested sub-prototypes *)
let rec check_proto ?(name = "main") (p : Proto.t) : proto_report list =
  let root_report = check_single name p in
  let sub_reports =
    Array.to_list p.sub_protos
    |>  List.mapi (fun idx sub_p ->
      let sub_name = Printf.sprintf "%s.proto_%d" name idx in
      check_proto ~name:sub_name sub_p)
    |> List.flatten
  in
  root_report :: sub_reports

let is_valid (reports : proto_report list) : bool =
  List.for_all (fun r -> r.violations = []) reports

let pp_violation ppf = function
  | Out_of_bounds { source_pc; target_pc; code_len; opcode; sbx } ->
    Format.fprintf ppf " [VIOLATION] at pc %d (%a sBx:%d) jumps to %d (out of bounds [0..%d])"
    source_pc Instruction.pp_opcode opcode sbx target_pc (code_len - 1)
  | Condition_missing_target { test_pc } -> 
    Format.fprintf ppf " [VIOLATION] conditional test at pc %d has no following instruction"
    test_pc

let pp_report ppf (r : proto_report) =
  Format.fprintf ppf " Verification report for [%s] (%d instructions) "
  r.proto_id r.total_instructions;
  if r.valid_jumps <> [] then begin
    Format.fprintf ppf " Valid jumps (%d):\n"  (List.length  r.valid_jumps);
    List.iter (fun j ->
      Format.fprintf ppf " pc %d -> pc %d (%a)\n"
      j.source_pc j.target_pc Instruction.pp_opcode j.opcode) r.valid_jumps
  end;
  if r.violations = [] then
    Format.fprintf ppf " Status: PASSED (all jumps are in-bounds)\n\n"
  else begin
    Format.fprintf ppf "  Status: FAILED (%d violations):\n" (List.length r.violations);
    List.iter (fun v -> Format.fprintf ppf "%a\n" pp_violation v) r.violations;
    Format.fprintf ppf "\n"
  end