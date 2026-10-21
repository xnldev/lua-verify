(** [Jump_checker] validates all relative jumps within function prototypes *)

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

(** [check_proto : ?name:string -> Proto.t -> proto_report list]
    Checks the protoype and recursively all nested sub-prototypes
    Returns a report for each prototype in the hierarchy*)
val check_proto : ?name:string -> Proto.t -> proto_report list

(** [is_valid : proto_report list -> bool] returns true if there are zero violations *)
val is_valid : proto_report list -> bool

(** [pp_report : Format.formatter -> proto_report -> unit] pretty-prints verif results *)
val pp_report : Format.formatter -> proto_report -> unit