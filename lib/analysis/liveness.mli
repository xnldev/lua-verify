(** [Liveness] performs dataflow dataflow analysis to verify register initialization *)

open Lua_bytecode

module RegSet : Set.S with type elt = int

type use_def = {
    uses : RegSet.t;
    defs : RegSet.t;
}

type uninitialized_read = {
    pc : int;
    reg : int;
    insn : Instruction.t;
}

type report = {
    proto_id : string;
    violations : uninitialized_read list;
}

(** [insn_use_def : Instruction.t -> use_def] 
    extracts registers read (used) and registers written (defined) by a single instruction *)
val insn_use_def : Instruction.t -> use_def

(** [verify_registers : ?name:string -> Proto.t -> report] 
    runs dataflow analysis and returns all instances of reading uninitialized registers *)
val verify_registers : ?name:string -> Proto.t -> report

(** [pp_report : Format.formatter -> report -> unit] pretty-prints verification results *)
val pp_report : Format.formatter -> report -> unit