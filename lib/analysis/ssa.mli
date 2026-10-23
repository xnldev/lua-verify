(** [Ssa] constructs Static Single Assignment (SSA) IR with Phi-nodes from CFG *)

open Lua_bytecode

type var = {
    reg : int;
    version : int;
}

type operand =
    | Var of var
    | Const of Proto.constant

type phi = {
    dest : var;
    args : (Cfg.block_id * var) list;
}

type insn =
    | Assign of var * operand
    | Binop of Instruction.opcode * var * operand * operand
    | Unop of Instruction.opcode * var * operand
    | Return of var list
    | Other of Instruction.t

type block = {
    id : Cfg.block_id;
    phis: phi list;
    instructions : insn list;
}

type t = {
    proto_name : string;
    blocks : block list;
    edges : Cfg.edge list;
}

(** [of_cfg : Proto.t -> Cfg.t -> t] transforms CFG into SSA form *)
val of_cfg : Proto.t -> Cfg.t -> t

(** [pp_var : Format.formatter -> var -> unit] formats variable as R<reg>_v<ver> *)
val pp_var : Format.formatter -> var -> unit

(** [pp_phi : Format.formatter -> phi -> unit] formats phi node *)
val pp_phi : Format.formatter -> phi -> unit

(** [pp_insn : Format.formatter -> insn -> unit] formats SSA instruction *)
val pp_insn : Format.formatter -> insn -> unit

(** [to_dot : t -> string] exports SSA CFG to Graphviz DOT with Phi-nodes *)
val to_dot : t -> string