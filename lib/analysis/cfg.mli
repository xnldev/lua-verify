(** [Cfg] builds and visualizes the Control Flow Graph of Lua 5.1 bytecode *)

open Lua_bytecode

type block_id = int

type edge_kind = 
    | Fallthrough
    | Jump
    | Cond_true
    | Cond_false

type edge = {
    src : block_id;
    dst : block_id;
    kind : edge_kind;
}

type basic_block = {
    id : block_id;
    start_pc : int;
    end_pc : int;
    instructions : Instruction.t array;
}

type t = {
    proto_name : string;
    blocks : basic_block list;
    edges : edge list;
    entry : block_id;
}

(** [build : ?name:string -> Proto.t -> t] constructs the CFG from a function prototype *)
val build : ?name:string -> Proto.t -> t

(** [to_dot : t -> string] exports the CFG in Graphviz DOT format *)
val to_dot : t -> string

(** [pp_dot : Format.formatter -> t -> unit] pretty-prints DOT directly to a formatter *)
val pp_dot : Format.formatter -> t -> unit