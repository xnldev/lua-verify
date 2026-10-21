(** [Instruction] defines the 32-bit VM instructions of Lua 5.1 *)

type opcode =
    | OP_MOVE       | OP_LOADK      | OP_LOADBOOL   | OP_LOADNIL
    | OP_GETUPVAL   | OP_GETGLOBAL  | OP_GETTABLE   | OP_SETGLOBAL
    | OP_SETUPVAL   | OP_SETTABLE   | OP_NEWTABLE   | OP_SELF
    | OP_ADD        | OP_SUB        | OP_MUL        | OP_DIV
    | OP_MOD        | OP_POW        | OP_UNM        | OP_NOT
    | OP_LEN        | OP_CONCAT     | OP_JMP        | OP_EQ
    | OP_LT         | OP_LE         | OP_TEST       | OP_TESTSET
    | OP_CALL       | OP_TAILCALL   | OP_RETURN     | OP_FORLOOP
    | OP_FORPREP    | OP_TFORLOOP   | OP_SETLIST    | OP_CLOSE
    | OP_CLOSURE    | OP_VARARG

(** represents either a Register index or a Constant index (RK operand) *)
type rk =
    | Reg of int
    | Const of int

(** decoded raw instruction fields *)
type t = {
    op : opcode;
    a : int;
    b : int;
    c : int;
    bx : int;
    sbx : int;
}

type error =
    | Invalid_opcode of int

(** decodes a 9-bit B or C operand into either Reg or Const *)
val decode_rk : int -> rk

(** converts integer (0..37) to opcode *)
val opcode_of_int : int -> (opcode, error) result

(** converts opcode to integer (0..37) *)
val int_of_opcode : opcode -> int

(** [decode : int32 -> (t, error) result] decodes a 32-bit machine word *)
val decode : int32 -> (t, error) result

(** [pp_opcode : Format.formatter -> opcode -> unit] *)
val pp_opcode : Format.formatter -> opcode -> unit

(** [pp : Format.formatter -> t -> unit] pretty-prints decoded instruction *)
val pp : Format.formatter -> t -> unit