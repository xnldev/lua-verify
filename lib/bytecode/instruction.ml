let ( let* ) = Result.bind

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

let opcode_of_int (n : int) : (opcode, error) result =
  match n with
    | 0 -> Ok OP_MOVE       | 1 -> Ok OP_LOADK      | 2 -> Ok OP_LOADBOOL   | 3 -> Ok OP_LOADNIL
    | 4 -> Ok OP_GETUPVAL   | 5 -> Ok OP_GETGLOBAL  | 6 -> Ok OP_GETTABLE   | 7 -> Ok OP_SETGLOBAL
    | 8 -> Ok OP_SETUPVAL   | 9 -> Ok OP_SETTABLE   | 10 -> Ok OP_NEWTABLE  | 11 -> Ok OP_SELF
    | 12 -> Ok OP_ADD       | 13 -> Ok OP_SUB       | 14 -> Ok OP_MUL       | 15 -> Ok OP_DIV
    | 16 -> Ok OP_MOD       | 17 -> Ok OP_POW       | 18 -> Ok OP_UNM       | 19 -> Ok OP_NOT
    | 20 -> Ok OP_LEN       | 21 -> Ok OP_CONCAT    | 22 -> Ok OP_JMP       | 23 -> Ok OP_EQ
    | 24 -> Ok OP_LT        | 25 -> Ok OP_LE        | 26 -> Ok OP_TEST      | 27 -> Ok OP_TESTSET
    | 28 -> Ok OP_CALL      | 29 -> Ok OP_TAILCALL  | 30 -> Ok OP_RETURN    | 31 -> Ok OP_FORLOOP
    | 32 -> Ok OP_FORPREP   | 33 -> Ok OP_TFORLOOP  | 34 -> Ok OP_SETLIST   | 35 -> Ok OP_CLOSE
    | 36 -> Ok OP_CLOSURE   | 37 -> Ok OP_VARARG    | other -> Error (Invalid_opcode other)

let int_of_opcode (op : opcode) : int =
  match op with
    | OP_MOVE -> 0       | OP_LOADK -> 1      | OP_LOADBOOL -> 2   | OP_LOADNIL -> 3
    | OP_GETUPVAL -> 4   | OP_GETGLOBAL -> 5  | OP_GETTABLE -> 6   | OP_SETGLOBAL -> 7
    | OP_SETUPVAL -> 8   | OP_SETTABLE -> 9   | OP_NEWTABLE -> 10  | OP_SELF -> 11
    | OP_ADD -> 12       | OP_SUB -> 13       | OP_MUL -> 14       | OP_DIV -> 15
    | OP_MOD -> 16       | OP_POW -> 17       | OP_UNM -> 18       | OP_NOT -> 19
    | OP_LEN -> 20       | OP_CONCAT -> 21    | OP_JMP -> 22       | OP_EQ -> 23
    | OP_LT -> 24        | OP_LE -> 25        | OP_TEST -> 26      | OP_TESTSET -> 27
    | OP_CALL -> 28      | OP_TAILCALL -> 29  | OP_RETURN -> 30    | OP_FORLOOP -> 31
    | OP_FORPREP -> 32   | OP_TFORLOOP -> 33  | OP_SETLIST -> 34   | OP_CLOSE -> 35
    | OP_CLOSURE -> 36   | OP_VARARG -> 37

let decode_rk (x : int) : rk =
  if x land 0x100 <> 0 then
    Const (x land 0xFF)
  else
    Reg x

let decode (insn : int32) : (t, error) result =
  let w = Int32.to_int insn in
  let op_num = w land 0x3F in           (* bits 0..5 (mask 63) *)
  let a = (w lsr 6) land 0xFF in        (* bits 6..13 (mask 255) *)
  let c = (w lsr 14) land 0x1FF in      (* bits 14..22 (mask 511) *)
  let b = (w lsr 23) land 0x1FF in      (* bits 23..31 (mask 511) *)
  let bx = (w lsr 14) land 0x3FFFF in   (* buts 14..31 (mask 262143) *)
  let sbx = bx - 131071 in              (* MAXARG_sBx = (1 lsl 17) - 1*)

  let* op = opcode_of_int op_num in
  Ok { op; a; b; c; bx; sbx }

let pp_opcode ppf op =
  Format.fprintf ppf "%s"
  (match op with
  | OP_MOVE -> "MOVE" | OP_LOADK -> "LOADK" | OP_LOADBOOL -> "LOADBOOL" | OP_LOADNIL -> "LOADNIL"
  | OP_GETUPVAL -> "GETUPVAL" | OP_GETGLOBAL -> "GETGLOBAL" | OP_GETTABLE -> "GETTABLE" | OP_SETGLOBAL -> "SETGLOBAL"
  | OP_SETUPVAL -> "SETUPVAL" | OP_SETTABLE -> "SETTABLE" | OP_NEWTABLE -> "NEWTABLE" | OP_SELF -> "SELF"
  | OP_ADD -> "ADD" | OP_SUB -> "SUB" | OP_MUL -> "MUL" | OP_DIV -> "DIV"
  | OP_MOD -> "MOD" | OP_POW -> "POW" | OP_UNM -> "UNM" | OP_NOT -> "NOT"
  | OP_LEN -> "LEN" | OP_CONCAT -> "CONCAT" | OP_JMP -> "JMP" | OP_EQ -> "EQ"
  | OP_LT -> "LT" | OP_LE -> "LE" | OP_TEST -> "TEST" | OP_TESTSET -> "TESTSET"
  | OP_CALL -> "CALL" | OP_TAILCALL -> "TAILCALL" | OP_RETURN -> "RETURN" | OP_FORLOOP -> "FORLOOP"
  | OP_FORPREP -> "FORPREP" | OP_TFORLOOP -> "TFORLOOP" | OP_SETLIST -> "SETLIST" | OP_CLOSE -> "CLOSE"
  | OP_CLOSURE -> "CLOSURE" | OP_VARARG -> "VARARG")

let pp ppf (i : t) =
  Format.fprintf ppf "%a A:%d B:%d C:%d Bx:%d sBx:%d"
    pp_opcode i.op i.a i.b i.c i.bx i.sbx