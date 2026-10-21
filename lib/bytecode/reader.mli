
type t

type error =
    | Unexpected_eof of { offset : int; needed : int; available : int }
    | Header_error of Header.header_error
    | Unsupported_size_t of int
    | Corrupted_string of { offset : int; reason : string }

(** create a new reader from a byte array *)
val of_string : string -> t

(** create reader from file on a disk *)
val of_file : string -> t

(** current position *)
val pos : t -> int

(** length of the data *)
val length : t -> int

(** [read_byte : t -> (int, error) result] reads 1 byte as [0..255] *)
val read_byte : t -> (int, error) result

(** [read_bytes : t -> int -> (string, error)  result] reads n bytes as a string *)
val read_bytes : t -> int -> (string, error) result

(** [read_int32 : t -> Header.endianness -> (int32, error) result] *)
val read_int32 : t -> Header.endianness ->  (int32, error) result

(** [read_int64 : t -> Header.endianness -> (int64, error) result] *)
val read_int64 : t -> Header.endianness -> (int64, error) result

(** [read_lua_number : t -> Header.endianness -> (float, error) result] *)
val read_lua_number : t -> Header.endianness -> (float, error) result

(** [read_header : t -> (Header.t, error) result] reads and validates first 12 bytes of the file *)
val read_header : t -> (Header.t, error) result

(** prints the error *)
val pp_error : Format.formatter -> error -> unit

(** [read_size_t : t -> Header.target_config -> Header.endianness -> (int, error) result]
    reads size_t based on config.size_t_size (4 or 8 bytes)  *)
val read_size_t : t -> Header.target_config -> Header.endianness -> (int, error) result

(** [read_string : t -> Header.target_config -> Header.endianness -> (string option, error) result]
    reads a Lua 5.1 string (prefixed by size_t length, with tailing null byte) *)
val read_string : t -> Header.target_config -> Header.endianness -> (string option, error) result