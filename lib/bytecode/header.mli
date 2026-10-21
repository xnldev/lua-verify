(** [Header] models and validates a 12-byte Lua 5.1 binary chunk header *)

type endianness =
    | Little_endian
    | Big_endian


type target_config = {
    int_size : int;
    size_t_size : int;
    instruction_size : int;
    lua_number_size : int;
    integral_flag : bool;
}

type t = {
    version_major : int;
    version_minor : int;
    format_version : int;
    endianness : endianness;
    config : target_config;
}

type header_error =
    | Bad_signature of string
    | Unsupported_version of { major : int; minor : int }
    | Unsupported_format of int
    | Unknown_endianness of int
    | Incompatible_instruction_size of int
    | Incompatible_lua_number_size of int



(** [default_config_64] - default target configuration for x86_64 systems
    (int=4, size_t=8, instruction=4, lua_number=8, integral=false). *)
val default_config_64 : target_config

(** [default_config_32] - default target configuration for x32 systems
    (int=4, size_t=4, instruction=4, lua_number=8, integral=false). *)
val default_config_32 : target_config

(** [validate_header : t -> (t, header_error) result] checks if a header is valid *)
val validate_header : t -> (t, header_error) result

(** [pp_error : Format.formatter -> header_error -> unit] prints a header error *)
val pp_error : Format.formatter -> header_error -> unit

