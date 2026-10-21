(** [default_config_64] - default target configuration for x86_64 systems
    (int=4, size_t=8, instruction=4, lua_number=8, integral=false). *)

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


let default_config_64 : target_config = {
    int_size = 4;
    size_t_size = 8;
    instruction_size = 4;
    lua_number_size = 8;
    integral_flag = false;
}


let default_config_32 : target_config = {
    int_size = 4;
    size_t_size = 4;
    instruction_size = 4;
    lua_number_size = 8;
    integral_flag = false;
}

let  ( let* ) = Result.bind

let check cond err = 
    if cond then Ok () else Error err

let validate_header (h : t) : (t, header_error) result =
    let* () = 
        check (h.version_major = 5 && h.version_minor = 1)
        (Unsupported_version { major = h.version_major; minor = h.version_minor })
    in
    let* ()  = 
        check (h.format_version = 0)
        (Unsupported_format h.format_version)
    in
    let* () =
        check (h.config.instruction_size = 4)
            (Incompatible_instruction_size h.config.instruction_size)
    in 
    let* () =
        check (h.config.lua_number_size = 8)
            (Incompatible_lua_number_size h.config.lua_number_size)
    in
    Ok h

let pp_error ppf = function
    | Bad_signature sig_str ->
        Format.fprintf ppf "Bad Lua signature: %S (expected \\033Lua)" sig_str
    | Unsupported_version { major; minor } ->
        Format.fprintf ppf "Unsupported Lua version: %d.%d (expected 5.1)" major minor
    | Unsupported_format fmt ->
        Format.fprintf ppf "Unsupported Lua format: %d (expected 0)" fmt
    | Unknown_endianness e ->
        Format.fprintf ppf "Unknown endianness flag: %d (expected 0 or 1)" e
    | Incompatible_instruction_size sz ->
        Format.fprintf ppf "Unsupported instruction size: %d bytes (expected 4)" sz
    | Incompatible_lua_number_size sz ->
        Format.fprintf ppf "Unsupported lua_Number size: %d bytes (expected 8)" sz

