let (let*) = Result.bind

type t = {
  data : string;
  mutable pos : int;
}

type error = 
  | Unexpected_eof of { offset : int; needed : int; available : int }
  | Header_error of Header.header_error
  | Unsupported_size_t of int
  | Corrupted_string of { offset : int; reason : string }

let of_string (s : string) : t =
  { data = s; pos = 0}

let of_file (path : string) : t = 
  let content = In_channel.with_open_bin path
In_channel.input_all in
  of_string content


let pos (r : t) : int = r.pos

let length (r : t) : int = String.length r.data

let ensure (r : t) (needed : int) : (unit, error) result =
  let available = String.length r.data - r.pos in
  if available < needed then
    Error (Unexpected_eof { offset = r.pos; needed; available})
  else
    Ok ()

let read_byte (r : t) : (int, error) result =
  match ensure r 1 with
  | Error e -> Error e
  | Ok () ->
    let b = Char.code r.data.[r.pos] in
    r.pos <- r.pos + 1;
    Ok b

let read_bytes (r : t) (n : int) : (string, error) result = 
  match ensure r n with
  Error e -> Error e
  | Ok () ->
    let s = String.sub r.data  r.pos n in
    r.pos <-  r.pos + n;
    Ok s

let read_int32 (r : t) (endian : Header.endianness) : (int32, error) result =
  match read_bytes r 4 with
  | Error e -> Error e
  | Ok s ->
    let b i = Int32.of_int (Char.code s.[i]) in
    let open Int32 in
    let res =
      match endian with
      | Header.Little_endian ->
            logor (b 0)
              (logor (shift_left (b 1) 8)
                (logor (shift_left (b 2) 16) (shift_left (b 3) 24)))
      | Header.Big_endian ->
        logor (shift_left (b 0) 24)
          (logor (shift_left (b 1) 16)
            (logor (shift_left (b 2) 8) (b 3)))
    in
    Ok res

let read_int64 (r : t) (endian : Header.endianness) : (int64, error) result =
  match read_bytes r 8 with
  | Error e -> Error e
  | Ok s ->
    let b i = Int64.of_int (Char.code s.[i]) in
    let open Int64 in
    let res =
      match endian with
      | Header.Little_endian ->
        logor (b 0)
          (logor (shift_left (b 1) 8)
            (logor (shift_left (b 2) 16)
              (logor (shift_left (b 3) 24)
                (logor (shift_left (b 4) 32)
                  (logor (shift_left (b 5) 40)
                    (logor (shift_left (b 6) 48) (shift_left  (b 7) 56)))))))
      | Header.Big_endian ->
        logor (shift_left (b 0) 56)
          (logor (shift_left (b 1) 48)
            (logor (shift_left (b 2) 40)
              (logor (shift_left (b 3) 32)
                (logor (shift_left (b 4) 24)
                  (logor (shift_left (b 5) 16)
                    (logor (shift_left (b 6) 8) (b 7)))))))
      in
      Ok res

let read_lua_number (r : t) (endian : Header.endianness) : (float, error) result =
  match read_int64 r endian with
  | Ok i64 -> Ok (Int64.float_of_bits i64)
  | Error e -> Error e

let read_size_t (r : t) (config : Header.target_config) (endian : Header.endianness)
  : (int, error) result =
  match config.size_t_size with
  | 4 ->
    let* n = read_int32 r endian in
    Ok (Int32.to_int n)
  | 8 ->
    let* n = read_int64 r endian in
    Ok (Int64.to_int n)
  | other ->
    Error (Unsupported_size_t other)

let read_string (r : t) (config : Header.target_config) (endian : Header.endianness)
  : (string option, error) result =
  let* size = read_size_t r config endian in
  if size = 0 then 
    Ok None
  else 
    let* raw = read_bytes r size in
    if raw.[size - 1] <> '\000' then
      Error (Corrupted_string { offset = r.pos - size; reason = "Missing NUL terminator"})
    else
      let content = String.sub raw 0 (size - 1) in
    Ok (Some content)


let read_header (r : t) : (Header.t, error)  result  =
  (* read 4-byte signature *)
  let* sig_str = read_bytes r 4 in
  let* () =
    if sig_str = "\027Lua" then Ok ()
    else Error (Header_error (Header.Bad_signature sig_str))
  in

  (* version *)

  let* v_byte = read_byte r in
  let version_major = v_byte lsr 4 in
  let version_minor = v_byte land 0x0F in

  (* format *)
  let* format_version = read_byte r in

  (* endianness *)
  let* endian_byte = read_byte r in
  let* endianness =
    match endian_byte with
    | 1 -> Ok Header.Little_endian
    | 0 -> Ok Header.Big_endian
    | other -> Error (Header_error (Header.Unknown_endianness other))
  in

  (* type size *)
  let* int_size = read_byte r in
  let* size_t_size = read_byte r in
  let* instruction_size = read_byte r in
  let* lua_number_size = read_byte r in
  let* integral_byte = read_byte r in
  let integral_flag = integral_byte <> 0 in

  (* *)
  let (h : Header.t) = {
    version_major;
    version_minor;
    format_version;
    endianness;
    config = {
    int_size;
    size_t_size;
    instruction_size;
    lua_number_size;
    integral_flag;
    };
  } in

  (* validate with Header.validate_header*)
  match Header.validate_header h with
  | Ok valid_h -> Ok valid_h
  | Error err -> Error (Header_error err)

let pp_error ppf = function
  | Unexpected_eof { offset; needed; available } ->
    Format.fprintf ppf "Unexpected EOF at offset %d: needed %d bytes, but only %d available"
      offset needed available
  | Header_error err ->
    Header.pp_error ppf err
  | Unsupported_size_t sz ->
    Format.fprintf ppf "Unsupported size_t size: %d bytes (expected 4 or 8)" sz
  | Corrupted_string { offset; reason } ->
    Format.fprintf ppf "Corrupted string at offset %d: %s" offset reason


