type error = Invalid_content of string | Truncated of string

let error (e : error) = Error (`Gzip e)

let pp_gzip_error ppf (`Gzip (e : error)) =
  match e with
  | Invalid_content msg -> Format.fprintf ppf "Invalid GZIP content: %s" msg
  | Truncated msg -> Format.fprintf ppf "Truncated gzip content: %s" msg

exception Exceeded_max_size

let default_buffer_size = ref 65_536

let refill s =
  let src_pos = ref 0 in
  fun bs ->
    let len = min (Bigstringaf.length bs) (String.length s - !src_pos) in
    Bigstringaf.blit_from_string s ~src_off:!src_pos bs ~dst_off:0 ~len;
    src_pos := !src_pos + len;
    len

let flush ~max_size decompressed_buf scratch_buf src len =
  if Buffer.length decompressed_buf + len > max_size then
    raise Exceeded_max_size
  else (
    Bigstringaf.blit_to_bytes src ~src_off:0 scratch_buf ~dst_off:0 ~len;
    Buffer.add_subbytes decompressed_buf scratch_buf 0 len )

let make_io_bigstrings buffer_size =
  (Bigstringaf.create buffer_size, Bigstringaf.create buffer_size)

let uncompress ?(max_size = Sys.max_string_length)
    ?(buffer_size = !default_buffer_size) gz =
  let i, o = make_io_bigstrings buffer_size in
  let refill = refill gz in
  let buffer = Buffer.create buffer_size in
  let scratch = Bytes.create buffer_size in
  let flush = flush ~max_size buffer scratch in
  match Gz.Higher.uncompress ~i ~o ~refill ~flush with
  | Ok _meta -> Ok (Buffer.contents buffer)
  | Error (`Msg m) -> error (Invalid_content m)
  | exception Exceeded_max_size -> error (Truncated (Buffer.contents buffer))

let compress ?level ?(buffer_size = !default_buffer_size) raw =
  let i, o = make_io_bigstrings buffer_size in
  let w = De.make_window ~bits:15 in
  let q = De.Queue.create 4_096 in
  let refill = refill raw in
  let buffer = Buffer.create buffer_size in
  let scratch = Bytes.create buffer_size in
  let flush = flush ~max_size:Sys.max_string_length buffer scratch in
  let mtime () = 0l in
  let config = Gz.Higher.configuration Unknown mtime in
  Gz.Higher.compress ?level ~w ~q ~i ~o ~refill ~flush () config;
  Buffer.contents buffer
