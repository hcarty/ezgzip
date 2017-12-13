type error =
  | Invalid_format
  | Compression_error of string
  | Size of {got: int; expected: int}
  | Checksum

val compress : ?level:int -> string -> string
(** [compress src] returns a gzip-compressed version of [src].

    @param level can use used to set the compression level from [0] (no
    compression) to [9] (highest compression).

    @raise Invalid_argument if [level] is outside of the range 0 to 9. *)

val decompress :
  ?ignore_size:bool -> ?ignore_checksum:bool -> string
  -> (string, [> `Gzip of error]) result
(** [decompress src] decompresses the content from the gzip-compressed [src].

    @param ignore_size may be set to [true] if you want to ignore the expected
    decompressed size information in the gzip footer.  Defaults to [false].
    @param ignore_checksum may be set to [true] if you want to ignore the
    expected decompressed data checksum in the gzip footer.  Defaults to
    [false].

    @return [Ok content] if the decompression was successful
    @return [Error err] if there was a problem during decompression *)

val pp_error : Format.formatter -> error -> unit

val pp_gzip_error : Format.formatter -> [`Gzip of error] -> unit
