(** {1 Simple gzip (de)compression} *)

type error = Invalid_content of string | Truncated of string

val pp_gzip_error : Format.formatter -> [ `Gzip of error] -> unit

val compress : ?level:int -> ?buffer_size:int -> string -> string
(** [compress src] returns a gzip-compressed version of [src].

    @param level can use used to set the compression level from [0] (no
    compression) to [3] (highest compression).

    @raise Invalid_argument if [level] is outside of the range 0 to 3. *)

val uncompress :
  ?max_size:int ->
  ?buffer_size:int ->
  string ->
  (string, [> `Gzip of error ]) result
(** [decompress src] decompresses the content from the gzip-compressed [src].

    @param max_size may be used to specify the maximum number of bytes to
    decompress.  Defaults to [Sys.max_string_length].  If [src] decompresses to
    more than [max_size] bytes then this function will return
    [Error (`Gzip (Truncated truncated_content))] containing the content which
    was decompressed.

    @return [Ok content] if the decompression was successful
    @return [Error err] if there was a problem during decompression *)

(*
module Z : sig
  (** {1 zlib compression} *)

  (** Possible error cases *)
  type error =
    | Truncated of string
        (** Extracted size is greater than the allowed maximum size *)
    | Compression_error of string  (** zlib error *)

  val compress : ?level:int -> ?header:bool -> string -> string
  (** [compress ?level ?header input] will return a zlib-compressed
      representation of [input]. *)

  val decompress :
    ?header:bool ->
    ?max_size:int ->
    string ->
    (string, [> `Zlib of error ]) result
  (** [decompress ?header ?max_size input] will return a decompressed
      representation of [input].

      @return [Error `Zlib Compression_error message] if {!Zlib.Error} was
      raised while inflating [input].
      @return [Error `Zlib Truncated incomplete] if [input] inflates to more
      than [max_size] bytes.  [incomplete] contains less than [max_size] bytes
      of inflated content from [input]. *)

  val pp_error : Format.formatter -> error -> unit

  val pp_zlib_error : Format.formatter -> [ `Zlib of error ] -> unit
end
*)
