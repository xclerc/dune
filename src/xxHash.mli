(** Fast hashes *)

type t = private int64

external file : string -> (t [@unboxed])
  = "jbuilder_digest_file_byte" "jbuilder_digest_file" [@@noalloc]

external string : string -> (t [@unboxed])
  = "jbuilder_digest_string_byte" "jbuilder_digest_string" [@@noalloc]

val to_hex : t -> string
val of_hex : string -> t
