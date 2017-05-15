open! Import

type t =
  | A        of string
  | As       of string list
  | S        of t list
  | Dep      of Path.t (** A path that is a dependency *)
  | Deps     of Path.t list
  | Dep_rel  of Path.t * string
  | Deps_rel of Path.t * string list
  | Path     of Path.t
  | Paths    of Path.t list

val deps   : t list -> Path.Set.t
val expand : dir:Path.t -> t list -> string list

