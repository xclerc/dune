type ('a, 'b) t

module Rule = Build_interpret.Rule

module Repr : sig
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Load_sexps : Path.t -> ('a, Sexp.Ast.t list) t
    | Dyn_rules : (unit, Rule.t list) t -> ('a, 'a) t
    | Rules : Rule.t list -> ('a, 'a) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | List_files : Path.t * (string -> bool) -> ('a, string list) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
end

module O : sig
  val ( >>>* ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( >>^* ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( &&&* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

val arr : ('a -> 'b) -> ('a, 'b) t

val repr : ('a, 'b) t -> ('a, 'b) Repr.t

val rule : Rule.t -> ('a, 'a) t

val rules : Rule.t list -> ('a, 'a) t

val load_sexps : Path.t -> ('a, Sexp.Ast.t list) t

val dyn_rule : (unit, Rule.t) t -> ('a, 'a) t

val dyn_rules : (unit, Rule.t list) t -> ('a, 'a) t

val empty : unit -> ('a, 'a) t

val list_files : dir:Path.t -> f:(string -> bool) -> ('a, string list) t

val all : ('a, 'b) t list -> ('a, 'b list) t
