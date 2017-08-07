type ('a, 'b) t

module Rule = Build_interpret.Rule

module Repr : sig
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Contents : Path.t -> ('a, string) t
    | Dyn_rules : ('a, Rule.t list) t -> ('a, 'a) t
    | Rules : Rule.t list -> ('a, 'a) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
end


module O : sig
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

val arr : ('a -> 'b) -> ('a, 'b) t

val repr : ('a, 'b) t -> ('a, 'b) Repr.t

val rule : Rule.t -> ('a, 'a) t

val rules : Rule.t list -> ('a, 'a) t

val contents : Path.t -> ('a, string) t

val dyn_rules : ('a, Rule.t list) t -> ('a, 'a) t
