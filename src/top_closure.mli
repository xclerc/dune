open Import


module type Elt = sig
  type t
  type graph
  type key
  val key : t -> key
  val deps : t -> graph -> t list
end

module Make(Key : Set.OrderedType)(Elt : Elt with type key := Key.t) : sig
  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure : Elt.graph -> Elt.t list -> (Elt.t list, Elt.t list) result
end

module type Elt_fut = sig
  type t
  type graph
  type key
  val key : t -> key
  val deps : t -> graph -> t list Future.t
end

module Make_fut(Key : Set.OrderedType)(Elt : Elt_fut with type key := Key.t) : sig
  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure : Elt.graph -> Elt.t list -> (Elt.t list, Elt.t list) result Future.t
end
