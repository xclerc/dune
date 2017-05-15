(** Common computation interface *)

open Import

module type Primitive = sig
  type 'a t
end

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val bind2 : 'a t -> 'b t -> f:('a -> 'b -> 'c t) -> 'c t
  val bind3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd t) -> 'd t

  val map  : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

  module O : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end

  val both : 'a t -> 'b t -> ('a * 'b) t
  val pack3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val all : 'a t list -> 'a list t

  module Read_result : sig
    (** Result of read computations. If not building, for instance
        evaluating the rules for [external-lib-deps], read actions
        return [Not_building] *)
    type 'a t =
      | Ok of 'a
      | Not_building
  end

  (** Reads the contents of a file *)
  val contents : Path.t -> string Read_result.t t
  val lines_of : Path.t -> string list Read_result.t t
  val read_sexp : Path.t -> 'a Sexp.Of_sexp.t -> 'a Read_result.t t

  (** Always fail when executed. We pass a function rather than an
      exception to get a proper backtrace.

      When not building, this doesn't fail and simply does nothing.
 *)
  val fail : fail -> unit t

  (** [memoize t ~name] is a computation that behaves like [t] except
      that its result is computed only once. *)
  val memoize : 'a t -> name:string -> 'a t

  type 'a primitive
  val prim : 'a primitive -> 'a t

  (**/**)

  module Repr : sig
    type 'a t =
      | Return : 'a -> 'a t
      | Bind  : 'a t * ('a -> 'b t) -> 'b t
      | Map   : 'a t * ('a -> 'b) -> 'b t
      | Both  : 'a t * 'b t -> ('a * 'b) t
      | Contents : Path.t -> string Read_result.t t
      | Lines_of : Path.t -> string list Read_result.t t
      | Fail : fail -> unit t
      | Memo : 'a memo -> 'a t
      | Prim : 'a primitive -> 'a t

    and 'a memo =
      { name          : string
      ; t             : 'a t
      ; mutable state : 'a memo_state
      }

    and 'a memo_state =
      | Unevaluated
      | Evaluating
      | Evaluated of 'a
  end

  val repr : 'a t -> 'a Repr.t
end
