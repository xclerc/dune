(** The jbuilder computation monad *)

open! Import

module Make(Prim : Comp_intf.Primitive)
  : Comp_intf.S with type 'a primitive := 'a Prim.t
