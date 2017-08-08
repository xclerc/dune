module Rule = Build_interpret.Rule

module Repr = struct
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Load_sexps : Path.t -> ('a, Sexp.Ast.t list) t
    | Dyn_rules : (unit, Rule.t list) t -> ('a, 'a) t
    | Rules : Rule.t list -> ('a, 'a) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
end

include Repr

let repr t = t

let arr f = Arr f

let rule r = Rules [r]

let rules l = Rules l

let load_sexps p = Load_sexps p

let dyn_rules l = Dyn_rules l

let empty () = Rules []

module O = struct
  let ( >>> ) a b = Compose (a, b)
  let ( >>^ ) t f = t >>> arr f
end
