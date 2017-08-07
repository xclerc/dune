module Rule = Build_interpret.Rule

module Repr = struct
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Contents : Path.t -> ('a, string) t
    | Dyn_rules : ('a, Rule.t list) t -> ('a, 'a) t
    | Rules : Rule.t list -> ('a, 'a) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
end

include Repr

let repr t = t

let arr f = Arr f

let rule r = Rules [r]

let rules l = Rules l

let contents p = Contents p

let dyn_rules l = Dyn_rules l

module O = struct
  let ( >>> ) a b = Compose (a, b)
  let ( >>^ ) t f = t >>> arr f
end
