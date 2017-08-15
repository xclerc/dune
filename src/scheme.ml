module Rule = Build_interpret.Rule

module Repr = struct
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Load_sexps : Path.t -> ('a, Sexp.Ast.t list) t
    | Dyn_rules : (unit, Rule.t list) t -> ('a, 'a) t
    | Rules : Rule.t list -> ('a, 'a) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | List_files : Path.t * (string -> bool) -> ('a, string list) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
end

include Repr


let repr t = t

let arr f = Arr f

module O = struct
  let ( >>>* ) a b = Compose (a, b)
  let ( >>^* ) t f = t >>>* arr f
  let ( &&&* ) a b = Fanout (a, b)
end

open O

let rule r = Rules [r]

let rules l = Rules l

let load_sexps p = Load_sexps p

let dyn_rule r =
  Dyn_rules (r >>^* fun r -> [r])

let dyn_rules l = Dyn_rules l

let empty () = Rules []

let list_files ~dir ~f = List_files (dir, f)

let rec all = function
  | [] -> arr (fun _ -> [])
  | t :: ts ->
    t &&&* all ts
    >>>*
    arr (fun (x, y) -> x :: y)
