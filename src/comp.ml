open Import

module Make(Prim : Comp_intf.Primitive) = struct
  module Read_result = struct
    type 'a t =
      | Ok of 'a
      | Not_building
  end

  module Repr = struct
    type 'a t =
      | Return : 'a -> 'a t
      | Bind  : 'a t * ('a -> 'b t) -> 'b t
      | Map   : 'a t * ('a -> 'b) -> 'b t
      | Both  : 'a t * 'b t -> ('a * 'b) t
      | Contents : Path.t -> string Read_result.t t
      | Lines_of : Path.t -> string list Read_result.t t
      | Fail : fail -> unit t
      | Memo : 'a memo -> 'a t
      | Prim : 'a Prim.t -> 'a t

    and 'a memo =
      { name          : string
      ; t             : 'a t
      ; mutable state : 'a memo_state
      }

    and 'a memo_state =
      | Unevaluated
      | Starting_evaluation
      | Evaluating of 'a Future.t
      | Evaluated  of 'a
  end
  include Repr
  let repr t = t

  let return x = Return x
  let bind t ~f = Bind (t, f)
  let map t ~f = Map (t, f)

  module O = struct
    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map  t ~f
  end
  open O

  let both a b = Both (a, b)
  let pack3 a b c = both a (both b c) >>| fun (a, (b, c)) -> (a, b, c)

  let bind2 a b   ~f = both a b          >>= fun (a, b     ) -> f a b
  let bind3 a b c ~f = both a (both b c) >>= fun (a, (b, c)) -> f a b c
  let map2  a b   ~f = both a b          >>| fun (a, b     ) -> f a b
  let map3  a b c ~f = both a (both b c) >>| fun (a, (b, c)) -> f a b c

  let rec all = function
    | [] -> return []
    | t :: ts ->
      both t (all ts) >>| fun (x, l) -> x :: l

  let contents p = Contents p
  let lines_of p = Lines_of p

  let fail fail = Fail fail

  let memoize t ~name =
    Memo { name; t; state = Unevaluated }

  let read_sexp path of_sexp =
    memoize ~name:(Path.to_string path)
      (contents path
       >>| function
       | Not_building -> Read_result.Not_building
       | Ok s ->
         let lb = Lexing.from_string s in
         lb.lex_curr_p <-
           { pos_fname = Path.to_string path
           ; pos_lnum  = 1
           ; pos_bol   = 0
           ; pos_cnum  = 0
           };
         Ok (of_sexp (Sexp_lexer.single lb)))

  let prim p = Prim p
end
