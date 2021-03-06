include module type of struct include Usexp end with module Loc := Usexp.Loc

module type Combinators = sig
  type 'a t
  val unit       : unit                      t

  val string     : string                    t
  (** Convert an [Atom] or a [Quoted_string] from/to a string. *)

  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t

  val string_set : String.Set.t            t
  (** [atom_set] is a conversion to/from a set of strings representing atoms. *)

  val string_map : 'a t -> 'a String.Map.t   t
  (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
     a conversion to/from a map where the keys are atoms and the
     values are of type ['a]. *)

  val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
  (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
end

module To_sexp : sig
  type sexp = t
  include Combinators with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp

  type field

  val field
    :  string
    -> 'a t
    -> ?equal:('a -> 'a -> bool)
    -> ?default:'a
    -> 'a
    -> field
  val field_o : string -> 'a t-> 'a option -> field

  val record_fields : field list t

  val unknown : _ t
end with type sexp := t

module Loc = Usexp.Loc

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | Template of Template.t
    | List of Loc.t * ast list

  type hint =
    { on: string
    ; candidates: string list
    }

  exception Of_sexp of Loc.t * string * hint option

  (** Monad producing a value of type ['a] by parsing an input
      composed of a sequence of S-expressions.

      The input can be seen either as a plain sequence of
      S-expressions or a list of fields. The ['kind] parameter
      indicates how the input is seen:

      - with {['kind = [values]]}, the input is seen as an ordered
      sequence of S-expressions

      - with {['kind = [fields]]}, the input is seen as an unordered
      sequence of fields

      A field is a S-expression of the form: [(<atom> <values>...)]
      where [atom] is a plain atom, i.e. not a quoted string and not
      containing variables. [values] is a sequence of zero, one or more
      S-expressions.

      It is possible to switch between the two mode at any time using
      the approriate combinator. Some primitives can be used in both
      mode while some are specific to one mode.  *)
  type ('a, 'kind) parser

  type values
  type fields

  type 'a t             = ('a, values) parser
  type 'a fields_parser = ('a, fields) parser

  (** [parse parser context sexp] parse a S-expression using the
      following parser. The input consist of a single
      S-expression. [context] allows to pass extra informations such as
      versions to individual parsers. *)
  val parse : 'a t -> Univ_map.t -> ast -> 'a

  val return : 'a -> ('a, _) parser
  val (>>=) : ('a, 'k) parser -> ('a -> ('b, 'k) parser) -> ('b, 'k) parser
  val (>>|) : ('a, 'k) parser -> ('a -> 'b) -> ('b, 'k) parser
  val (>>>) : (unit, 'k) parser -> ('a, 'k) parser -> ('a, 'k) parser
  val map : ('a, 'k) parser -> f:('a -> 'b) -> ('b, 'k) parser
  val try_ : ('a, 'k) parser -> (exn -> ('a, 'k) parser) -> ('a, 'k) parser

  (** Access to the context *)
  val get : 'a Univ_map.Key.t -> ('a option, _) parser
  val set : 'a Univ_map.Key.t -> 'a -> ('b, 'k) parser -> ('b, 'k) parser
  val get_all : (Univ_map.t, _) parser
  val set_many : Univ_map.t -> ('a, 'k) parser -> ('a, 'k) parser

  (** Return the location of the list currently being parsed. *)
  val loc : (Loc.t, _) parser

  (** End of sequence condition. Returns [true] iff they are no more
      S-expressions to parse *)
  val eos : (bool, _) parser

  (** What is currently being parsed. The second argument is the atom
      at the beginnig of the list when inside a [sum ...] or [field
      ...]. *)
  type kind =
    | Values of Loc.t * string option
    | Fields of Loc.t * string option
  val kind : (kind, _) parser

  (** [repeat t] use [t] to consume all remaning elements of the input
      until the end of sequence is reached. *)
  val repeat : 'a t -> 'a list t

  (** Capture the rest of the input for later parsing *)
  val capture : ('a t -> 'a) t

  (** [enter t] expect the next element of the input to be a list and
      parse its contents with [t]. *)
  val enter : 'a t -> 'a t

  (** [fields fp] converts the rest of the current input to a list of
      fields and parse them with [fp]. This operation fails if one the
      S-expression in the input is not of the form [(<atom>
      <values>...)] *)
  val fields : 'a fields_parser -> 'a t

  (** [record fp = enter (fields fp)] *)
  val record : 'a fields_parser -> 'a t

  (** Consume the next element of the input as a string, int, char, ... *)
  include Combinators with type 'a t := 'a t

  (** Unparsed next element of the input *)
  val raw : ast t

  (** Inspect the next element of the input without consuming it *)
  val peek : ast option t

  (** Same as [peek] but fail if the end of input is reached *)
  val peek_exn : ast t

  (** Consume and ignore the next element of the input *)
  val junk : unit t

  (** Ignore all the rest of the input *)
  val junk_everything : (unit, _) parser

  (** [plain_string f] expects the next element of the input to be a
      plain string, i.e. either an atom or a quoted string, but not a
      template nor a list. *)
  val plain_string : (loc:Loc.t -> string -> 'a) -> 'a t

  val fix : ('a t -> 'a t) -> 'a t

  val of_sexp_error
    :  ?hint:hint
    -> Loc.t
    -> string
    -> _
  val of_sexp_errorf
    :  ?hint:hint
    -> Loc.t
    -> ('a, unit, string, 'b) format4
    -> 'a

  val no_templates
    : ?hint:hint
    -> Loc.t
    -> ('a, unit, string, 'b) format4
    -> 'a

  val located : 'a t -> (Loc.t * 'a) t

  val enum : (string * 'a) list -> 'a t

  (** Parser that parse a S-expression of the form [(<atom> <s-exp1>
      <s-exp2> ...)] or [<atom>]. [<atom>] is looked up in the list and
      the remaining s-expressions are parsed using the corresponding
      list parser. *)
  val sum : (string * 'a t) list -> 'a t

  (** Check the result of a list parser, and raise a properly located
      error in case of failure. *)
  val map_validate
    :  'a fields_parser
    -> f:('a -> ('b, string) Result.t)
    -> 'b fields_parser

  (** {3 Parsing record fields} *)

  val field
    :  string
    -> ?default:'a
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> 'a t
    -> 'a fields_parser
  val field_o
    :  string
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> 'a t
    -> 'a option fields_parser

  val field_b
    :  ?check:(unit t)
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> string
    -> bool fields_parser

  (** A field that can appear multiple times *)
  val multi_field
    :  string
    -> 'a t
    -> 'a list fields_parser

  (** Default value for [on_dup]. It fails with an appropriate error
      message. *)
  val field_present_too_many_times : Univ_map.t -> string -> Ast.t list -> _
end

module type Sexpable = sig
  type t
  val t : t Of_sexp.t
  val sexp_of_t : t To_sexp.t
end
