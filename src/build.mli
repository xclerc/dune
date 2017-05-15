(** The build monad *)

(** The build monad is a monad that accumulate a list of dependencies
    and library dependencies (as written by the user) *)

open! Import

type 'a t

module Rule : sig
  type 'a build = 'a t
  type t =
    { build   : Action.t build
    ; targets : Path.Set.t
    ; sandbox : bool
    }

  val make : ?sandbox:bool -> targets:Path.t list -> Action.t build -> t
end with type 'a build := 'a t

type lib_dep_kind =
  | Optional
  | Required

type lib_deps = lib_dep_kind String_map.t

(**/**)

module Memoized_data : sig
  type 'a t =
    { data          : 'a
    ; deps          : Path.Set.t
    ; wait_for_deps : unit Future.t
    }
end

module Primitive : sig
  type 'a t =
    | Paths : Path.Set.t -> unit t
    | Glob : Path.t * Re.re -> Path.Set.t t
    | Record_lib_deps : Path.t * lib_deps -> unit t
    | File_exists : Path.t -> bool t
end

(**/**)

include Comp_intf.S
  with type 'a t := 'a t
  with type 'a primitive := 'a Primitive.t

(** Declare a dependency of the resulting action *)
val path : Path.t -> unit t
val paths : Path.t list -> unit t
val path_set : Path.Set.t -> unit t
val files_recursively_in : dir:Path.t -> file_tree:File_tree.t -> unit t

(** Evaluate a glob on a path *)
val eval_glob : dir:Path.t -> Re.re -> Path.Set.t t

(** Evaluates to [true] if the file is present on the file system or
    is the target of a rule. *)
val file_exists : Path.t -> bool t

val run
  :  context:Context.t
  -> ?dir:Path.t (* default: context.build_dir *)
  -> ?stdout_to:Path.t
  -> Path.t
  -> Arg_spec.t list
  ->  Action.t t

val copy : src:Path.t -> dst:Path.t -> Action.t t
val copy_and_add_line_directive : src:Path.t -> dst:Path.t -> Action.t t
val symlink : src:Path.t -> dst:Path.t -> Action.t t

val record_lib_deps
  :  dir:Path.t
  -> kind:lib_dep_kind
  -> Jbuild_types.Lib_dep.t list
  -> unit t

val record_lib_deps_simple : dir:Path.t -> lib_deps -> unit t
val merge_lib_deps : lib_deps -> lib_deps -> lib_deps
