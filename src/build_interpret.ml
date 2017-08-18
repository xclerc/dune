open Import
open Build.Repr

module Pset = Path.Set
module Pmap = Path.Map
module Vspec = Build.Vspec

module Target = struct
  type t =
    | Normal of Path.t
    | Vfile : _ Vspec.t -> t

  let path = function
    | Normal p -> p
    | Vfile (Vspec.T (p, _)) -> p

  let paths ts =
    List.fold_left ts ~init:Pset.empty ~f:(fun acc t ->
      Pset.add (path t) acc)
end

module Static_deps = struct
  type t =
    { rule_deps   : Path.Set.t
    ; action_deps : Path.Set.t
    }
end

let eval_glob dir re ~all_targets_by_dir =
  match Pmap.find dir all_targets_by_dir with
  | None -> Pset.empty
  | Some targets ->
    Pset.filter targets ~f:(fun path ->
      Re.execp re (Path.basename path))

let eval_file_exists p ~all_targets_by_dir =
  let dir = Path.parent p in
  let targets =
    Option.value (Pmap.find dir all_targets_by_dir)
      ~default:Pset.empty
  in
  Pset.mem p targets

let static_deps t ~all_targets_by_dir =
  let rec loop : type a b. (a, b) t -> Static_deps.t -> Static_deps.t = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets _ -> acc
    | Store_vfile _ -> acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths fns -> { acc with action_deps = Pset.union fns acc.action_deps }
    | Paths_glob state -> begin
        match !state with
        | G_evaluated (_, ps) ->
          { acc with action_deps = Pset.union acc.action_deps ps }
        | G_unevaluated (dir, re) ->
          let result = eval_glob dir re ~all_targets_by_dir in
          state := G_evaluated ((dir, re), result);
          let action_deps = Pset.union result acc.action_deps in
          { acc with action_deps }
      end
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (_, t) -> loop t acc
        | Undecided (then_, else_) ->
          let exists = eval_file_exists p ~all_targets_by_dir in
          if exists then begin
            state := Decided (true, then_);
            loop then_ acc
          end else begin
            state := Decided (false, else_);
            loop else_ acc
          end
      end
    | Dyn_paths t -> loop t acc
    | Vpath (Vspec.T (p, _)) -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Contents p -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Lines_of p -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
  in
  loop (Build.repr t) { rule_deps = Pset.empty; action_deps = Pset.empty }

let dir_deps t =
  let rec loop : type a b. (a, b) t -> Path.t list -> Path.t list = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets _ -> acc
    | Store_vfile _ -> acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths _ -> acc
    | Paths_glob state -> begin
        match !state with
        | G_evaluated _ -> acc
        | G_unevaluated (dir, _) -> dir :: acc
      end
    | If_file_exists (p, _) -> (Path.parent p) :: acc
    | Dyn_paths t -> loop t acc
    | Vpath _ -> acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
  in
  loop (Build.repr t) []

let lib_deps =
  let rec loop : type a b. (a, b) t -> Build.lib_deps Pmap.t -> Build.lib_deps Pmap.t
    = fun t acc ->
      match t with
      | Arr _ -> acc
      | Targets _ -> acc
      | Store_vfile _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths _ -> acc
      | Vpath _ -> acc
      | Paths_glob _ -> acc
      | Dyn_paths t -> loop t acc
      | Contents _ -> acc
      | Lines_of _ -> acc
      | Record_lib_deps (dir, deps) ->
        let data =
          match Pmap.find dir acc with
          | None -> deps
          | Some others -> Build.merge_lib_deps deps others
        in
        Pmap.add acc ~key:dir ~data
      | Fail _ -> acc
      | If_file_exists (_, state) ->
        loop (get_if_file_exists_exn state) acc
      | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) Pmap.empty

let targets =
  let rec loop : type a b. (a, b) t -> Target.t list -> Target.t list = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets targets ->
      List.fold_left targets ~init:acc ~f:(fun acc fn -> Target.Normal fn :: acc)
    | Store_vfile spec -> Vfile spec :: acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths _ -> acc
    | Vpath _ -> acc
    | Paths_glob _ -> acc
    | Dyn_paths t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | If_file_exists (_, state) -> begin
        match !state with
        | Decided _ -> code_errorf "Build_interpret.targets got decided if_file_exists"
        | Undecided (a, b) ->
          match loop a [], loop b [] with
          | [], [] -> acc
          | _ ->
            code_errorf "Build_interpret.targets: cannot have targets \
                         under a [if_file_exists]"
      end
    | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) []

let check_dir_deps t ~all_targets_by_dir =
  let rec loop : type a b. (a, b) t -> unit = fun t ->
    match t with
    | Arr _ -> ()
    | Targets _ -> ()
    | Store_vfile _ -> ()
    | Compose (a, b) -> loop a; loop b
    | First t -> loop t
    | Second t -> loop t
    | Split (a, b) -> loop a; loop b
    | Fanout (a, b) -> loop a; loop b
    | Paths _ -> ()
    | Paths_glob state -> begin
        match !state with
        | G_evaluated ((dir, re), ps) ->
          let new_res = eval_glob dir re ~all_targets_by_dir in
          if Pset.compare new_res ps <> 0 then die "glob evaluation in dir %s changed unexpectedly"
                                                 (Path.to_string dir)
        | G_unevaluated _ -> die "unevaluated globs in check_dir_deps"
      end
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (s, t) ->
          let exists = eval_file_exists p ~all_targets_by_dir in
          if exists = s then loop t
          else die "file_exists evaluation changed unexpectedly"
        | Undecided _ -> die "unevaluated if_file_exists in check_dir_deps"
      end
    | Dyn_paths t -> loop t
    | Vpath (Vspec.T _) -> ()
    | Contents _ -> ()
    | Lines_of _ -> ()
    | Record_lib_deps _ -> ()
    | Fail _ -> ()
    | Memo m -> loop m.t
  in
  loop (Build.repr t)

module Rule = struct
  type t =
    { context  : Context.t option
    ; build    : (unit, Action.t) Build.t
    ; targets  : Target.t list
    ; sandbox  : bool
    ; fallback : Jbuild.Rule.Fallback.t
    ; loc      : Loc.t option
    }

  let make ?(sandbox=false) ?(fallback=Jbuild.Rule.Fallback.Not_possible)
        ?context ?loc build =
    { context
    ; build
    ; targets = targets build
    ; sandbox
    ; fallback
    ; loc
    }

  let target_dir_exn t =
    let target_dir =
      List.fold_left t.targets ~init:None ~f:(fun acc target ->
        let path = Target.path target in
        let dir = Path.parent path in
        match acc with
        | None -> Some dir
        | Some d ->
          if (Path.compare d dir) <> 0 then
            die "Rule has targets in different dirs: %s %s\n"
              (Path.to_string d) (Path.to_string dir)
          else
            Some dir)
    in
    Option.value_exn target_dir
end
