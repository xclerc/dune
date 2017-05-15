open Import

module Pset = Path.Set

type lib_dep_kind =
  | Optional
  | Required

type lib_deps = lib_dep_kind String_map.t

let merge_lib_dep_kind a b =
  match a, b with
  | Optional, Optional -> Optional
  | _ -> Required

let merge_lib_deps a b =
  String_map.merge a b ~f:(fun _ a b ->
    match a, b with
    | None, None -> None
    | x, None | None, x -> x
    | Some a, Some b -> Some (merge_lib_dep_kind a b))

module Primitive = struct
  type 'a t =
    | Paths : Path.Set.t -> unit t
    | Glob : Path.t * Re.re -> Path.Set.t t
    | Record_lib_deps : Path.t * lib_deps -> unit t
    | File_exists : Path.t -> bool t
end

include Comp.Make(Primitive)
open O

module Rule = struct
  type 'a build = 'a t
  type t =
    { build   : Action.t build
    ; targets : Path.Set.t
    ; sandbox : bool
    }

  let make ?(sandbox=false) ~targets build =
    { build
    ; targets = Path.Set.of_list targets
    ; sandbox
    }
end

let record_lib_deps_simple ~dir lib_deps =
  prim (Record_lib_deps (dir, lib_deps))

let record_lib_deps ~dir ~kind lib_deps =
  prim
    (Record_lib_deps
       (dir,
        List.concat_map lib_deps ~f:(function
          | Jbuild_types.Lib_dep.Direct s -> [(s, kind)]
          | Select { choices; _ } ->
            List.concat_map choices ~f:(fun c ->
              String_set.elements c.Jbuild_types.Lib_dep.required
              |> List.map ~f:(fun d -> (d, Optional))))
        |> String_map.of_alist_reduce ~f:merge_lib_dep_kind))

let path p = prim (Paths (Pset.singleton p))
let paths ps = prim (Paths (Pset.of_list ps))
let path_set ps = prim (Paths ps)

let eval_glob ~dir re = prim (Glob (dir, re))

let file_exists p = prim (File_exists p)

let files_recursively_in ~dir ~file_tree =
  let prefix_with, dir =
    match Path.extract_build_context_dir dir with
    | None -> (Path.root, dir)
    | Some (ctx_dir, src_dir) -> (ctx_dir, src_dir)
  in
  path_set (File_tree.files_recursively_in file_tree dir ~prefix_with)

let run ~context ?(dir=context.Context.build_dir) ?stdout_to
      prog args =
  path_set (Pset.add prog (Arg_spec.deps args))
  >>| fun () ->
  let args = Arg_spec.expand args ~dir in
  let action : Action.Mini_shexp.t = Run (prog, args) in
  let action =
    match stdout_to with
    | None      -> action
    | Some path -> Redirect (Stdout, path, action)
  in
  { Action.
    dir
  ; context = Some context
  ; action
  }

let copy ~src ~dst =
  path src >>| fun () ->
  Action.make_context_independant (Copy (src, dst))

let copy_and_add_line_directive ~src ~dst =
  path src >>| fun () ->
  Action.make_context_independant (Copy_and_add_line_directive (src, dst))

let symlink ~src ~dst =
  path src >>| fun () ->
  Action.make_context_independant (Symlink (src, dst))

(*
   {[
     let progn ts =
       all ts >>^ fun (actions : Action.t list) ->
       match actions with
       | [] ->
         { Action.
           context = None
         ; dir     = Path.root
         ; action  = Progn []
         }
       | first :: rest ->
         let rest =
           List.map rest ~f:(fun a ->
             (match first.context, a.context with
              | None, None -> ()
              | Some c1, Some c2 when c1.name = c2.name -> ()
              | _ ->
                Sexp.code_error "Build.progn"
                  [ "actions", Sexp.To_sexp.list Action.sexp_of_t actions ]);
             if first.dir = a.dir then
               a.action
             else
               Chdir (a.dir, a.action))
         in
         { first with action = Progn (first :: rest) }
   ]}
*)
