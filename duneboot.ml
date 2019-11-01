(** {2 General configuration *)

let build_dir = "_boot"

let ignored_source_files = [ "dune"; ".merlin"; "setup.defaults.ml" ]

type task = { target: string * string
            ; external_libraries: string list
            ; local_libraries : (string * string option * bool * string list) list
            }

(* XXX This should be coming from boot/libs.ml *)
let tasks =
  [{ target = ("dune", "bin/main.ml")
   ; external_libraries = [ "unix"; "threads.posix" ]
   ; local_libraries =
       [ ("src/stdune/caml", Some "Dune_caml", false, [])
       ; ("src/stdune", Some "Stdune", false, [])
       ; ("src/dune_lang", Some "Dune_lang", false, [])
       ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false, [])
       ; ("src/dag", Some "Dag", false, [])
       ; ("src/fiber", Some "Fiber", false, [])
       ; ("src/memo", Some "Memo", false, [])
       ; ("src/xdg", Some "Xdg", false, [])
       ; ("src/dune_memory", Some "Dune_memory", false, [])
       ; ("src/dune_manager", Some "Dune_manager", false, [])
       ; ("vendor/re/src", Some "Dune_re", false, [])
       ; ("vendor/opam-file-format/src", None, false, [])
       ; ("otherlibs/dune-glob", Some "Dune_glob", false, [])
       ; ("src/ocaml-config", Some "Ocaml_config", false, [])
       ; ("src/catapult", Some "Catapult", false, [])
       ; ("src/jbuild_support", Some "Jbuild_support", false, [])
       ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", false, [])
       ; ("src/dune", Some "Dune", true, [])
       ; ("vendor/cmdliner/src", None, false, [])
       ; ("otherlibs/build-info/src", Some "Build_info", false, ["boot/build_info_data.ml"])
       ]
       };
   ]

(** {2 Utility functions *)

open StdLabels
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module List = struct
  include List

  let fold_left_map ~f ~init l =
    let rec aux accu l_accu = function
    | [] -> accu, rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l in
  aux init [] l

  let fold_left_filter_map ~f ~init l =
    let rec aux accu l_accu = function
    | [] -> accu, rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        let l_accu =
          Option.fold ~some:(fun x -> x::l_accu) ~none:l_accu x
        in
        aux accu l_accu l in
  aux init [] l
end

let ( ^/ ) = Filename.concat

let failwith fmt = Printf.ksprintf failwith fmt

(* Return list of entries in [path] as [path/entry] *)
let readdir path =
  Array.fold_right ~f:(fun entry dir -> (path ^/ entry)::dir) ~init:[] (Sys.readdir path)

(* Libraries in the vendor/ directory are assembled first, to avoid picking up
   stdune modules which override the stdlib *)
let is_vendor_dir directory = false (* vendor directories aren't being treated specially here
  let rec get dir =
    let next = Filename.dirname dir in
    if next = Filename.current_dir_name then
      dir
    else
      get next
  in
    get directory = "vendor" *)

let open_out file =
  if Sys.file_exists file then failwith "%s already exists" file;
  open_out file

(* copy a file - fails if the file exists *)
let copy ?write_header a b =
  if Sys.file_exists b then failwith "%s already exists" b;
  let ic = open_in_bin a in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  let oc = open_out_bin b in
  let () = Option.iter (fun f -> f oc a) write_header in
  let () =
    try
      output_string oc s
    with e ->
      close_out oc;
      Sys.remove b;
      raise e
  in
  close_out oc

(* Copy a file to a directory *)
let copy_to a ~dir =
  copy a (dir ^/ Filename.basename a)

let exec_or_die ?cwd pgm fmt =
  let restore_cwd = Sys.getcwd () in
  Option.iter Sys.chdir cwd;
  Printf.ksprintf
    (fun args ->
      let cmd = Filename.quote pgm ^ " " ^ args in
      print_endline cmd;
      let code =
        Sys.command
          ( if Sys.win32 then
            "\"" ^ cmd ^ "\""
          else
            cmd )
       in
         if cwd <> None then
           Sys.chdir restore_cwd;
         if code <> 0 then exit code)
    fmt

let mkdir_p dir =
  let rec recurse dir =
    let next = Filename.dirname dir in
    if dir = Filename.current_dir_name then
      ()
    else
      let () = recurse next in
      if Sys.file_exists dir then
        if Sys.is_directory dir then ()
        else
          failwith "%s should be a directory, not a file" dir
      else
        exec_or_die "mkdir" "%s" dir
  in
    recurse dir

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'

let split_path s =
  let rec loop i j =
    if j = String.length s then
      [ String.sub s ~pos:i ~len:(j - i) ]
    else if s.[j] = path_sep then
      String.sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> split_path s

let exe =
  if Sys.win32 then
    ".exe"
  else
    ""

let prog_not_found prog =
  Printf.eprintf "Program %s not found in PATH" prog;
  exit 2

let best_prog dir prog =
  let fn = dir ^/ prog ^ ".opt" ^ exe in
  if Sys.file_exists fn then
    Some fn
  else
    let fn = dir ^/ prog ^ exe in
    if Sys.file_exists fn then
      Some fn
    else
      None

let find_prog prog =
  let rec search = function
    | [] -> None
    | dir :: rest -> (
      match best_prog dir prog with
      | None -> search rest
      | Some fn -> Some (dir, fn) )
  in
  search path

let get_prog dir prog =
  match best_prog dir prog with
  | None -> prog_not_found prog
  | Some fn -> fn

let bin_dir, ocamlc =
  match find_prog "ocamlc" with
  | None -> prog_not_found "ocamlc"
  | Some x -> x

let ocamlyacc = get_prog bin_dir "ocamlyacc"

let ocamllex = get_prog bin_dir "ocamllex"

let ocamldep = get_prog bin_dir "ocamldep"

(* XXX get_prog or still allow fallback to ocamlc? *)
let ocamlopt = get_prog bin_dir "ocamlopt"

(* XXX This isn't correctly handling checks for files existing (or killing .mli for ocamlyacc?) *)
(* XXX This isn't using write_header yet, because we'll need to post-process those files (can't easily add lines to header of .mly or .mll) and also because of the previous problem altering the .mli is 'ard *)
let copy_generator ~write_header cmd fmt src dst =
  let subdir = Filename.dirname src in
  mkdir_p (build_dir ^/ subdir);
  copy_to src (build_dir ^/ subdir);
  let src_name = subdir ^/ Filename.basename src in
  exec_or_die ~cwd:build_dir cmd fmt src_name;
  Sys.remove (build_dir ^/ src_name);
  let output_name = Filename.remove_extension (Filename.basename dst) in
  let src_name = build_dir ^/ Filename.remove_extension src_name in
  if Sys.file_exists (src_name ^ ".mli") then begin
    copy ~write_header (src_name ^ ".mli") (build_dir ^/ output_name ^ ".mli");
    Sys.remove (src_name ^ ".mli")
  end;
  copy ~write_header (src_name ^ ".ml") (build_dir ^/ output_name ^ ".ml");
  Sys.remove (src_name ^ ".ml");
  Filename.remove_extension (Filename.basename src) ^ ".ml"

let copy_lexer = copy_generator ocamllex "-q %s"

let copy_parser = copy_generator ocamlyacc "%s"

(** {2 Bootstrap functions *)
let get_libraries tasks =
  let fold_task libraries {local_libraries} =
    let f ((x, _, _, _) as lib) libraries =
      if List.exists ~f:(fun (y, _, _, _) -> x = y) libraries then
        libraries
      else
        lib::libraries
    in
      List.fold_right ~f local_libraries ~init:libraries
  in
  List.fold_left ~f:fold_task ~init:[] tasks

let get_namespace_processing_functions directory namespace =
  (* If [namespace <> None] then we require a .ml for the namespace and we generate one if it
     doesn't. we don't permit mli-only namespaces (i.e. the .ml file would be generated too) *)
  match namespace with
  | Some namespace ->
      let namespace_file =
        let primary = String.uncapitalize_ascii namespace ^ ".ml" in
        if Sys.file_exists (directory ^/ primary) then
          String.uncapitalize_ascii namespace ^ "__.ml"
        else
          primary
      in
        let ch = open_out (build_dir ^/ namespace_file) in
        let get_target_module file =
          namespace ^ "__" ^ file
        in
        let add_to_namespace file =
          let name = String.capitalize_ascii (Filename.chop_extension file) in
          if name <> namespace then begin
            let target = String.capitalize_ascii (Filename.chop_extension (get_target_module file)) in
            Printf.fprintf ch "module %s = %s\n" name target;
            (* XXX Code dup. here isn't right! *)
            String.uncapitalize_ascii target ^ Filename.extension file
          end else
            file
        and close_namespace ml_files =
          let () = close_out ch in
          namespace_file :: ml_files
        and get_target file =
          (* XXX This is also a mess *)
          if String.capitalize_ascii (Filename.chop_extension (Filename.basename file)) = namespace then
            Filename.basename file
          else
            String.uncapitalize_ascii (get_target_module (Filename.basename file))
        and write_header ch file =
          (* XXX Yet more instances! *)
          (* XXX Add the line number back to the source file? *)
          Printf.fprintf ch "open! %s\n" (String.capitalize_ascii (Filename.chop_extension namespace_file));
          Printf.fprintf ch "# 1 \"%s\"\n" file
        in
          (add_to_namespace, close_namespace, get_target, write_header)
  | None ->
      (Fun.id, Fun.id, Filename.basename, (fun _ _ -> ()))

let not_ignored_file file =
  if not (List.mem (Filename.basename file) ignored_source_files) then
    let ext = Filename.extension file in
    String.length ext <> 4 || String.sub ext ~pos:0 ~len:3 <> ".sw"
  else
    false

let assemble_unit directory namespace scan_subdirs extra_files =
  let (add_to_namespace, close_namespace, get_target, write_header) = get_namespace_processing_functions directory namespace in
  (* Copy the files to the target directory, assembling lexers and parsers (and, if necessary, the
     namespace module) on the way. Return a list of files copied. *)
  let rec process_file units file =
    let ext = Filename.extension file in
    if Sys.is_directory file then
      if scan_subdirs then
        List.fold_left ~f:process_file ~init:units (readdir file)
      else
        units
    else if not_ignored_file file then
      match ext with
      | ".mll" ->
          let name = add_to_namespace (copy_lexer write_header file (get_target file)) in
          name::units
      | ".mly" ->
          let name = add_to_namespace (copy_parser write_header file (get_target file)) in
          (name ^ "i")::name::units
      | ".ml" ->
          copy ~write_header file (build_dir ^/ get_target file);
          (add_to_namespace (Filename.basename file))::units
      | ".c" ->
          copy_to file ~dir:build_dir;
          (Filename.basename file)::units
      | ".mli" ->
          let target = get_target file in
          copy ~write_header file (build_dir ^/ target);
          target::units
      | _ ->
          failwith "%s should not be in the tree" file
      else
        units
    in
      List.fold_left ~f:process_file ~init:[] (List.rev_append extra_files (readdir directory))
      |> close_namespace

let map_library (directory, namespace, scan_subdirs, extra_files) =
  let files = assemble_unit directory namespace scan_subdirs extra_files in
  let (c_files, ml_files) =
    List.partition ~f:(fun x -> Filename.extension x = ".c") files
  in
  let (namespace_files, ml_files) =
    (* XXX !!! *)
    List.partition ~f:(fun x -> Some (Filename.chop_extension x |> String.capitalize_ascii) = namespace ||
                                Some (Filename.chop_extension x |> String.capitalize_ascii) = Option.map (fun x -> x ^ "__") namespace) ml_files
  in
  (ml_files, namespace_files, c_files)

let read_ocamldep file =
  let ch = open_in file in
  let rec process () =
    match input_line ch with
    | line ->
        let colon = String.index line ':' in
        let filename = String.sub line 0 colon in
        let modules =
          if colon = String.length line - 1 then
            []
          else
            String.split_on_char ~sep:' ' (String.sub line (colon + 2) (String.length line - colon - 2))
        in
          (filename, modules) :: process ()
    | exception End_of_file ->
        close_in ch;
        []
  in
    process ()

let get_map_flags maps =
  if maps = [] then
    ""
  else
    " -map " ^ String.concat ~sep:" -map " maps

let get_map_dependencies existing_maps file =
  let existing_maps = get_map_flags existing_maps in
  exec_or_die ~cwd:build_dir ocamldep "-modules%s %s > %s.dep" existing_maps file file;
  exec_or_die ~cwd:build_dir ocamldep "-modules%s -as-map %s > %s.map.dep" existing_maps file file;
  let dependencies = read_ocamldep (build_dir ^/ file ^ ".dep") in
  let map_dependencies = read_ocamldep (build_dir ^/ file ^ ".map.dep") in
  (dependencies, map_dependencies)

let get_dependencies ~maps libraries =
  let all_source_files =
    List.map ~f:(fun (x, _, _) -> x) libraries
    |> List.concat
  in
  exec_or_die ~cwd:build_dir ocamldep "-modules%s %s > dependencies"
    (get_map_flags maps)
    (String.concat ~sep:" " all_source_files);
  let raw_dependencies = read_ocamldep (build_dir ^/ "dependencies") in
  let convert_dependencies (file, raw_dependencies, map_dependencies) =
    let is_mli = (Filename.extension file = ".mli") in
    let convert_module module_name =
      let filename = String.uncapitalize_ascii module_name in
      if filename = Filename.chop_extension file then
        (* Self-reference *)
        None
      else if Sys.file_exists (build_dir ^/ filename ^ ".mli") then
        if not is_mli && Sys.file_exists (build_dir ^/ filename ^ ".ml") && List.exists ~f:(fun x -> x = module_name) map_dependencies then
          (* .ml files can't be linked until the .ml file has been compiled *)
          Some [filename ^ ".mli"; filename ^ ".ml"]
        else
          (* .mli files never depend on .ml files *)
          Some [filename ^ ".mli"]
      else if Sys.file_exists (build_dir ^/ filename ^ ".ml") then
        (* If there's no .mli, then we must always depend on the .ml *)
        Some [filename ^ ".ml"]
      else
        (* This is a stdlib module *)
        None
    in
      let link_after = [] (* This seems to have been the product of a mistake!
        let aliased_modules =
          let f name = not (List.exists ~f:(fun x -> x = name) map_dependencies) in
          List.filter ~f raw_dependencies
        in
          List.map ~f:(fun module_name -> String.uncapitalize_ascii module_name ^ ".ml") aliased_modules*)
      in
      let raw_dependencies = List.concat (List.filter_map ~f:convert_module map_dependencies) in
      (* .ml depends on .mli, if it exists *)
      let raw_dependencies =
        if not is_mli && Sys.file_exists (build_dir ^/ file ^ "i") then
        (file ^ "i") :: raw_dependencies
      else
        raw_dependencies
    in
      (file, (raw_dependencies, link_after))
  in
    let unit_dependencies = List.map ~f:(fun (name, deps) -> convert_dependencies (name, deps, deps)) raw_dependencies in
    let map_dependencies = List.map ~f:convert_dependencies (List.concat (List.map ~f:(fun (_, x, _) -> x) libraries)) in
    List.rev_append map_dependencies unit_dependencies

let assemble_libraries existing_maps libraries =
  let libraries = List.map ~f:map_library libraries in
  let eliminate_non_alias_namespaces existing_maps (ml_files, namespace_files, c_files) =
    let f (ml_files, existing_maps) namespace =
      let (raw_dependencies, map_dependencies) as dependencies =
        get_map_dependencies existing_maps namespace
      in
        if raw_dependencies = map_dependencies then
          ((namespace::ml_files, existing_maps), None)
        else
          let dependencies =
            List.map2 ~f:(fun (name, raw_dependencies) (_, map_dependencies) -> (name, raw_dependencies, map_dependencies)) raw_dependencies map_dependencies
          in
            ((ml_files, (List.hd dependencies |> fun (x, _, _) -> x)::existing_maps), Some (List.hd dependencies))
    in
      let ((ml_files, _), namespaces) = List.fold_left_filter_map ~f ~init:(ml_files, existing_maps) namespace_files in
      let () = Printf.eprintf "Got namespaces: %s\n%!" (String.concat ~sep:" " (List.map ~f:(fun (x, _, _) -> x) namespaces)) in
      let existing_maps =
        match namespaces with
        | [] ->
            existing_maps
        | [(name, _, _)] ->
            existing_maps @ [name]
        | [(name1, _, _); (name2, _, _)] ->
            (* XXX Urgh! *)
            (*if Filename.extension name1 <> ".mli" && Filename.extension name2 <> ".mli" then*)
            if Filename.chop_extension name1 <> Filename.chop_extension name2 then
              existing_maps @ [name1; name2]
            else
            if Filename.extension name1 = ".mli" then
              existing_maps @ [name1]
            else
              existing_maps @ [name2]
        | _ ->
            (* XXX COMBAK it is now possible to have 3 *)
            failwith "Encountered more than 2 namespace files for a library?"
      in
        (existing_maps, (ml_files, namespaces, c_files))
  in
    List.fold_left_map ~f:eliminate_non_alias_namespaces ~init:existing_maps libraries

let get_target_file file =
  match Filename.extension file with
  | ".ml" -> Filename.chop_extension file ^ ".cmx"
  | ".mli" -> Filename.chop_extension file ^ ".cmi"
  | _ -> assert false

let topsort dependencies modules =
  let rec process cycle_count seen pending = function
  | filename::rest ->
      let () = if not (List.mem_assoc filename ~map:dependencies) then (Printf.eprintf "topsort Dependencies %s missing\n%!" filename; raise Exit) in
      let (deps, (link_after : string list)) = List.assoc filename dependencies in
      if List.for_all ~f:(fun x -> StringSet.mem x seen) deps && List.for_all ~f:(fun x -> not (List.exists ~f:(fun y -> x = y) pending) && not (List.exists ~f:(fun y -> x = y) rest)) link_after then
        filename :: process 0 (StringSet.add filename seen) pending rest
      else
        process cycle_count seen (filename::pending) rest
  | [] ->
      if pending = [] then
        []
      else if cycle_count > 1 then
        let () = Printf.eprintf "Pending items: %s\n%!" (String.concat ~sep:"\n               " (List.map ~f:(fun pending -> pending ^ " [" ^ String.concat ~sep:" " (List.filter ~f:(fun x -> not (StringSet.mem x seen)) (List.assoc pending dependencies |> fst)) ^ "]" ^ " <" ^ String.concat ~sep:" " (List.filter ~f:(fun x -> not (StringSet.mem x seen)) (List.assoc pending dependencies |> snd)) ^ ">") pending)) in
        failwith "topsort is cycling"
      else
        process (cycle_count + 1) seen [] (List.rev pending)
  in
    process 0 StringSet.empty [] modules

let get_compilation_order built dependencies ~for_linking modules =
  let filter_dependency file =
    for_linking && Filename.extension file = ".ml" || not (StringSet.mem file built)
  in
  (* Filter out built items from the dependency map *)
  let dependencies =
    let apply_built ((name, (dependencies, link_after)) as unit) =
      if not (StringSet.mem name built) then
        unit
      else
       (* XXX If for_linking = false, can't link_after safely become []? *)
       (name, (List.filter ~f:filter_dependency dependencies, link_after))
    in
    List.rev_map ~f:apply_built dependencies
  in
  let modules =
    (* Filter out any files already built (unless linking, when .ml must be kept) *)
    let modules =
      List.filter ~f:filter_dependency modules
    in
    (* Now expand the list to include everything which needs building *)
    let rec add_dependencies to_build filename =
      if StringSet.mem filename to_build then
        to_build
      else
        let () = if not (List.mem_assoc filename ~map:dependencies) then (Printf.eprintf "gco Dependencies %s missing\n%!" filename; raise Exit) in
        let deps = fst (List.assoc filename dependencies) in
        List.fold_left ~f:add_dependencies ~init:(StringSet.add filename to_build) deps
    in
      List.fold_left ~f:add_dependencies ~init:StringSet.empty modules
      |> StringSet.elements
  in
  topsort dependencies modules

let assemble_task maps main =
  match assemble_libraries maps [(Filename.dirname main, Some (String.capitalize_ascii (Filename.chop_extension (Filename.basename main))), false, [])] with
  | (maps, [(ml_files, namespaces, [])]) -> (maps, ml_files, namespaces)
  | _ -> assert false

let remove_all_trace ~cwd name =
  let rm_f file = if Sys.file_exists file then Sys.remove file in
  let file = cwd ^/ name in
  let base = Filename.chop_extension file in
  List.iter ~f:rm_f [file; base ^ ".cmi"; base ^ ".cmx"]

let get_releaseable_modules dependencies released modules =
  let f (released, can_build, to_build) unit =
    let (deps, _) = List.assoc unit dependencies in
    (*Printf.eprintf "%s has %d deps\n%!" unit (List.length deps);*)
    if List.for_all ~f:(fun x -> StringSet.mem x released) deps then
      ((*StringSet.add unit*) released, unit::can_build, to_build)
    else
      (released, can_build, unit::to_build)
  in
  List.fold_left ~f ~init:(released, [], []) modules
  (*let rec phase released to_build =
  let (released, can_build, to_build) = List.fold_left ~f ~init:(released, [], []) to_build in
  let (can_build, to_build) = (List.rev can_build, List.rev to_build) in
      (*Printf.eprintf "\n\n%!";
      Printf.eprintf "release: %s\n%!" (String.concat ~sep:" " can_build);
      Printf.eprintf "\n\n (%d held back)\n%!" (List.length to_build);*)
  if to_build <> [] then
    phase (List.fold_left ~f:(fun released x -> StringSet.add x released) ~init:released can_build) to_build
  else
  (released, can_build, to_build)
  in phase StringSet.empty modules*)

let process_task ~maps ~dependencies ~built ~c_taints last_files {target = (name, main); external_libraries; local_libraries} =
  List.iter ~f:(remove_all_trace ~cwd:build_dir) last_files;
  let (maps, ml_files, namespaces) = assemble_task maps main in
  let dependencies =
    let new_dependencies = get_dependencies ~maps [(ml_files, namespaces, [])] in
    List.rev_append new_dependencies dependencies
  in
    let build_modules =
      get_compilation_order built dependencies ~for_linking:true [Filename.basename main]
    in
    let c_objects =
      let c_objects =
        let f c_objects unit =
          List.fold_left ~f:(fun c_objects file -> StringSet.add file c_objects) ~init:c_objects (Option.value ~default:[] (StringMap.find_opt unit c_taints))
        in
          List.fold_left ~f ~init:StringSet.empty build_modules
      in
        (* XXX At present, the .c file is recompiled each time *)
        if StringSet.is_empty c_objects then
          ""
        else
          " " ^ String.concat ~sep:" " (StringSet.elements c_objects)
    in
    let build_modules =
      let f unit =
        if StringSet.mem unit built then
          if Filename.extension unit = ".mli" then
            None
          else
            Some (Filename.chop_extension unit ^ ".cmx")
        else
          Some unit
      in
        List.filter_map ~f build_modules
    in
    let external_libraries =
      let convert = function
      | "threads.posix" -> "-I +threads threads.cmxa"
      | "unix" -> "unix.cmxa"
      | s -> failwith "unhandled external library %s" s
      in
        let external_libraries = List.map ~f:convert external_libraries in
        if external_libraries = [] then
          ""
        else
          " " ^ String.concat ~sep:" " external_libraries
    in
    let parallel_count = 4
    in
      let rec loop released modules stack =
        (* See what's finished *)
        let ((running, released), stack) =
          let f (running, released) item =
            match item with
            | `Task (pid, releases) ->
                let (pid', status) = Unix.waitpid [WNOHANG] pid in
                if pid' = 0 then
                  ((succ running, released), Some item)
                else
                  if status <> (WEXITED 0) then
                    (* XXX COMBAK Display proper error *)
                    failwith "a command failed"
                  else
                    ((pred running, List.fold_left ~f:(fun released x -> StringSet.add x released) ~init:released releases), None)
            | `Module releases ->
                 if running < parallel_count then
                   let pid =
                     let pwd = Sys.getcwd () in
                     Sys.chdir build_dir;
                     Printf.printf "%s -I +threads -c -g -no-alias-deps -w -49 %s\n%!" ocamlopt (String.concat ~sep:" " releases);
                     let pid = Unix.create_process ocamlopt (Array.of_list (ocamlopt :: "-I" :: "+threads" :: "-c" :: "-g" :: "-no-alias-deps" :: "-w" :: "-49" :: releases)) Unix.stdin Unix.stdout Unix.stderr in Sys.chdir pwd; pid in
                   ((succ running, released), Some (`Task (pid, releases)))
                 else
                   ((running, released), Some item)
          in
            List.fold_left_filter_map ~f ~init:(0, released) stack
        in
        if running = parallel_count then Unix.sleepf 0.1;
        let (released, can_build, to_build) =
          if modules = [] then
            (released, [], [])
          else
            get_releaseable_modules dependencies released modules
        in
        if to_build = [] && stack = [] then
          released
        else
          let (mli, ml) = List.partition ~f:(fun x -> Filename.extension x = ".mli") can_build in
          let ml = List.map ~f:(fun x -> `Module [x]) ml in
          loop released to_build (stack @ (if mli = [] then ml else `Module mli :: ml))
      in
      (* XXX Obviously dreadful *)
      let released = loop StringSet.empty build_modules [] in
      let build_modules = get_compilation_order (StringSet.fold (fun x built -> StringSet.add x built) released built) dependencies ~for_linking:true [Filename.basename main] in
      exec_or_die ~cwd:build_dir ocamlopt "-o %s%s -g -no-alias-deps -w -49%s %s" name external_libraries c_objects (String.concat ~sep:" " (List.filter_map ~f:(fun x -> if StringSet.mem x released then if Filename.extension x = ".mli" then None else Some (get_target_file x) else Some x) build_modules));
      ml_files

let get_c_taints c_taints libraries =
  let f c_taints (ml_files, namespaces, c_files) =
    let ml_files =
      List.rev_append (List.rev_map ~f:(fun (x, _, _) -> x) namespaces) ml_files
    in
      List.fold_left ~f:(fun c_taints x -> StringMap.add x c_files c_taints) ~init:c_taints ml_files
  in
    List.fold_left ~f ~init:c_taints libraries

(** {2 Bootstrap process *)
let main () =
  let () = mkdir_p build_dir in
  (* XXX Clean out *.ml, *.mli, *.c and any other crap from the build directory *)
  (* Separate vendored from internal libraries *)
  let (vendored_libraries, internal_libraries) =
    get_libraries tasks
    |> List.partition ~f:(fun (dir, _, _, _) -> is_vendor_dir dir)
  in
  (* Get the module alias maps *)
  let (maps, vendored_libraries) = assemble_libraries [] vendored_libraries in
  let c_taints = get_c_taints StringMap.empty vendored_libraries in
  (* Compute dependencies and compile the vendored modules *)
  let dependencies = get_dependencies ~maps vendored_libraries in
  let vendored_modules =
    List.map ~f:(fun (x, (_, _)) -> x) dependencies
    |> get_compilation_order StringSet.empty dependencies ~for_linking:false
  in
  assert (vendored_modules = []);
  exec_or_die ~cwd:build_dir ocamlopt "-c -no-alias-deps -w -49 %s" (String.concat ~sep:" " vendored_modules);
  let (maps, internal_libraries) = assemble_libraries maps internal_libraries in
  let c_taints = get_c_taints c_taints internal_libraries in
  let dependencies =
    List.rev_append dependencies @@ get_dependencies ~maps internal_libraries
  in
  let built = StringSet.of_list vendored_modules in
  List.fold_left ~f:(process_task ~maps ~built ~dependencies ~c_taints) ~init:[] tasks |> ignore

let () = main ()
