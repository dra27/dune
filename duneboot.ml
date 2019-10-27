(** {2 General configuration *)

let build_dir = "_boot"

let ignored_source_files = [ "dune"; ".merlin"; "setup.defaults.ml" ]

type task = { target: string * string
            ; external_libraries: string list
            ; local_libraries : (string * string option * bool * string list) list
            }

(* XXX This should be coming from boot/libs.ml *)
let task = { target = ("dune", "bin/main.ml")
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
               ; ("src/dune_cache", Some "Dune_cache", false, [])
               ; ("src/dune_cache_daemon", Some "Dune_cache_daemon", false, [])
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
           }

(** {2 Utility functions *)

open StdLabels
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let ( ^/ ) = Filename.concat

let failwith fmt = Printf.ksprintf failwith fmt

(* Return list of entries in [path] as [path/entry] *)
let readdir path =
  Array.fold_right ~f:(fun entry dir -> (path ^/ entry)::dir) ~init:[] (Sys.readdir path)

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

let copy_generator ?(writes_mli = false) ~write_header cmd fmt src dst =
  let subdir = Filename.dirname src in
  let target_dir = build_dir ^/ subdir in
  let src_file = Filename.basename src in
  let mod_name = target_dir ^/ Filename.remove_extension src_file in
  let output_name = build_dir ^/ Filename.remove_extension (Filename.basename dst) in
  (* These files must be processed in an equivalent directory, or the # comments
     in them will be wrong *)
  mkdir_p target_dir;
  copy_to src target_dir;
  exec_or_die ~cwd:build_dir cmd fmt src;
  (* Copy of the source no longer required *)
  Sys.remove (build_dir ^/ src);
  (* Process the generated .ml (and .mli) into build_dir *)
  if writes_mli then begin
    copy ~write_header (mod_name ^ ".mli") (output_name ^ ".mli");
    Sys.remove (mod_name ^ ".mli")
  end;
  copy ~write_header (mod_name ^ ".ml") (output_name ^ ".ml");
  Sys.remove (mod_name ^ ".ml");
  Filename.remove_extension src_file ^ ".ml"

let copy_lexer = copy_generator ocamllex "-q %s"

let copy_parser = copy_generator ~writes_mli:true ocamlyacc "%s"

let timings = Queue.create ()

let mark_overhead () = Queue.push (Unix.gettimeofday (), None) timings

let mark_timing fmt = Format.ksprintf (fun msg -> Queue.push (Unix.gettimeofday (), Some msg) timings) fmt 

(** {2 Bootstrap functions *)

let get_namespace_processing_functions directory namespace =
  (* If [namespace <> None] then we require a .ml for the namespace and we generate one if it
     doesn't. we don't permit mli-only namespaces (i.e. the .ml file would be generated too) *)
  match namespace with
  | Some namespace ->
      let (namespace, namespace_module, generated_file) =
        (* Determine the name of the generated file (Namespace or Namespace__) *)
        let namespace_file = String.uncapitalize_ascii namespace in
        let primary = namespace_file ^ ".ml" in
        if Sys.file_exists (directory ^/ primary) then
          (namespace_file, namespace ^ "__", namespace_file ^ "__.ml")
        else
          (namespace_file, namespace, primary)
      in
        let ch = open_out (build_dir ^/ generated_file) in
        let get_target_module file =
          namespace ^ "__" ^ file
        in
        let added = ref false in
        let add_to_namespace file =
          added := true;
          let name = Filename.chop_extension file in
          if name <> namespace then begin
            let target = get_target_module file in
            Printf.fprintf ch "module %s = %s\n" (String.capitalize_ascii name) (String.capitalize_ascii (Filename.chop_extension target));
            target
          end else
            file
        and close_namespace ml_files =
          let () = close_out ch in
          if !added then
            generated_file :: ml_files
          else begin
            Sys.remove (build_dir ^/ generated_file);
            ml_files
          end
        and get_target file =
          let file = Filename.basename file in
          if Filename.chop_extension file = namespace then
            file
          else
            get_target_module file
        and write_header ch file =
          Printf.fprintf ch "open! %s\n" namespace_module;
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
    match namespace with
    | None -> ([], ml_files)
    | Some namespace ->
        let generated_namespace = namespace ^ "__" in
        let is_namespace_file file =
          let module_name = String.capitalize_ascii (Filename.chop_extension file) in
          module_name = namespace || module_name = generated_namespace
        in
          List.partition ~f:is_namespace_file ml_files
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

let convert_dependencies (file, dependencies) =
  let is_mli = (Filename.extension file = ".mli") in
  let convert_module module_name =
    let filename = String.uncapitalize_ascii module_name in
    if filename = Filename.chop_extension file then
      (* Self-reference *)
      None
    else if Sys.file_exists (build_dir ^/ filename ^ ".mli") then
      if not is_mli && Sys.file_exists (build_dir ^/ filename ^ ".ml") && List.exists ~f:(fun x -> x = module_name) dependencies then
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
    let dependencies = List.concat (List.filter_map ~f:convert_module dependencies) in
    (* .ml depends on .mli, if it exists *)
    let dependencies =
      if not is_mli && Sys.file_exists (build_dir ^/ file ^ "i") then
        (file ^ "i") :: dependencies
      else
        dependencies
    in
      (file, dependencies)

let get_dependencies ~maps libraries =
  let all_source_files =
    List.map ~f:(fun (x, _, _) -> x) libraries
    |> List.concat
  in
  let all_map_deps =
    List.map ~f:(fun (_, x, _) -> x) libraries
    |> List.concat
  in
  mark_overhead ();
  exec_or_die ~cwd:build_dir ocamldep "-modules%s %s > dependencies"
    (get_map_flags maps)
    (String.concat ~sep:" " all_source_files);
  mark_timing "Compute dependencies (ocamldep)";
  let dependencies = read_ocamldep (build_dir ^/ "dependencies") in
  List.rev_append (List.map ~f:convert_dependencies all_map_deps)
                  (List.map ~f:convert_dependencies dependencies)

let assemble_libraries {local_libraries; target = (_, main)} =
  let libraries = local_libraries @ [(Filename.dirname main, Some (String.capitalize_ascii (Filename.chop_extension (Filename.basename main))), false, [])] in
  let libraries = List.map ~f:map_library libraries in
  let all_namespaces =
    let f (_, namespaces, _) = namespaces in
    List.map ~f libraries
    |> List.concat
  in
  mark_overhead ();
  exec_or_die ~cwd:build_dir ocamldep "-modules %s > namespaces.dep" (String.concat ~sep:" " all_namespaces);
  exec_or_die ~cwd:build_dir ocamldep "-modules -as-map %s > rawmaps.dep" (String.concat ~sep:" -as-map " all_namespaces);
  mark_timing "Scan maps (ocamldep)";
  let raw_dependencies = read_ocamldep (build_dir ^/ "namespaces.dep") |> List.sort ~cmp:compare in
  let map_dependencies = read_ocamldep (build_dir ^/ "rawmaps.dep") |> List.sort ~cmp:compare in
  let maps =
    let f (raw_file, raw_dependencies) (map_file, map_dependencies) maps =
      assert (raw_file = map_file);
      if raw_dependencies <> map_dependencies then
        StringMap.add raw_file (raw_file, map_dependencies) maps
      else
        maps
    in
      List.fold_right2 ~f raw_dependencies map_dependencies ~init:StringMap.empty
  in
  let map_namespaces (ml_files, namespaces, c_files) =
    let (namespaces, not_namespaces) =
      List.partition ~f:(fun x -> StringMap.mem x maps) namespaces
    in
      (not_namespaces @ ml_files, namespaces, c_files)
  in
  let libraries = List.map ~f:map_namespaces libraries in
  let map_modules =
    List.map ~f:(fun (_, x, _) -> x) libraries
    |> List.concat
  in
  let maps =
    mark_overhead ();
    exec_or_die ~cwd:build_dir ocamldep "-modules -map %s -as-map %s > maps.dep" (String.concat " -map " map_modules) (String.concat ~sep:" -as-map " map_modules);
    mark_timing "Compute map dependencies (ocamldep)";
    read_ocamldep (build_dir ^/ "maps.dep")
  in
  let apply_dependencies (ml_files, namespaces, c_files) =
    let namespaces =
      let f namespace =
        List.find_opt ~f:(fun (x, _) -> x = namespace) maps
      in
        List.filter_map ~f namespaces
    in
      (ml_files, namespaces, c_files)
  in
    (map_modules, List.map ~f:apply_dependencies libraries)

let topsort dependencies modules =
  let rec process cycle_count seen pending = function
  | filename::rest ->
      let deps = StringMap.find filename dependencies in
      if List.for_all ~f:(fun x -> StringSet.mem x seen) deps then
        filename :: process 0 (StringSet.add filename seen) pending rest
      else
        process cycle_count seen (filename::pending) rest
  | [] ->
      if pending = [] then
        []
      else if cycle_count > 1 then
        let () = Printf.eprintf "Pending items: %s\n%!" (String.concat ~sep:"\n               " (List.map ~f:(fun pending -> pending ^ " [" ^ String.concat ~sep:" " (List.filter ~f:(fun x -> not (StringSet.mem x seen)) (StringMap.find pending dependencies)) ^ "]") pending)) in
        failwith "topsort is cycling"
      else
        process (cycle_count + 1) seen [] (List.rev pending)
  in
    process 0 StringSet.empty [] modules

let get_compilation_order dependencies modules =
  let dependencies = List.fold_left ~f:(fun map (key, value) -> StringMap.add key value map) ~init:StringMap.empty dependencies in
  let modules =
    (* Expand the list to include everything which needs building *)
    let rec add_dependencies to_build filename =
      if StringSet.mem filename to_build then
        to_build
      else
        let deps = StringMap.find filename dependencies in
        List.fold_left ~f:add_dependencies ~init:(StringSet.add filename to_build) deps
    in
      List.fold_left ~f:add_dependencies ~init:StringSet.empty modules
      |> StringSet.elements
  in
  mark_overhead ();
  let r = topsort dependencies modules in
  mark_timing "topsort"; r

let process_task ~start ~maps ~dependencies ~c_taints {target = (name, main); external_libraries; local_libraries} =
  let build_modules =
    get_compilation_order dependencies [Filename.basename main]
  in
  let c_objects =
    let c_objects =
      let f c_objects unit =
        List.fold_left ~f:(fun c_objects file -> StringSet.add file c_objects) ~init:c_objects (Option.value ~default:[] (StringMap.find_opt unit c_taints))
      in
        List.fold_left ~f ~init:StringSet.empty build_modules
    in
      if StringSet.is_empty c_objects then
        ""
      else
        " " ^ String.concat ~sep:" " (StringSet.elements c_objects)
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
    mark_overhead ();
    exec_or_die ~cwd:build_dir ocamlopt "-o %s%s%s -g -no-alias-deps -w -49%s %s" name exe external_libraries c_objects (String.concat ~sep:" " build_modules);
    mark_timing "Compilation"

let get_c_taints c_taints libraries =
  let f c_taints (ml_files, namespaces, c_files) =
    let f c_taints file = StringMap.add file c_files c_taints in
    let c_taints = List.fold_left ~f ~init:c_taints ml_files in
    List.fold_left ~f ~init:c_taints (List.map ~f:fst namespaces)
  in
    List.fold_left ~f ~init:c_taints libraries

(** {2 Bootstrap process *)
let main () =
  let start = Unix.gettimeofday () in
  let () = mkdir_p build_dir in
  (* XXX Clean out *.ml, *.mli, *.c and any other crap from the build directory *)
  let (maps, libraries) = assemble_libraries task in
  let c_taints = get_c_taints StringMap.empty libraries in
  let dependencies = get_dependencies ~maps libraries in
  process_task ~start ~maps ~dependencies ~c_taints task;
  Queue.fold (fun last (time, message) -> Printf.eprintf "%s: %f\n%!" (Option.value ~default:"Overhead" message) (time -. last); time) start timings |> ignore

let () = main ()
