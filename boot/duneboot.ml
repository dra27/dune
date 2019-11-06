(** {2 General configuration *)

let build_dir = "_boot"

let ignored_source_files = [ "dune"; ".merlin"; "setup.defaults.ml" ]

type task =
  { target : string * string
  ; external_libraries : string list
  ; local_libraries : (string * string option * bool * string option) list
  }

let task =
  { target = ("dune", "bin/main.ml")
  ; external_libraries = Libs.external_libraries
  ; local_libraries = Libs.local_libraries
  }

(** {2 Utility functions *)

open StdLabels
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let ( ^/ ) = Filename.concat

let failwith fmt = Printf.ksprintf failwith fmt

(* Return list of entries in [path] as [path/entry] *)
let readdir path =
  Array.fold_right
    ~f:(fun entry dir -> (path ^/ entry) :: dir)
    ~init:[] (Sys.readdir path)

let open_out file =
  if Sys.file_exists file then failwith "%s already exists" file;
  open_out file

let input_lines ic =
  let rec loop ic acc =
    match input_line ic with
    | line -> loop ic (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop ic []

let read_lines fn =
  let ic = open_in fn in
  let lines = input_lines ic in
  close_in ic;
  lines

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
    try output_string oc s
    with e ->
      close_out oc;
      Sys.remove b;
      raise e
  in
  close_out oc

(* Copy a file to a directory *)
let copy_to a ~dir = copy a (dir ^/ Filename.basename a)

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
      if cwd <> None then Sys.chdir restore_cwd;
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
        if Sys.is_directory dir then
          ()
        else
          failwith "%s should be a directory, not a file" dir
      else
        Unix.mkdir dir 0o777
  in
  recurse dir

let rec rm_rf fn =
  match Unix.lstat fn with
  | { st_kind = S_DIR; _ } ->
    List.iter (readdir fn) ~f:rm_rf;
    Unix.rmdir fn
  | _ -> Unix.unlink fn
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

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

let fatal fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 2)
    fmt

let prog_not_found prog = fatal "Program %s not found in PATH" prog

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

let use_secondary = Array.length Sys.argv > 1 && Sys.argv.(1) = "secondary"

let bin_dir, ocamlc =
  match find_prog "ocamlc" with
  | None -> prog_not_found "ocamlc"
  | Some x -> x

let bin_dir, ocamlc =
  if use_secondary then
    let () =
      at_exit (fun () -> try Sys.remove "boot-secondary" with _ -> ())
    in
    let code =
      Sys.command "ocamlfind -toolchain secondary query ocaml > boot-secondary"
    in
    if code = 0 then
      match read_lines "boot-secondary" with
      | []
      | _ :: _ :: _ ->
        fatal "Unexpected output locating secondary compiler"
      | [ bin_dir ] -> (
        match best_prog bin_dir "ocamlc" with
        | None -> fatal "Failed to locate secondary ocamlc"
        | Some x -> (bin_dir, x) )
    else
      fatal "Unexpected exit code %d from ocamlfind" code
  else
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
  let output_name =
    build_dir ^/ Filename.remove_extension (Filename.basename dst)
  in
  (* These files must be processed in an equivalent directory, or the #
     comments in them will be wrong *)
  mkdir_p target_dir;
  copy_to src target_dir;
  exec_or_die ~cwd:build_dir cmd fmt src;
  (* Copy of the source no longer required *)
  Sys.remove (build_dir ^/ src);
  (* Process the generated .ml (and .mli) into build_dir *)
  if writes_mli then (
    copy ~write_header (mod_name ^ ".mli") (output_name ^ ".mli");
    Sys.remove (mod_name ^ ".mli")
  );
  copy ~write_header (mod_name ^ ".ml") (output_name ^ ".ml");
  Sys.remove (mod_name ^ ".ml");
  Filename.remove_extension src_file ^ ".ml"

let copy_lexer = copy_generator ocamllex "-q %s"

let copy_parser = copy_generator ~writes_mli:true ocamlyacc "%s"

let timings = Queue.create ()

let mark_overhead () = Queue.push (Unix.gettimeofday (), None) timings

let mark_timing fmt =
  Format.ksprintf
    (fun msg -> Queue.push (Unix.gettimeofday (), Some msg) timings)
    fmt

(** {2 Bootstrap functions *)

let get_namespace_processing_functions directory namespace =
  (* If [namespace <> None] then we require a .ml for the namespace and we
     generate one if it doesn't. we don't permit mli-only namespaces (i.e. the
     .ml file would be generated too) *)
  match namespace with
  | Some namespace ->
    let namespace, namespace_module, generated_file =
      (* Determine the name of the generated file (Namespace or Namespace__) *)
      let namespace_file = String.uncapitalize_ascii namespace in
      let primary = namespace_file ^ ".ml" in
      if Sys.file_exists (directory ^/ primary) then
        (namespace_file, namespace ^ "__", namespace_file ^ "__.ml")
      else
        (namespace_file, namespace, primary)
    in
    let ch = open_out (build_dir ^/ generated_file) in
    let get_target_module file = namespace ^ "__" ^ file in
    let added = ref false in
    let add_to_namespace file =
      added := true;
      let name = Filename.chop_extension file in
      if name <> namespace then (
        let target = get_target_module file in
        let name = String.capitalize_ascii name in
        let real_name =
          String.capitalize_ascii (Filename.chop_extension target)
        in
        Printf.fprintf ch "module %s = %s\n" name real_name;
        target
      ) else
        file
    and close_namespace ml_files =
      let () = close_out ch in
      if !added then
        generated_file :: ml_files
      else (
        Sys.remove (build_dir ^/ generated_file);
        ml_files
      )
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
  | None -> (Fun.id, Fun.id, Filename.basename, fun _ _ -> ())

let not_ignored_file file =
  if not (List.mem (Filename.basename file) ignored_source_files) then
    let ext = Filename.extension file in
    String.length ext <> 4 || String.sub ext ~pos:0 ~len:3 <> ".sw"
  else
    false

let get_version () =
  let from_dune_project =
    match read_lines "dune-project" with
    | exception _ -> None
    | lines ->
      let rec loop = function
        | [] -> None
        | line :: lines -> (
          match Scanf.sscanf line "(version %s)" (fun v -> v) with
          | exception _ -> loop lines
          | v -> Some v )
      in
      loop lines
  in
  match from_dune_project with
  | Some _ -> from_dune_project
  | None -> (
    let ic = Unix.open_process_in "git describe --always --dirty" in
    let lines = input_lines ic in
    match (Unix.close_process_in ic, lines) with
    | WEXITED 0, [ v ] -> Some v
    | _ -> None )

let print_build_info oc =
  let pr fmt = Printf.fprintf oc fmt in
  let prlist name l ~f =
    match l with
    | [] -> pr "let %s = []\n" name
    | x :: l ->
      pr "let %s =\n" name;
      pr "  [ ";
      f x;
      List.iter l ~f:(fun x ->
          pr "  ; ";
          f x);
      pr "  ]\n"
  in
  pr "let version = %s\n"
    ( match get_version () with
    | None -> "None"
    | Some v -> Printf.sprintf "Some %S" v );
  pr "\n";
  let libs =
    List.map task.local_libraries ~f:(fun (name, _, _, _) -> (name, "version"))
    @ List.map task.external_libraries ~f:(fun name ->
          (name, {|Some "[distributed with Ocaml]"|}))
    |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
  in
  prlist "statically_linked_libraries" libs ~f:(fun (name, v) ->
      pr "%S, %s\n" name v)

let assemble_unit directory namespace scan_subdirs build_info_module =
  let add_to_namespace, close_namespace, get_target, write_header =
    get_namespace_processing_functions directory namespace
  in
  (* Copy the files to the target directory, assembling lexers and parsers
     (and, if necessary, the namespace module) on the way. Return a list of
     files copied. *)
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
        let name =
          add_to_namespace (copy_lexer write_header file (get_target file))
        in
        name :: units
      | ".mly" ->
        let name =
          add_to_namespace (copy_parser write_header file (get_target file))
        in
        (name ^ "i") :: name :: units
      | ".ml" ->
        copy ~write_header file (build_dir ^/ get_target file);
        add_to_namespace (Filename.basename file) :: units
      | ".c" ->
        copy_to file ~dir:build_dir;
        Filename.basename file :: units
      | ".mli" ->
        let target = get_target file in
        copy ~write_header file (build_dir ^/ target);
        target :: units
      | _ -> failwith "%s should not be in the tree" file
    else
      units
  in
  let init =
    match build_info_module with
    | None -> []
    | Some m ->
      let file = directory ^/ m ^ ".ml" in
      let oc = open_out (build_dir ^/ get_target file) in
      print_build_info oc;
      close_out oc;
      [ add_to_namespace (Filename.basename file) ]
  in
  List.fold_left ~f:process_file ~init (readdir directory) |> close_namespace

let map_library (directory, namespace, scan_subdirs, extra_files) =
  let files = assemble_unit directory namespace scan_subdirs extra_files in
  let c_files, ml_files =
    List.partition ~f:(fun x -> Filename.extension x = ".c") files
  in
  let namespace_files, ml_files =
    match namespace with
    | None -> ([], ml_files)
    | Some namespace ->
      let generated_namespace = namespace ^ "__" in
      let is_namespace_file file =
        let module_name =
          String.capitalize_ascii (Filename.chop_extension file)
        in
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
          let modules =
            String.sub line (colon + 2) (String.length line - colon - 2)
          in
          String.split_on_char ~sep:' ' modules
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
  let is_mli = Filename.extension file = ".mli" in
  let convert_module module_name =
    let filename = String.uncapitalize_ascii module_name in
    if filename = Filename.chop_extension file then
      (* Self-reference *)
      None
    else if Sys.file_exists (build_dir ^/ filename ^ ".mli") then
      if
        (not is_mli)
        && Sys.file_exists (build_dir ^/ filename ^ ".ml")
        && List.exists ~f:(fun x -> x = module_name) dependencies
      then
        (* .ml files can't be linked until the .ml file has been compiled *)
        Some [ filename ^ ".mli"; filename ^ ".ml" ]
      else
        (* .mli files never depend on .ml files *)
        Some [ filename ^ ".mli" ]
    else if Sys.file_exists (build_dir ^/ filename ^ ".ml") then
      (* If there's no .mli, then we must always depend on the .ml *)
      Some [ filename ^ ".ml" ]
    else
      (* This is a stdlib module *)
      None
  in
  let dependencies =
    List.concat (List.filter_map ~f:convert_module dependencies)
  in
  (* .ml depends on .mli, if it exists *)
  let dependencies =
    if (not is_mli) && Sys.file_exists (build_dir ^/ file ^ "i") then
      (file ^ "i") :: dependencies
    else
      dependencies
  in
  (file, dependencies)

let write_args file args =
  let ch = open_out (build_dir ^/ file) in
  output_string ch (String.concat ~sep:"\n" args);
  close_out ch

let get_dependencies ~maps libraries =
  let all_source_files =
    List.map ~f:(fun (x, _, _) -> x) libraries |> List.concat
  in
  let all_map_deps =
    List.map ~f:(fun (_, x, _) -> x) libraries |> List.concat
  in
  mark_overhead ();
  write_args "args_list" all_source_files;
  exec_or_die ~cwd:build_dir ocamldep
    "-modules%s -args args_list > dependencies" (get_map_flags maps);
  mark_timing "Compute dependencies (ocamldep)";
  let dependencies = read_ocamldep (build_dir ^/ "dependencies") in
  List.rev_append
    (List.map ~f:convert_dependencies all_map_deps)
    (List.map ~f:convert_dependencies dependencies)

let assemble_libraries { local_libraries; target = _, main } =
  let libraries =
    (* In order to assemble all the sources in one place, the executables
       modules are also put in a namespace *)
    let task_lib =
      let dir = Filename.dirname main in
      let namespace =
        String.capitalize_ascii
          (Filename.chop_extension (Filename.basename main))
      in
      (dir, Some namespace, false, None)
    in
    local_libraries @ [ task_lib ]
  in
  let libraries = List.map ~f:map_library libraries in
  let all_namespaces =
    let f (_, namespaces, _) = namespaces in
    List.map ~f libraries |> List.concat
  in
  mark_overhead ();
  let plain_modules = String.concat ~sep:" " all_namespaces in
  exec_or_die ~cwd:build_dir ocamldep "-modules %s > namespaces.dep"
    plain_modules;
  let as_map_modules = String.concat ~sep:" -as-map " all_namespaces in
  exec_or_die ~cwd:build_dir ocamldep "-modules -as-map %s > rawmaps.dep"
    as_map_modules;
  mark_timing "Scan maps (ocamldep)";
  let raw_dependencies =
    read_ocamldep (build_dir ^/ "namespaces.dep") |> List.sort ~cmp:compare
  in
  let map_dependencies =
    read_ocamldep (build_dir ^/ "rawmaps.dep") |> List.sort ~cmp:compare
  in
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
    let namespaces, not_namespaces =
      List.partition ~f:(fun x -> StringMap.mem x maps) namespaces
    in
    (not_namespaces @ ml_files, namespaces, c_files)
  in
  let libraries = List.map ~f:map_namespaces libraries in
  let map_modules =
    List.map ~f:(fun (_, x, _) -> x) libraries |> List.concat
  in
  let maps =
    mark_overhead ();
    let maps = String.concat " -map " map_modules in
    let as_maps = String.concat ~sep:" -as-map " map_modules in
    exec_or_die ~cwd:build_dir ocamldep
      "-modules -map %s -as-map %s > maps.dep" maps as_maps;
    mark_timing "Compute map dependencies (ocamldep)";
    read_ocamldep (build_dir ^/ "maps.dep")
  in
  let apply_dependencies (ml_files, namespaces, c_files) =
    let namespaces =
      let f namespace = List.find_opt ~f:(fun (x, _) -> x = namespace) maps in
      List.filter_map ~f namespaces
    in
    (ml_files, namespaces, c_files)
  in
  (map_modules, List.map ~f:apply_dependencies libraries)

type 'a build_task = Task of 'a option * (StringSet.t -> 'a build_task)

let get_compilation_order dependencies modules =
  mark_overhead ();
  let get_released released modules =
    let buildable, modules =
      List.partition ~f:(fun (_, _, dep_count) -> !dep_count = 0) modules
    in
    let buildable =
      List.rev_map ~f:(fun (name, releases, _) -> (name, releases)) buildable
    in
    (buildable, modules)
  in
  (* Convert dependencies to a map *)
  let dependencies =
    (* Each item is augmented with a reference counter of the number of
       dependencies it has. *)
    let refs =
      let f map (file, deps) =
        StringMap.add file (ref [], ref (List.length deps), deps) map
      in
      List.fold_left ~f ~init:StringMap.empty dependencies
    in
    (* Now ensure that every item has a list of references it should
       *decrement* when it's built. *)
    let propagate_updates file (_, dep_count, deps) =
      let update_dep dep =
        let updates, _, _ = StringMap.find dep refs in
        updates := dep_count :: !updates
      in
      List.iter ~f:update_dep deps
    in
    StringMap.iter propagate_updates refs;
    refs
  in
  let modules =
    (* Expand the list to include everything which needs building *)
    let rec add_dependencies to_build filename =
      if StringSet.mem filename to_build then
        to_build
      else
        let _, _, deps = StringMap.find filename dependencies in
        List.fold_left ~f:add_dependencies
          ~init:(StringSet.add filename to_build)
          deps
    in
    let finalise_dependency file =
      let { contents = updates }, dep_count, _ =
        StringMap.find file dependencies
      in
      (file, updates, dep_count)
    in
    (* The final result is a list of files to build. Each item can be built
       when its counter reaches zero and when its built all the refs in the
       list should be decremented to release more items (this is measurably
       faster than keeping dependency sets) *)
    List.fold_left ~f:add_dependencies ~init:StringSet.empty modules
    |> StringSet.elements
    |> List.map ~f:finalise_dependency
  in
  let rec seq modules buildable released =
    (* XXX Any coalescing (e.g. of .mli) and so forth would take place here *)
    match buildable with
    | [] ->
      let buildable, modules = get_released released modules in
      if buildable = [] then
        Task (None, seq modules [])
      else
        seq modules buildable released
    | item :: buildable -> Task (Some item, seq modules buildable)
  in
  seq modules []

let scheduler parallel_count name exe external_includes external_libraries
    c_files build_modules =
  let rec scheduler final_link released tasks build_modules =
    let finished_pid =
      if Sys.win32 || tasks = [] then
        0
      else
        let pid, status = Unix.wait () in
        if status <> WEXITED 0 then
          (* XXX Can maybe do better... *)
          failwith "a command failed";
        pid
    in
    let rec process_tasks released n tasks = function
      | [] -> (released, tasks, n)
      | ((pid, file, releases) as task) :: rest ->
        if pid = finished_pid then
          let () = List.iter ~f:decr releases in
          process_tasks (StringSet.add file released) n tasks rest
        else
          let pid', status = Unix.waitpid [ WNOHANG ] pid in
          if pid' = 0 then
            process_tasks released (succ n) (task :: tasks) rest
          else (
            if status <> WEXITED 0 then
              (* XXX As above *)
              failwith "a command failed";
            let () = List.iter ~f:decr releases in
            process_tasks (StringSet.add file released) n tasks rest
          )
    in
    let released, tasks, num_tasks = process_tasks released 0 [] tasks in
    let rec add_tasks final_link tasks build_modules n =
      if n < parallel_count then
        match build_modules released with
        | Task (Some (file, releases), next) ->
          let task =
            let cwd = Sys.getcwd () in
            Sys.chdir build_dir;
            Printf.printf "%s -c -g -no-alias-deps -w -49 -I +threads %s\n%!"
              ocamlopt file;
            let args =
              let args =
                [ "ocamlopt"; "-c"; "-g"; "-no-alias-deps"; "-w"; "-49" ]
              in
              Array.of_list (List.concat [ args; external_includes; [ file ] ])
            in
            let pid =
              Unix.create_process ocamlopt args Unix.stdin Unix.stdout
                Unix.stderr
            in
            Sys.chdir cwd;
            (pid, file, releases)
          in
          let final_link =
            if Filename.extension file = ".ml" then
              (Filename.chop_extension file ^ ".cmx") :: final_link
            else
              final_link
          in
          add_tasks final_link (task :: tasks) next (succ n)
        | Task (None, next) -> (final_link, tasks, next)
      else
        (final_link, tasks, build_modules)
    in
    let final_link, tasks, build_modules =
      add_tasks final_link tasks build_modules num_tasks
    in
    if tasks = [] then
      exec_or_die ~cwd:build_dir ocamlopt "-o %s%s -g %s %s %s %s" name exe
        (String.concat ~sep:" " external_includes)
        external_libraries c_files
        (String.concat ~sep:" " (List.rev final_link))
    else (
      if Sys.win32 then ignore (Unix.select [] [] [] 0.001);
      scheduler final_link released tasks build_modules
    )
  in
  scheduler [] StringSet.empty [] build_modules

let process_task ~parallel_count ~maps ~dependencies ~c_files
    { target = name, main; external_libraries; local_libraries } =
  let c_files = String.concat ~sep:" " c_files in
  let build_modules =
    get_compilation_order dependencies [ Filename.basename main ]
  in
  let external_libraries, external_includes =
    let convert = function
      | "threads.posix" -> ("threads.cmxa", Some [ "-I"; "+threads" ])
      | "unix" -> ("unix.cmxa", None)
      | s -> failwith "unhandled external library %s" s
    in
    let externals = List.map ~f:convert external_libraries in
    let external_libraries = List.map ~f:fst externals in
    let external_includes = List.filter_map ~f:snd externals in
    (String.concat ~sep:" " external_libraries, List.concat external_includes)
  in
  let rec j1_version released seq =
    match seq released with
    | Task (Some (item, releases), next) ->
      List.iter ~f:decr releases;
      let released = StringSet.add item released in
      item :: j1_version released next
    | Task (None, _) -> []
  in
  mark_overhead ();
  if parallel_count = 1 then
    let () =
      write_args "mods_list" (j1_version StringSet.empty build_modules)
    in
    exec_or_die ~cwd:build_dir ocamlopt
      "-o %s%s -g -no-alias-deps -w -49 %s %s %s -args mods_list" name exe
      (String.concat ~sep:" " external_includes)
      external_libraries c_files
  else
    scheduler parallel_count name exe external_includes external_libraries
      c_files build_modules;
  mark_timing "Compilation"

(** {2 Bootstrap process *)
let main () =
  let start = Unix.gettimeofday () in
  rm_rf build_dir;
  mkdir_p build_dir;
  (* XXX Clean out *.ml, *.mli, *.c and any other crap from the build directory *)
  let parallel_count =
    if Sys.win32 then
      1
    (* Sad, but true *)
    else
      4
  in
  let maps, libraries = assemble_libraries task in
  let c_files =
    List.map ~f:(fun (_, _, c_files) -> c_files) libraries |> List.concat
  in
  let dependencies = get_dependencies ~maps libraries in
  process_task ~parallel_count ~maps ~dependencies ~c_files task;
  let print_timing last (time, message) =
    Printf.eprintf "%s: %f\n"
      (Option.value ~default:"Overhead" message)
      (time -. last);
    time
  in
  copy_to "boot/dune.install" ~dir:".";
  Queue.fold print_timing start timings |> ignore

let () = main ()
