module List = ListLabels

let skip_test =
  match Sys.getenv "SKIP_TEST" with
  | exception Not_found -> false
  | s -> bool_of_string s

let run cmd args =
  let cmd = String.concat " " (cmd :: args) in
  match Sys.command cmd with
  | 0 -> ()
  | n ->
    Printf.eprintf "'%s' failed with code %d" cmd n;
    exit n

let opam args = run "opam" args

let pin () =
  let packages =
    let packages = Sys.readdir "." |> Array.to_list in
    let packages =
      List.fold_left packages ~init:[] ~f:(fun acc fname ->
          if Filename.check_suffix fname ".opam" then
            let dot = String.rindex fname '.' in
            String.sub fname 0 dot :: acc
          else
            acc)
    in
    if skip_test then
      List.filter packages ~f:(fun pkg -> pkg = "dune")
    else
      packages
  in
  List.iter packages ~f:(fun package ->
      opam [ "pin"; "add"; package ^ ".next"; "."; "--no-action" ])

let test () =
  if skip_test then
    ()
  else
    let win = Sys.cygwin || Sys.win32 in
    if win then (
      opam [ "install"; "./dune-configurator.opam"; "--deps-only" ];
      run "make" [ "test-windows" ]
    ) else (
      opam [ "install"; "."; "--deps-only"; "--with-test" ];
      run "make" [ "dev-deps" ];
      run "make" [ "test" ]
    )

let () =
  let cmd = ref None in
  let anon s =
    match !cmd with
    | None -> cmd := Some s
    | Some _ -> raise (Arg.Bad "action already set")
  in
  Arg.parse [] anon "Usage: ocaml ci.ml pin";
  match !cmd with
  | Some "pin" -> pin ()
  | Some "test" -> test ()
  | None -> raise (Arg.Bad "action must be set")
  | _ ->
    prerr_endline "unknown command";
    exit 1
