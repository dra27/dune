(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 3.07! *)

let secondary = "ocaml-secondary.exe"

let secondary_err = "ocaml-secondary.err"

let secondary_win = "ocaml-secondary.cmd"

let () =
  at_exit (fun () ->
      List.iter
        (fun fn -> try Sys.remove fn with _ -> ())
        [ secondary; secondary_err; secondary_win ])

let legacy_launch () =
  if
    Printf.kprintf Sys.command
      "ocamlfind -toolchain secondary ocamlmktop -o %s 2>%s" secondary
      secondary_err
    = 0
  then
    let ic = open_in secondary_err in
    let rec loop empty =
      try
        let s = input_line ic in
        prerr_endline s;
        loop false
      with End_of_file ->
        close_in ic;
        empty
    in
    if loop true then
      if Sys.win32 then (
        let oc = open_out secondary_win in
        output_string oc "@echo off\nset CAML_LD_LIBRARY_PATH=\n.\\%%*\n";
        close_out oc;
        Printf.kprintf Sys.command ".\\%s unix.cma duneboot.ml secondary"
          secondary_win
      ) else
        Printf.kprintf Sys.command
          "CAML_LD_LIBRARY_PATH= ./%s unix.cma duneboot.ml secondary" secondary
    else (
      prerr_endline "ocamlfind warnings considered fatal!";
      2
    )
  else (
    prerr_endline
      "Unable to invoke ocamlmktop in ocamlfind's secondary toolchain\n\
       Dune requires OCaml 4.08 or later to compile.\n\
       Please either upgrade your compile or configure a secondary\n\
       OCaml compiler (in opam, this can be done by installing the\n\
       ocamlfind-secondary package)";
    2
  )

let () =
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) in
  let code =
    if v < (4, 08) then
      legacy_launch ()
    else
      Sys.command "ocaml unix.cma duneboot.ml"
  in
  exit code
