open StdLabels

(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 3.07! *)

let modules = [ "boot/libs"; "boot/duneboot" ]

let prog = ".duneboot.exe"

let runf fmt =
  Printf.ksprintf
    (fun cmd ->
      prerr_endline cmd;
      Sys.command cmd)
    fmt

let exit_if_non_zero = function
  | 0 -> ()
  | n -> exit n

let read_file fn =
  let ic = open_in_bin fn in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) in
  let compiler, which =
    if v >= (4, 08) then
      ("ocamlc", "")
    else
      let compiler = "ocamlfind -toolchain secondary ocamlc" in
      let output_fn = ".ocamlfind-output" in
      let n = runf "%s 2>%s" compiler output_fn in
      let s = read_file output_fn in
      Sys.remove output_fn;
      prerr_endline s;
      if n <> 0 || s <> "" then (
        Format.eprintf "@[%a@]@." Format.pp_print_text
          "The ocamlfind's secondary toolchain does not seem to be correctly \
           installed.\n\
           Dune requires OCaml 4.08 or later to compile.\n\
           Please either upgrade your compile or configure a secondary OCaml \
           compiler (in opam, this can be done by installing the \
           ocamlfind-secondary package).";
        exit 2
      );
      (compiler, "secondary")
  in
  exit_if_non_zero
    (runf "%s %s -o %s unix.cma %s" compiler
       (* Make sure to produce a self-contained binary as dlls tend to cause
          issues *)
       ( if v < (4, 10) then
         "-custom"
       else
         "-output-complete-obj" )
       prog
       (String.concat ~sep:" " (List.map modules ~f:(fun m -> m ^ ".ml"))));
  List.iter modules ~f:(fun m ->
      List.iter [ ".cmi"; ".cmo" ] ~f:(fun ext -> Sys.remove (m ^ ext)));
  let n = runf "./%s %s" prog which in
  Sys.remove prog;
  exit n
