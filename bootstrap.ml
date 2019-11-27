open StdLabels
open Printf

(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 4.02! *)

let min_supported_natively = (4, 06)

let verbose, keep_generated_files, debug =
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s\n" s)) in
  let verbose = ref false in
  let keep_generated_files = ref false in
  let debug = ref false in
  Arg.parse
    [ ("-j", Arg.Int ignore, "JOBS Concurrency")
    ; ("--verbose", Arg.Set verbose, " Set the display mode")
    ; ( "--keep-generated-files"
      , Arg.Set keep_generated_files
      , " Keep generated files" )
    ; ("--debug", Arg.Set debug, " Enable various debugging options")
    ; ( "--force-byte-compilation"
      , Arg.Unit ignore
      , " Force bytecode compilation even if ocamlopt is available" )
    ]
    anon "Usage: ocaml bootstrap.ml <options>\nOptions are:";
  (!verbose, !keep_generated_files, !debug)

let modules = [ "boot/libs"; "boot/duneboot" ]

let duneboot = ".duneboot"

let prog = duneboot ^ ".exe"

let () =
  at_exit (fun () ->
      Array.iter (Sys.readdir "boot") ~f:(fun fn ->
          let fn = Filename.concat "boot" fn in
          if Filename.check_suffix fn ".cmi" || Filename.check_suffix fn ".cmo"
          then
            Sys.remove fn));
  if not keep_generated_files then
    at_exit (fun () ->
        Array.iter (Sys.readdir ".") ~f:(fun fn ->
            if
              String.length fn >= String.length duneboot
              && String.sub fn 0 (String.length duneboot) = duneboot
            then
              Sys.remove fn))

let runf fmt =
  kprintf
    (fun cmd ->
      prerr_endline cmd;
      Sys.command cmd)
    fmt

let exit_if_non_zero = function
  | 0 -> ()
  | n -> exit n

(* Added in 4.02.0 *)
let pp_print_text ppf s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    match s.[!right] with
      | '\n' ->
        flush ();
        Format.pp_force_newline ppf ()
      | ' ' ->
        flush (); Format.pp_print_space ppf ()
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> incr right
  done;
  if !left <> len then flush ()

let () =
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) in
  let compiler, which =
    if v >= min_supported_natively then
      ("ocamlc", None)
    else
      let compiler = "ocamlfind -toolchain secondary ocamlc" in
      let output_fn = duneboot ^ ".ocamlfind-output" in
      let n = runf "%s 2>%s" compiler output_fn in
      let output_is_empty =
        let ic = open_in output_fn in
        let outcome =
          if in_channel_length ic = 0 then
            true
          else let rec display () =
                 prerr_endline (input_line ic);
                 display ()
               in
                 try
                   display ()
                 with End_of_file -> false
        in
          close_in ic; outcome
      in
      if n <> 0 || not output_is_empty then (
        Format.eprintf "@[%a@]@." pp_print_text
          (sprintf
             "The ocamlfind's secondary toolchain does not seem to be \
              correctly installed.\n\
              Dune requires OCaml %d.%02d or later to compile.\n\
              Please either upgrade your compile or configure a secondary \
              OCaml compiler (in opam, this can be done by installing the \
              ocamlfind-secondary package)."
             (fst min_supported_natively)
             (snd min_supported_natively));
        exit 2
      );
      (compiler, Some "--secondary")
  in
  exit_if_non_zero
    (runf "%s %s -w -24 -g -o %s -I boot unix.cma %s" compiler
       (* Make sure to produce a self-contained binary as dlls tend to cause
          issues *)
       ( if v < (4, 10) then
         "-custom"
       else
         "-output-complete-exe" )
       prog
       (String.concat ~sep:" " (List.map modules ~f:(fun m -> m ^ ".ml"))));
  let args = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
  let args =
    match which with
    | None -> args
    | Some x -> x :: args
  in
  let args = Filename.concat "." prog :: args in
  exit (runf "%s" (String.concat ~sep:" " args))
