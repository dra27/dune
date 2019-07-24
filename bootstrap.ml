(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 3.07! *)

let v = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> a, b)
in
  let code =
    if v < (4, 08) then begin
      prerr_endline "Dune 2.0 requires OCaml 4.08 to compile";
      1
    end else
      Sys.command "ocaml duneboot.ml"
  in
  exit code;;
