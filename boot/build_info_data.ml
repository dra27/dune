let version = None

let statically_linked_libraries =
  [ "dune-private-libs.caml", None
  ; "unix", Some "[distributed with Ocaml]"
  ; "dune-private-libs.stdune", None
  ; "dune-private-libs.dune-lang", None
  ; "incremental_cycles", None
  ; "dag", None
  ; "fiber", None
  ; "memo", None
  ; "threads.posix", Some "[internal]"
  ; "xdg", None
  ; "dune_memory", None
  ; "dune_manager", None
  ; "dune-private-libs.dune_re", None
  ; "opam_file_format", None
  ; "dune-glob", None
  ; "dune-private-libs.ocaml-config", None
  ; "catapult", None
  ; "jbuild_support", None
  ; "dune-action-plugin", None
  ; "dune", None
  ; "cmdliner", None
  ; "dune-build-info", None
  ]
