let executables = [ "main" ]

let external_libraries = [ "unix"; "threads.posix" ]

let local_libraries =
  [ ("src/stdune/caml", Some "Dune_caml", false)
  ; ("src/stdune", Some "Stdune", false)
  ; ("src/dune_lang", Some "Dune_lang", false)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false)
  ; ("src/dag", Some "Dag", false)
  ; ("src/fiber", Some "Fiber", false)
  ; ("src/memo", Some "Memo", false)
  ; ("src/xdg", Some "Xdg", false)
  ; ("src/dune_memory", Some "Dune_memory", false)
  ; ("src/dune_manager", Some "Dune_manager", false)
  ; ("vendor/re/src", Some "Dune_re", false)
  ; ("vendor/opam-file-format/src", None, false)
  ; ("otherlibs/dune-glob", Some "Dune_glob", false)
  ; ("src/ocaml-config", Some "Ocaml_config", false)
  ; ("src/catapult", Some "Catapult", false)
  ; ("src/jbuild_support", Some "Jbuild_support", false)
  ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", false)
  ; ("src/dune", Some "Dune", true)
  ; ("vendor/cmdliner/src", None, false)
  ; ("otherlibs/build-info/src", Some "Build_info", false)
  ]
