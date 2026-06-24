(lang dune 3.17)

(name todos_ocaml)

(package
 (name todos_ocaml)
 (synopsis "Cross-platform todo app built with DataScript OCaml and Bonsai Native")
 (description
  "A cross-platform todo app whose shared OCaml state is stored in datascript-ocaml and rendered with Bonsai Native or Bonsai Web.")
 (depends
  (ocaml (and (>= 5.2.1) (< 5.3.0)))
  (dune (>= 3.17))
  datascript_ocaml
  persistent_sorted_set_ocaml
  yojson
  bonsai_web
  js_of_ocaml
  js_of_ocaml-ppx
  (bonsai (= v0.18~preview.130.100+614))
  (core (= v0.18~preview.130.100+614))
  (ppx_jane (= v0.18~preview.130.100+614))
  bonsai_native
  bonsai_apple))
