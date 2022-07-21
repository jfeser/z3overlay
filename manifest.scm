(use-modules
 (guix packages)
 (gnu packages ocaml)
 (gnu packages maths)
 (feser ocaml)
 (feser sketch))

(packages->manifest
 (list dune
       ocaml
       ocaml-core
       ocaml-z3
       ocaml-zarith

       ocaml-lsp-server
       ocamlformat))
