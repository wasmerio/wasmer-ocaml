(lang dune 2.0)

(name wasmer)

(generate_opam_files true)

(source
 (github wasmerio/wasmer-ocaml))

(authors "rajdakin")

(maintainers "engineering@wasmer.io")

(license MIT)

(package
 (name wasmer)
 (synopsis "OCaml bindings for Wasmer")
 (description "The official Wasmer bindings for OCaml")
 (depends ocaml dune ctypes ctypes-foreign)
 (tags ("org:wasmer")))
