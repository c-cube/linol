(lang dune 2.0)

;(implicit_transitive_deps false)

(generate_opam_files true)

(using menhir 2.0)

(name linol)

(version 0.9)

(license MIT)

(authors "Simon Cruanes")

(maintainers "Simon Cruanes")

(source
 (github c-cube/linol))

(package
 (name linol)
 (synopsis "LSP server library")
 (depends
  ("yojson"
   (>= "1.6"))
  "logs"
  ("trace"
   (>= "0.4"))
  ("ocaml"
   (>= "4.14"))
  ("odoc" :with-doc)
  ; The following dependencies are needed for lsp, which we vendor
  (uutf
   (>= 1.0.2))
  (ppx_yojson_conv_lib
   (>= "v0.14"))))

(package
 (name linol-lwt)
 (synopsis "LSP server library (with Lwt for concurrency)")
 (depends
  ("yojson"
   (>= "1.6"))
  ("linol"
   (= :version))
  base-unix
  ("lwt"
   (and
    (>= "5.1")
    (< "6.0")))
  ("odoc" :with-doc)))

(package
 (name linol-eio)
 (authors "Nick Hu")
 (synopsis "LSP server library (with Eio for concurrency)")
 (depends
  ("yojson"
   (>= "1.6"))
  ("linol"
   (= :version))
  base-unix
  ("eio"
   (and
    (>= "1.0")
    (< "2.0")))
  (eio_main :with-test)
  ("odoc" :with-doc)))
