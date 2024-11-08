(lang dune 2.0)

;(implicit_transitive_deps false)

(generate_opam_files true)

(using menhir 2.0)

(name linol)

(version 0.7)

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
  ("lsp"
   (and
    (>= "1.17")
    (< "1.18")))
  atomic
  ("jsonrpc"
   (and
    (>= "1.17")
    (< "1.18")))
  ("ocaml"
   (>= "4.14"))
  ("odoc" :with-doc)))

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
  ("lsp"
   (and
    (>= "1.17")
    (< "1.18")))
  ("jsonrpc"
   (and
    (>= "1.17")
    (< "1.18")))
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
  ("lsp"
   (and
    (>= "1.17")
    (< "1.18")))
  ("jsonrpc"
   (and
    (>= "1.17")
    (< "1.18")))
  ("odoc" :with-doc)))
