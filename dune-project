(lang dune 2.0)

;(implicit_transitive_deps false)

(generate_opam_files true)

(using menhir 2.0)

(name linol)

(version 0.5)

(license MIT)

(authors "Simon Cruanes")

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
