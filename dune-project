(lang dune 3.10)

(name mrm)

(generate_opam_files true)

(source
 (github Tozarin/mrm))

(authors "Startsev Matvey")

(maintainers "https://github.com/Tozarin")

(license LGPL-2.1)

(documentation https://github.com/Tozarin/mrm)

(package
 (name mrm)
 (synopsis "Module relational mapper")
 (description
  "ORM-like library to OCaml that used first-class modules insted objects")
 (depends
  ocaml
  dune
  bisect_ppx
  caqti
  caqti-driver-postgresql
  caqti-driver-sqlite3)
 (tags
  ("ORM" "MRM")))
