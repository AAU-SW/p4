(lang dune 3.21)

(name markdowns)

(generate_opam_files true)

(source
 (github AAU-SW/p4))

(authors "P4 <cs-26-sw-kbh-4-gr-6@student.aau.dk>")

(maintainers "P4 <cs-26-sw-kbh-4-gr-6@student.aau.dk>")

(license LICENSE)

(documentation https://github.com/AAU-SW/p4/blob/main/README.md)

(using menhir 3.0)
(package
 (name markdowns)
 (synopsis "P4: Markdowns, a Markdown Scripting Language")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
