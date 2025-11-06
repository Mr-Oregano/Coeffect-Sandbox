(lang dune 3.20)

(name Coeffect-Sandbox)

(generate_opam_files true)

(source
 (github Mr-Oregano/Coeffect-Sandbox))

(license MIT)

(authors Mr-Oregano)

(maintainers Mr-Oregano)

(package
 (name Coeffect-Sandbox)
 (synopsis "Sample Coeffect Type System")
 (description
  "Coeffects can be used to track what a program requires from its environment, this can help eliminate a variety of bugs.")
 (depends
  (ocaml
   (>= 5.4))
  (dune
   (>= 3.0))
  (alcotest :with-test))
 (tags
  ("coeffects" "programming-languages")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
