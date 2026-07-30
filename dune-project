(lang dune 3.16)

(name riscvhardcaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name riscvhardcaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core hardcaml ppx_jane ppx_hardcaml (hardcaml_waveterm :with-test) (hardcaml_step_testbench :with-test) (hardcaml_test_harness :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
