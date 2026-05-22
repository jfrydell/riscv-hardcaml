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
 (depends ocaml dune core core_unix hardcaml hardcaml_circuits hardcaml_waveterm hardcaml_step_testbench hardcaml_test_harness)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
