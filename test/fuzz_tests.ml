open Core
open Fuzz

(* Test 1: Small programs with limited registers to stress pipeline hazards *)
let () =
  Quickcheck.test
    ~seed:(`Deterministic "hazard-fuzz")
    ~trials:100
    ~shrinker:fuzz_config_shrinker
    ~shrink_attempts:(`Limit 100)
    ~sexp_of:[%sexp_of: fuzz_config]
    (let open Quickcheck.Generator.Let_syntax in
     let%map seed = Int.gen_incl 1 100_000
     and insn_count = Int.gen_incl 20 200 in
     { seed; insn_count; working_insn_count = 1 })
    ~f:(check_equivalence ~reg_max:4)
;;

(* Test 2: Large programs with full register range for coverage *)
let () =
  Quickcheck.test
    ~seed:(`Deterministic "coverage-fuzz")
    ~trials:100
    ~shrinker:fuzz_config_shrinker
    ~shrink_attempts:(`Limit 100)
    ~sexp_of:[%sexp_of: fuzz_config]
    (let open Quickcheck.Generator.Let_syntax in
     let%map seed = Int.gen_incl 1 100_000
     and insn_count = Int.gen_incl 500 2000 in
     { seed; insn_count; working_insn_count = 1 })
    ~f:(check_equivalence ~reg_max:32)
;;
