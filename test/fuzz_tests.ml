open Core
open Fuzz

let time ~name f =
  let start = Time_ns.now () in
  f ();
  let end_ = Time_ns.now () in
  let duration = Time_ns.diff end_ start |> Time_ns.Span.to_sec in
  print_endline [%string "Duration (%{name}): %{duration#Float}"]
;;

(* Very small tests for debugging. Branches disallowed to make reproducing
   easier (but filter can be updated based on test that fails). *)
let () =
  time ~name:"small" (fun () ->
    Quickcheck.test
      ~seed:(`Deterministic "small-fuzz")
      ~trials:500
      ~shrinker:fuzz_config_shrinker
      ~shrink_attempts:(`Limit 100)
      ~sexp_of:[%sexp_of: fuzz_config]
      (let open Quickcheck.Generator.Let_syntax in
       let%map seed = Int.gen_incl 1 100_000
       and insn_count = Int.gen_uniform_incl 10 10 in
       { seed; insn_count; working_insn_count = 1 })
      ~f:
        (check_equivalence
           ~filter:(function
             | IntImm (Add, _) | Load _ | Store _ | Branch _ -> true
             | _ -> false)
           ~reg_max:4))
;;

(* Test 1: Small programs with limited registers to stress pipeline hazards *)
let () =
  time ~name:"hazards" (fun () ->
    Quickcheck.test
      ~seed:(`Deterministic "hazard-fuzz")
      ~trials:500
      ~shrinker:fuzz_config_shrinker
      ~shrink_attempts:(`Limit 100)
      ~sexp_of:[%sexp_of: fuzz_config]
      (let open Quickcheck.Generator.Let_syntax in
       let%map seed = Int.gen_incl 1 100_000
       and insn_count = Int.gen_uniform_incl 100 200 in
       { seed; insn_count; working_insn_count = 1 })
      ~f:(check_equivalence ~reg_max:4))
;;

(* Test 2: Large programs with full register range for coverage *)
let () =
  time ~name:"coverage" (fun () ->
    Quickcheck.test
      ~seed:(`Deterministic "coverage-fuzz")
      ~trials:500
      ~shrinker:fuzz_config_shrinker
      ~shrink_attempts:(`Limit 100)
      ~sexp_of:[%sexp_of: fuzz_config]
      (let open Quickcheck.Generator.Let_syntax in
       let%map seed = Int.gen_incl 1 100_000
       and insn_count = Int.gen_uniform_incl 500 2000 in
       { seed; insn_count; working_insn_count = 1 })
      ~f:(check_equivalence ~reg_max:32))
;;
