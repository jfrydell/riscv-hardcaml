open Core
open Fuzz

let time ~name f =
  let () = print_endline [%string "Running %{name}..."] in
  let start = Time_ns.now () in
  f ();
  let end_ = Time_ns.now () in
  let duration = Time_ns.diff end_ start |> Time_ns.Span.to_sec in
  print_endline [%string "Duration (%{name}): %{duration#Float}"]
;;

let run_fuzz_test (test : Test_definitions.Fuzz.t) =
  let name = test.name
  and quickcheck_seed = test.quickcheck_seed
  and trials = test.trials
  and insn_count_low = test.insn_count_low
  and insn_count_high = test.insn_count_high
  and reg_max = test.reg_max
  and filter = test.filter in
  time ~name (fun () ->
    Quickcheck.test
      ~seed:(`Deterministic quickcheck_seed)
      ~trials
      ~shrinker:fuzz_config_shrinker
      ~shrink_attempts:(`Limit 100)
      ~sexp_of:[%sexp_of: fuzz_config]
      (let open Quickcheck.Generator.Let_syntax in
       let%map seed = Int.gen_incl 1 100_000
       and insn_count = Int.gen_uniform_incl insn_count_low insn_count_high in
       { seed; insn_count; working_insn_count = 1 })
      ~f:(check_equivalence ~filter ~reg_max))
;;

let () = List.iter Test_definitions.Fuzz.all ~f:run_fuzz_test
