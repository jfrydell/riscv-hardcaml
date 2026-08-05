open Core
open Fuzzing

type t =
  { name : string
  ; quickcheck_seed : string
  ; trials : int
  ; insn_count_low : int
  ; insn_count_high : int
  ; reg_max : int
  ; filter : Riscv_isa.Insn.insn -> bool
  }

let small =
  { name = "small"
  ; quickcheck_seed = "small-fuzz"
  ; trials = 500
  ; insn_count_low = 10
  ; insn_count_high = 10
  ; reg_max = 4
  ; filter =
      (function
        | Riscv_isa.Insn.IntImm (Riscv_isa.Insn.Add, _)
        | Riscv_isa.Insn.Load _ | Riscv_isa.Insn.Store _ | Riscv_isa.Insn.Branch _ -> true
        | _ -> false)
  }
;;

let hazards =
  { name = "hazards"
  ; quickcheck_seed = "hazard-fuzz"
  ; trials = 500
  ; insn_count_low = 100
  ; insn_count_high = 200
  ; reg_max = 4
  ; filter = Fn.const true
  }
;;

let coverage =
  { name = "coverage"
  ; quickcheck_seed = "coverage-fuzz"
  ; trials = 500
  ; insn_count_low = 500
  ; insn_count_high = 2000
  ; reg_max = 32
  ; filter = Fn.const true
  }
;;

let all_tests = [ small; hazards; coverage ]
let of_name name = List.find all_tests ~f:(fun t -> String.equal t.name name)
let of_name_exn name = of_name name |> Option.value_exn

let print_stats ~name f =
  let () = print_endline [%string "Running %{name}..."] in
  let sim_cycles = ref 0 in
  let start = Time_ns.now () in
  f ~cycle_fn:(fun () -> Int.incr sim_cycles) ();
  let end_ = Time_ns.now () in
  let duration = Time_ns.diff end_ start |> Time_ns.Span.to_sec in
  let hz = Float.(of_int !sim_cycles / duration) in
  print_endline [%string "Stats for %{name}:"];
  print_endline [%string "Duration: %{duration#Float}"];
  print_endline [%string "Total Cycles: %{!sim_cycles#Int} (%{hz#Float} Hz)"]
;;

let run_fuzz_test test =
  let name = test.name
  and quickcheck_seed = test.quickcheck_seed
  and trials = test.trials
  and insn_count_low = test.insn_count_low
  and insn_count_high = test.insn_count_high
  and reg_max = test.reg_max
  and filter = test.filter in
  print_stats ~name (fun ~cycle_fn () ->
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
      ~f:(check_equivalence ~cycle_fn ~filter ~reg_max))
;;

let%test_unit "fuzz tests" = List.iter all_tests ~f:run_fuzz_test
