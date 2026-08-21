open Core
open User_fuzzing

type t =
  { name : string
  ; quickcheck_seed : string
  ; trials : int
  ; insn_count : int
  ; exception_rate : float
  }

let all_tests =
  List.map
    [ "2% exceptions", "user-fuzz-2", 0.02; "20% exceptions", "user-fuzz-20", 0.20 ]
    ~f:(fun (name, quickcheck_seed, exception_rate) ->
      { name; quickcheck_seed; trials = 50; insn_count = 1000; exception_rate })
;;

let print_stats ~name f =
  let () = print_endline [%string "Running %{name}..."] in
  let sim_cycles = ref 0 in
  let start = Time_ns.now () in
  f ~cycle_fn:(fun () -> Int.incr sim_cycles) ();
  let duration = Time_ns.diff (Time_ns.now ()) start |> Time_ns.Span.to_sec in
  let hz = Float.(of_int !sim_cycles / duration) in
  print_endline [%string "Stats for %{name}:"];
  print_endline [%string "Duration: %{duration#Float}"];
  print_endline [%string "Total Cycles: %{!sim_cycles#Int} (%{hz#Float} Hz)"]
;;

let run_fuzz_test { name; quickcheck_seed; trials; insn_count; exception_rate } =
  print_stats ~name (fun ~cycle_fn () ->
    Quickcheck.test
      ~seed:(`Deterministic quickcheck_seed)
      ~trials
      ~sexp_of:[%sexp_of: int]
      (Int.gen_incl 1 100_000)
      ~f:(fun seed -> run_and_check ~cycle_fn ~seed ~insn_count ~exception_rate ()))
;;

let%test_unit "user fuzz tests" = List.iter all_tests ~f:run_fuzz_test
