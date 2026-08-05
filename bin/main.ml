open! Core
open Hardcaml
open Hardcaml_waveterm
module Command = Core.Command
module Insn = Riscv_isa.Insn
module State = Riscvemulate.State
module Unpriv = Riscvemulate.Unpriv

module Trace_info = struct
  type t =
    | Reg of int
    | Pc
    | Insn

  let parse_one token =
    let token = String.strip token in
    match String.lowercase token with
    | "pc" -> Pc
    | "insn" -> Insn
    | _ ->
      let reg = Int.of_string token in
      if Int.between reg ~low:0 ~high:31
      then Reg reg
      else failwithf "register index out of range: %d" reg ()
  ;;

  let parse value =
    value
    |> String.split ~on:','
    |> List.filter ~f:(fun token -> not (String.is_empty (String.strip token)))
    |> List.map ~f:parse_one
  ;;
end

type loaded_program =
  { memory : int Int32.Table.t
  ; insn_count : int
  ; description : string
  }

module Debug_target = struct
  type t =
    | Basic of int
    | Fuzz of
        { name : string
        ; seed : int
        ; insn_count : int
        }

  let load_basic_test basic_test =
    let test = Test_definitions.Basic.get_exn basic_test in
    let emulator = State.init ~insns:test.program ~addr:Int32.zero in
    { memory = Hashtbl.copy emulator.memory
    ; insn_count = test.insn_count
    ; description = [%string "basic test %{test.name} (%{basic_test#Int})"]
    }
  ;;

  let load_fuzz_test ~name ~seed ~insn_count =
    let test = Fuzz.Fuzz_tests.of_name_exn name in
    let insn_stream = Fuzz.Fuzzing.insn_stream ~reg_max:test.reg_max ~seed in
    let memory, _trace =
      Fuzz.Fuzzing.generate_program ~insn_count ~insn_stream ~filter:test.filter
    in
    { memory
    ; insn_count
    ; description =
        [%string "fuzz test %{name} seed=%{seed#Int} insn_count=%{insn_count#Int}"]
    }
  ;;

  let to_program = function
    | Basic basic_test -> load_basic_test basic_test
    | Fuzz { name; seed; insn_count } -> load_fuzz_test ~name ~seed ~insn_count
  ;;
end

type sim_run =
  { cpu : Sim.Cpu.t
  ; waves : Waveform.t option
  }

let create_sim ~memory ~view_waves =
  if view_waves
  then (
    let sim, waves =
      Sim.Cpu.create ~memory ~config:(Cyclesim.Config.trace `Everything) Waves
    in
    { cpu = sim; waves = Some waves })
  else { cpu = Sim.Cpu.create ~memory No_waves; waves = None }
;;

let load_hw_insn ~memory ~pc =
  let insn_bits = State.load ~memory ~addr:pc ~size:4 ~extend:Insn.Unsigned in
  match Insn.of_int32 insn_bits with
  | Ok insn -> Sexp.to_string_hum [%sexp (insn : Insn.insn)]
  | Error error -> Error.to_string_hum error
;;

let format_trace_field ~memory ~pc_valid ~pc regs = function
  | Trace_info.Reg reg -> [%string "x%{reg#Int}=%{regs.(reg)#Int32}"]
  | Pc -> if pc_valid then [%string "pc=%{pc#Int32}"] else "pc=<no-commit>"
  | Insn ->
    if pc_valid then [%string "insn=%{load_hw_insn ~memory ~pc}"] else "insn=<no-commit>"
;;

let print_hw_trace ~cycle sim trace_infos =
  if not (List.is_empty trace_infos)
  then (
    let commit_pc = Sim.Cpu.commit_pc sim in
    let pc_valid = Option.is_some commit_pc in
    let pc = Option.value commit_pc ~default:Int32.zero in
    let regs = Sim.Cpu.regs sim in
    let fields =
      List.map
        trace_infos
        ~f:(format_trace_field ~memory:(Sim.Cpu.memory sim) ~pc_valid ~pc regs)
      |> String.concat ~sep:" "
    in
    print_endline [%string "hw cycle=%{cycle#Int} %{fields}"])
;;

let print_emulator_trace ~step emulator trace_infos =
  if not (List.is_empty trace_infos)
  then (
    let pc = State.pc emulator in
    let regs = State.regs emulator in
    let fields =
      List.map trace_infos ~f:(function
        | Trace_info.Reg reg -> [%string "x%{reg#Int}=%{regs.(reg)#Int32}"]
        | Pc -> [%string "pc=%{pc#Int32}"]
        | Insn ->
          let insn = Unpriv.current_pc_insn emulator in
          [%string "insn=%{Sexp.to_string_hum [%sexp (insn : Insn.insn)]}"])
      |> String.concat ~sep:" "
    in
    print_endline [%string "emu step=%{step#Int} %{fields}"])
;;

let run_emulator_trace ~memory ~insn_count trace_infos =
  if not (List.is_empty trace_infos)
  then (
    let emulator = State.with_mem (Hashtbl.copy memory) in
    for step = 1 to insn_count do
      print_emulator_trace ~step emulator trace_infos;
      Unpriv.step emulator
    done)
;;

let run_simulation ~memory ~insn_count ~max_cycles ~extra_cycles ~trace_infos ~view_waves =
  let sim = create_sim ~memory:(Hashtbl.copy memory) ~view_waves in
  let committed = ref 0 in
  let cycle = ref 0 in
  let cycle_limit = Option.value max_cycles ~default:Int.max_value in
  let committed_cycle_limit = ref None in
  while
    !cycle < cycle_limit
    && !cycle < Option.value !committed_cycle_limit ~default:Int.max_value
  do
    Int.incr cycle;
    print_hw_trace ~cycle:!cycle sim.cpu trace_infos;
    if Option.is_some (Sim.Cpu.commit_pc sim.cpu) then Int.incr committed;
    Sim.Cpu.cycle sim.cpu;
    if !committed >= insn_count && Option.is_none !committed_cycle_limit
    then committed_cycle_limit := Some (!cycle + extra_cycles)
  done;
  let max_cycles_suffix =
    match max_cycles with
    | None -> ""
    | Some max_cycles -> [%string " max_cycles=%{max_cycles#Int}"]
  in
  print_endline
    [%string
      "simulation complete: committed=%{!committed#Int}/%{insn_count#Int} \
       cycles=%{!cycle#Int}%{max_cycles_suffix}"];
  Option.iter sim.waves ~f:Hardcaml_waveterm_interactive.run
;;

let debug_command =
  Command.basic
    ~summary:"Run a named basic or fuzz program under the emulator and simulator"
    Command.Let_syntax.(
      let%map_open basic_test =
        flag "--basic-test" (optional int) ~doc:"N Run basic test N"
      and fuzz_test =
        flag
          "--fuzz-test"
          (optional string)
          ~doc:"CONFIG Run fuzz config: small, hazards, or coverage"
      and seed = flag "--seed" (optional int) ~doc:"N Seed for --fuzz-test"
      and insn_count =
        flag
          "--insn-count"
          (optional int)
          ~doc:
            "N Instruction count for --fuzz-test; simulation stops after this many \
             commits"
      and emulator_trace =
        flag
          "--emulator-trace"
          (optional string)
          ~doc:"INFO Comma-separated trace info: 0-31,pc,insn"
      and trace =
        flag
          "--trace"
          (optional string)
          ~doc:"INFO Comma-separated trace info: 0-31,pc,insn"
      and view_waves =
        flag
          "--view-waves"
          no_arg
          ~doc:"Open the interactive waveform viewer after simulation"
      and extra_cycles =
        flag
          "--extra-cycles"
          (optional_with_default 0 int)
          ~doc:"N Extra cycles to run at the end (e.g., for memory to flush)"
      and max_cycles =
        flag
          "--max-cycles"
          (optional int)
          ~doc:
            "N Stop simulation if this many cycles are reached before insn-count commits"
      in
      fun () ->
        let target =
          match basic_test, fuzz_test, seed, insn_count with
          | Some basic_test, None, None, None -> Debug_target.Basic basic_test
          | None, Some name, Some seed, Some insn_count ->
            Debug_target.Fuzz { name; seed; insn_count }
          | Some _, Some _, _, _ ->
            failwith "--basic-test and --fuzz-test are mutually exclusive"
          | None, Some _, None, _ -> failwith "--seed is required with --fuzz-test"
          | None, Some _, _, None -> failwith "--insn-count is required with --fuzz-test"
          | Some _, None, Some _, _ | Some _, None, _, Some _ ->
            failwith "--seed and --insn-count are only valid with --fuzz-test"
          | None, None, _, _ ->
            failwith "one of --basic-test or --fuzz-test must be specified"
        in
        let parse_trace_infos label = function
          | None -> []
          | Some value ->
            (try Trace_info.parse value with
             | exn -> failwithf "invalid %s: %s" label (Exn.to_string exn) ())
        in
        let emulator_trace = parse_trace_infos "emulator trace" emulator_trace in
        let trace = parse_trace_infos "trace" trace in
        let { memory; insn_count; description } = Debug_target.to_program target in
        print_endline [%string "loaded %{description}"];
        run_emulator_trace ~memory ~insn_count emulator_trace;
        run_simulation
          ~memory
          ~insn_count
          ~max_cycles
          ~extra_cycles
          ~trace_infos:trace
          ~view_waves)
;;

let rtl_command =
  Command.basic
    ~summary:"Dump CPU RTL to a Verilog file"
    Command.Let_syntax.(
      let%map_open output_path = anon ("output-path" %: string) in
      fun () ->
        let scope = Scope.create ~flatten_design:false () in
        (* TODO: configure system (for both RTL and debugging) *)
        let module Cpu =
          Riscv_system.Cpu.Make (struct
            let caches = Riscv_system.Cpu.Cache_config.L2
            let disable_address_translation = false
          end)
        in
        let circuit =
          let module Circuit = Circuit.With_interface (Cpu.I) (Cpu.O) in
          Circuit.create_exn ~name:"cpu" (Cpu.create scope)
        in
        let database = Scope.circuit_database scope in
        let rtl = Rtl.create ~database Verilog [ circuit ] in
        let rtl_string = Rope.to_string (Rtl.full_hierarchy rtl) in
        Out_channel.write_all output_path ~data:rtl_string)
;;

let () =
  Command.group
    ~summary:"Utilities for debugging and inspecting the CPU"
    [ "debug", debug_command; "rtl", rtl_command ]
  |> Command_unix.run
;;
