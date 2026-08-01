open! Core
open Hardcaml
open Riscvemulate
open Trap_test_utils

module Interrupt_system = struct
  module System = Riscv_system.System.Make (struct
      module Cpu = struct
        let caches = Riscv_system.Cpu.Cache_config.L1s
        let disable_address_translation = true
      end
    end)

  module Dut = struct
    module I = struct
      type 'a t =
        { clocking : 'a Types.Clocking.t
        ; button : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { commit_pc : 'a With_valid.t [@bits 32]
        ; button_seen : 'a
        ; request_interrupt : 'a
        ; led_value : 'a [@bits 32]
        }
      [@@deriving hardcaml]
    end

    let create scope ({ clocking; button } : _ I.t) =
      let system = System.create ~scope ~clocking in
      System.attach_bram_memory ~size_bytes:0x8000 system;
      let led_port = System.attach_mmio_register ~addr:0x80000000 system in
      let led_value =
        Types.Clocking.reg clocking ~enable:led_port.write.valid led_port.write.value
      in
      Signal.(led_port.read_value <-- led_value);
      let button_port = System.attach_mmio_register ~addr:0x80000004 system in
      let button_seen =
        Types.Clocking.reg
          clocking
          ~enable:button_port.write.valid
          (Signal.lsb button_port.write.value)
      in
      Signal.(button_port.read_value <-- uresize ~width:32 button);
      Signal.(System.interrupt system <-- (button_seen <>: button));
      System.complete system;
      let cpu = System.cpu system in
      ({ commit_pc = cpu.commit_pc
       ; button_seen
       ; request_interrupt = System.interrupt system
       ; led_value
       }
       : _ O.t)
    ;;
  end

  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
end

let csrs csr rs1 = Csr { op = Csrrs; rd = 0; src = Reg rs1; csr }

type trap_stream_op =
  | Arithmetic of
      { register : int
      ; immediate : int
      }
  | Call
  | Break
[@@deriving sexp_of]

let trap_stream_op_generator =
  let open Quickcheck.Generator.Let_syntax in
  Quickcheck.Generator.weighted_union
    [ ( 5.
      , let%map register = Int.gen_incl 4 15
        and immediate = Int.gen_incl (-32) 31 in
        Arithmetic { register; immediate } )
    ; 2., Quickcheck.Generator.singleton Call
    ; 2., Quickcheck.Generator.singleton Break
    ]
;;

let trap_stream_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind length = Int.gen_incl 10 40 in
  let%map operations =
    Quickcheck.Generator.list_with_length length trap_stream_op_generator
  in
  (* Guarantee both synchronous trap classes appear in every trial. *)
  Call :: Break :: operations
;;

let instruction_of_stream_op = function
  | Arithmetic { register; immediate } -> addi ~rd:register ~rs1:register immediate
  | Call -> Ecall
  | Break -> Ebreak
;;

let run_trap_stream operations =
  let open Csr_address in
  let expected_traps =
    List.count operations ~f:(function
      | Call | Break -> true
      | Arithmetic _ -> false)
  in
  let user_program =
    List.map operations ~f:instruction_of_stream_op @ [ addi ~rd:31 ~rs1:0 1; halt ]
  in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x400
          ; csrw mtvec 1
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; 0x40, user_program
      ; 0x400, exception_handler
      ]
  in
  let sim, _ =
    run_hardware ~max_cycles:20_000 memory ~done_:(fun sim ->
      Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  Sim.Cpu.flush sim;
  let emulator =
    run_emulator ~max_steps:5_000 memory ~done_:(fun emulator ->
      Int32.equal emulator.regs.(31) Int32.one)
  in
  let hardware_regs = Sim.Cpu.regs sim in
  if (not (Array.equal Int32.equal hardware_regs emulator.regs))
     || not (Int32.equal hardware_regs.(24) (int expected_traps))
  then
    raise_s
      [%message
        "Randomized synchronous trap stream mismatch"
          (operations : trap_stream_op list)
          (expected_traps : int)
          (hardware_regs : int32 array)
          (emulator.regs : int32 array)]
;;

let run_delegated_trap_stream operations =
  let open Csr_address in
  let expected_traps =
    List.count operations ~f:(function
      | Call | Break -> true
      | Arithmetic _ -> false)
  in
  let user_program =
    List.map operations ~f:instruction_of_stream_op @ [ addi ~rd:31 ~rs1:0 1; halt ]
  in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x400
          ; csrw stvec 1
          ; addi ~rd:2 ~rs1:0 ((1 lsl 3) lor (1 lsl 8))
          ; csrw medeleg 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; 0x40, user_program
      ; 0x400, supervisor_exception_handler
      ]
  in
  let sim, _ =
    run_hardware ~max_cycles:20_000 memory ~done_:(fun sim ->
      Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  Sim.Cpu.flush sim;
  let emulator =
    run_emulator ~max_steps:5_000 memory ~done_:(fun emulator ->
      Int32.equal emulator.regs.(31) Int32.one)
  in
  let hardware_regs = Sim.Cpu.regs sim in
  if (not (Array.equal Int32.equal hardware_regs emulator.regs))
     || not (Int32.equal hardware_regs.(24) (int expected_traps))
  then
    raise_s
      [%message
        "Randomized delegated supervisor trap stream mismatch"
          (operations : trap_stream_op list)
          (expected_traps : int)
          (hardware_regs : int32 array)
          (emulator.regs : int32 array)]
;;

type interrupt_scenario =
  { vectored : bool
  ; delays : int list
  }
[@@deriving sexp_of]

let interrupt_scenario_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map vectored = Bool.quickcheck_generator
  and delays = Quickcheck.Generator.list_with_length 12 (Int.gen_incl 1 80) in
  { vectored; delays }
;;

let run_interrupt_scenario { vectored; delays } =
  let open Csr_address in
  let mtvec_base = 0x400 in
  let mtvec_value = mtvec_base lor if vectored then 1 else 0 in
  let handler_pc = if vectored then mtvec_base + (4 * 11) else mtvec_base in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 mtvec_value
          ; csrw mtvec 1
          ; csrr 4 mtvec
          ; addi ~rd:2 ~rs1:0 8
          ; csrw mstatus 2
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mie 2
          ; addi ~rd:3 ~rs1:0 0x600
          ; Jal { rd = 0; imm = int 0x1c }
          ] )
      ; ( 0x40
        , [ addi ~rd:10 ~rs1:10 1
          ; Store (Word, { rs1 = 3; rs2 = 10; imm = Int32.zero })
          ; Load (Word, Unsigned, { rd = 11; rs1 = 3; imm = Int32.zero })
          ; Branch (Eq, { rs1 = 0; rs2 = 0; imm = int (-12) })
          ] )
      ; handler_pc, interrupt_handler
      ]
  in
  let remaining_delays = ref delays in
  let delay = ref (List.hd_exn delays) in
  let request_active = ref false in
  let acknowledged = ref 0 in
  let main_count_at_last_ack = ref Int32.zero in
  let before_cycle sim =
    let regs = Sim.Cpu.regs sim in
    let hardware_count = Int32.to_int_exn regs.(24) in
    if hardware_count > !acknowledged
    then (
      if hardware_count <> !acknowledged + 1
      then
        raise_s
          [%message
            "Interrupt handler ran more than once for one request"
              (hardware_count : int)
              (!acknowledged : int)];
      acknowledged := hardware_count;
      main_count_at_last_ack := regs.(10);
      request_active := false;
      remaining_delays := List.tl_exn !remaining_delays;
      Option.iter (List.hd !remaining_delays) ~f:(fun next_delay -> delay := next_delay));
    if (not !request_active) && not (List.is_empty !remaining_delays)
    then if !delay = 0 then request_active := true else Int.decr delay;
    Sim.Cpu.set_interrupt sim !request_active
  in
  let sim, _ =
    run_hardware ~max_cycles:50_000 ~before_cycle memory ~done_:(fun sim ->
      !acknowledged = List.length delays
      && Int32.((Sim.Cpu.regs sim).(10) >= !main_count_at_last_ack + of_int_exn 3))
  in
  let regs = Sim.Cpu.regs sim in
  if !acknowledged <> List.length delays
  then
    raise_s
      [%message
        "Not all randomized interrupts were acknowledged"
          (!acknowledged : int)
          (delays : int list)];
  check_reg regs 1 mtvec_value;
  check_reg regs 2 2048;
  check_reg regs 4 mtvec_value;
  check_reg regs 20 (-2147483637);
  check_reg regs 24 (List.length delays);
  if Int32.(regs.(10) <= zero)
  then
    raise_s [%message "Main loop made no progress around interrupts" (regs : int32 array)]
;;

let run_supervisor_interrupt_scenario { vectored; delays } =
  let open Csr_address in
  let stvec_base = 0x400 in
  let stvec_value = stvec_base lor if vectored then 1 else 0 in
  let handler_pc = if vectored then stvec_base + (4 * 11) else stvec_base in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 stvec_value
          ; csrw stvec 1
          ; csrr 4 stvec
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mideleg 2
          ; csrw mie 2
          ; addi ~rd:3 ~rs1:0 0x40
          ; csrw mepc 3
          ; addi ~rd:2 ~rs1:2 2
          ; csrw mstatus 2
          ; Mret
          ] )
      ; ( 0x40
        , [ addi ~rd:10 ~rs1:10 1
          ; addi ~rd:11 ~rs1:11 3
          ; Branch (Eq, { rs1 = 0; rs2 = 0; imm = int (-8) })
          ] )
      ; handler_pc, supervisor_interrupt_handler
      ]
  in
  let remaining_delays = ref delays in
  let delay = ref (List.hd_exn delays) in
  let request_active = ref false in
  let acknowledged = ref 0 in
  let main_count_at_last_ack = ref Int32.zero in
  let before_cycle sim =
    let regs = Sim.Cpu.regs sim in
    let hardware_count = Int32.to_int_exn regs.(24) in
    if hardware_count > !acknowledged
    then (
      if hardware_count <> !acknowledged + 1
      then
        raise_s
          [%message
            "Delegated interrupt handler ran more than once for one request"
              (hardware_count : int)
              (!acknowledged : int)];
      acknowledged := hardware_count;
      main_count_at_last_ack := regs.(10);
      request_active := false;
      remaining_delays := List.tl_exn !remaining_delays;
      Option.iter (List.hd !remaining_delays) ~f:(fun next_delay -> delay := next_delay));
    if (not !request_active) && not (List.is_empty !remaining_delays)
    then if !delay = 0 then request_active := true else Int.decr delay;
    Sim.Cpu.set_interrupt sim !request_active
  in
  let sim, _ =
    run_hardware ~max_cycles:50_000 ~before_cycle memory ~done_:(fun sim ->
      !acknowledged = List.length delays
      && Int32.((Sim.Cpu.regs sim).(10) >= !main_count_at_last_ack + of_int_exn 3))
  in
  let regs = Sim.Cpu.regs sim in
  if !acknowledged <> List.length delays
  then
    raise_s
      [%message
        "Not all randomized delegated interrupts were acknowledged"
          (!acknowledged : int)
          (delays : int list)];
  check_reg regs 1 stvec_value;
  check_reg regs 2 2050;
  check_reg regs 4 stvec_value;
  check_reg regs 20 (-2147483637);
  check_reg regs 24 (List.length delays);
  if Int32.(regs.(10) <= zero)
  then
    raise_s
      [%message
        "Supervisor main loop made no progress around interrupts" (regs : int32 array)]
;;

type mmio_interrupt_scenario =
  { initial_button : bool
  ; delays : int list
  }
[@@deriving sexp_of]

let mmio_interrupt_scenario_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map initial_button = Bool.quickcheck_generator
  and delays = Quickcheck.Generator.list_with_length 24 (Int.gen_incl 0 48) in
  { initial_button; delays }
;;

let preload_system_program sim memory =
  Hashtbl.iter_keys memory ~f:(fun addr ->
    if Int32.(addr < zero || addr >= of_int_exn 0x8000)
    then failwithf "interrupt test program outside BRAM: 0x%lx" addr ());
  let words = Int.Table.create () in
  Hashtbl.iteri memory ~f:(fun ~key:addr ~data:byte ->
    let addr = Int32.to_int_exn addr in
    let word_address = addr / 8 in
    let shift = 8 * (addr % 8) in
    Hashtbl.update words word_address ~f:(fun current ->
      let current = Option.value current ~default:0L in
      Int64.(current lor shift_left (of_int byte) shift)));
  let main_memory =
    match Cyclesim.lookup_mem_by_name sim "main_memory_bram" with
    | Some memory -> memory
    | None ->
      let names =
        (Cyclesim.traced sim).internal_signals
        |> List.concat_map ~f:(fun signal -> signal.mangled_names)
        |> List.filter ~f:(fun name ->
          String.is_substring name ~substring:"main"
          || String.is_substring name ~substring:"bram")
      in
      raise_s [%message "Could not find test BRAM" (names : string list)]
  in
  Hashtbl.iteri words ~f:(fun ~key:address ~data ->
    Cyclesim.Memory.of_bits main_memory ~address (Bits.of_int64_trunc ~width:64 data))
;;

let system_interrupt_program =
  let open Csr_address in
  program
    [ ( 0
      , [ Lui { rd = 2; imm = Int32.of_int_exn 0x8000 }
        ; Jal { rd = 1; imm = int 0x98 }
        ; addi ~rd:10 ~rs1:10 1
        ; Branch (Eq, { rs1 = 0; rs2 = 0; imm = int (-4) })
        ] )
    ; ( 0x28
      , [ addi ~rd:2 ~rs1:2 (-16)
        ; Store (Word, { rs1 = 2; rs2 = 12; imm = int 12 })
        ; Store (Word, { rs1 = 2; rs2 = 13; imm = int 8 })
        ; Store (Word, { rs1 = 2; rs2 = 14; imm = int 4 })
        ; Store (Word, { rs1 = 2; rs2 = 15; imm = Int32.zero })
        ; csrr 13 mcause
        ; Lui { rd = 14; imm = Int32.of_string "0x80000000" }
        ; Lui { rd = 15; imm = Int32.of_string "0x80000000" }
        ; addi ~rd:14 ~rs1:14 11
        ; Load (Word, Unsigned, { rd = 12; rs1 = 15; imm = int 4 })
        ; addi ~rd:24 ~rs1:24 1
        ; Branch (Eq, { rs1 = 12; rs2 = 0; imm = int 20 })
        ; Load (Word, Unsigned, { rd = 13; rs1 = 0; imm = int 0x200 })
        ; addi ~rd:13 ~rs1:13 1
        ; Store (Word, { rs1 = 0; rs2 = 13; imm = int 0x200 })
        ; Store (Word, { rs1 = 15; rs2 = 13; imm = Int32.zero })
        ; Store (Word, { rs1 = 15; rs2 = 12; imm = int 4 })
        ; Load (Word, Unsigned, { rd = 12; rs1 = 2; imm = int 12 })
        ; Load (Word, Unsigned, { rd = 13; rs1 = 2; imm = int 8 })
        ; Load (Word, Unsigned, { rd = 14; rs1 = 2; imm = int 4 })
        ; Load (Word, Unsigned, { rd = 15; rs1 = 2; imm = Int32.zero })
        ; addi ~rd:2 ~rs1:2 16
        ; Mret
        ] )
    ; ( 0x9c
      , [ Lui { rd = 15; imm = Int32.of_string "0x80000000" }
        ; addi ~rd:15 ~rs1:15 0
        ; addi ~rd:14 ~rs1:0 42
        ; Store (Word, { rs1 = 15; rs2 = 14; imm = Int32.zero })
        ; addi ~rd:14 ~rs1:0 0x28
        ; csrw mtvec 14
        ; addi ~rd:14 ~rs1:0 2047
        ; addi ~rd:14 ~rs1:14 1
        ; csrs mie 14
        ; addi ~rd:14 ~rs1:0 8
        ; csrs mstatus 14
        ; Load (Word, Unsigned, { rd = 14; rs1 = 0; imm = int 0x200 })
        ; Store (Word, { rs1 = 15; rs2 = 14; imm = Int32.zero })
        ; Jalr { rd = 0; rs1 = 1; imm = Int32.zero }
        ] )
    ]
;;

let run_mmio_interrupt_scenario { initial_button; delays } =
  if List.is_empty delays then invalid_arg "MMIO interrupt scenario must not be empty";
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Interrupt_system.Sim.create
      ~config:(Cyclesim.Config.trace `All_named)
      (Interrupt_system.Dut.create scope)
  in
  let inputs = Cyclesim.inputs sim in
  inputs.button := Bits.of_bool initial_button;
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  preload_system_program sim system_interrupt_program;
  let regs () =
    Cyclesim.lookup_mem_by_name sim "regfile"
    |> Option.value_exn
    |> Cyclesim.Memory.read_all
    |> Array.map ~f:Bits.to_int32_trunc
  in
  let output () = Cyclesim.outputs sim in
  let event_index = ref 0 in
  let countdown = ref (List.hd_exn delays) in
  let button = ref initial_button in
  let waiting_for_ack = ref initial_button in
  let initial_pending = ref initial_button in
  let last_handler_count = ref 0 in
  let resume_at = ref None in
  let started = ref false in
  let rec loop cycles =
    let current_regs = regs () in
    let current_output = output () in
    let complete =
      !event_index = List.length delays
      && (not !waiting_for_ack)
      && Option.value_map !resume_at ~default:true ~f:(fun minimum ->
        Int32.to_int_exn current_regs.(10) >= minimum)
    in
    if complete
    then ()
    else if cycles = 0
    then
      raise_s
        [%message
          "MMIO interrupt timing scenario timed out"
            (delays : int list)
            (!event_index : int)
            (!waiting_for_ack : bool)
            (current_regs : int32 array)
            (current_output.commit_pc : Bits.t ref With_valid.t)
            (Bits.to_bool !(current_output.request_interrupt) : bool)
            (Bits.to_bool !(current_output.button_seen) : bool)]
    else (
      if (not !started) && Int32.(current_regs.(10) > zero) then started := true;
      if !waiting_for_ack
      then (
        if Bool.equal (Bits.to_bool !(current_output.button_seen)) !button
           && Int32.to_int_exn current_regs.(24) > !last_handler_count
        then (
          waiting_for_ack := false;
          resume_at := Some (Int32.to_int_exn current_regs.(10) + 3);
          if !initial_pending
          then (
            initial_pending := false;
            countdown := List.hd_exn delays)
          else (
            Int.incr event_index;
            Option.iter (List.nth delays !event_index) ~f:(fun delay ->
              countdown := delay))))
      else if !started && !event_index < List.length delays
      then
        if !countdown = 0
        then (
          button := not !button;
          waiting_for_ack := true;
          last_handler_count := Int32.to_int_exn current_regs.(24))
        else Int.decr countdown;
      inputs.button := Bits.of_bool !button;
      Cyclesim.cycle sim;
      loop (cycles - 1))
  in
  loop 250_000;
  let final_regs = regs () in
  let expected_events = List.length delays + if initial_button then 1 else 0 in
  if Int32.to_int_exn final_regs.(24) <> expected_events
  then
    raise_s
      [%message
        "MMIO interrupt handler count mismatch"
          (delays : int list)
          (final_regs : int32 array)];
  let final_output = output () in
  if not (Bool.equal (Bits.to_bool !(final_output.button_seen)) !button)
  then
    raise_s
      [%message
        "MMIO interrupt was not acknowledged"
          (delays : int list)
          (final_regs : int32 array)]
  else (
    let high_events = ref (if initial_button then 1 else 0) in
    let next_button = ref initial_button in
    List.iter delays ~f:(fun _ ->
      next_button := not !next_button;
      if !next_button then Int.incr high_events);
    let led_value = Bits.to_int32_trunc !(final_output.led_value) in
    if not (Int32.equal led_value (Int32.of_int_exn !high_events))
    then
      raise_s
        [%message
          "Conditional interrupt handler state mismatch"
            (delays : int list)
            (led_value : int32)
            (!high_events : int)])
;;

let () =
  Quickcheck.test
    ~seed:(`Deterministic "trap-stream-fuzz")
    ~trials:30
    ~sexp_of:[%sexp_of: trap_stream_op list]
    trap_stream_generator
    ~f:run_trap_stream;
  Quickcheck.test
    ~seed:(`Deterministic "interrupt-timing-fuzz")
    ~trials:8
    ~sexp_of:[%sexp_of: interrupt_scenario]
    interrupt_scenario_generator
    ~f:run_interrupt_scenario;
  Quickcheck.test
    ~seed:(`Deterministic "delegated-trap-stream-fuzz")
    ~trials:30
    ~sexp_of:[%sexp_of: trap_stream_op list]
    trap_stream_generator
    ~f:run_delegated_trap_stream;
  Quickcheck.test
    ~seed:(`Deterministic "supervisor-interrupt-timing-fuzz")
    ~trials:8
    ~sexp_of:[%sexp_of: interrupt_scenario]
    interrupt_scenario_generator
    ~f:run_supervisor_interrupt_scenario;
  run_mmio_interrupt_scenario
    { initial_button = false; delays = List.init 96 ~f:(fun index -> index mod 32) };
  run_mmio_interrupt_scenario
    { initial_button = true; delays = List.init 96 ~f:(fun index -> index mod 32) };
  Quickcheck.test
    ~seed:(`Deterministic "mmio-interrupt-ack-timing-fuzz")
    ~trials:8
    ~sexp_of:[%sexp_of: mmio_interrupt_scenario]
    mmio_interrupt_scenario_generator
    ~f:run_mmio_interrupt_scenario;
  Stdio.print_endline "Trap and interrupt fuzzing: all scenarios good"
;;
