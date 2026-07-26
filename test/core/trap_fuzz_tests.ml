open! Core
open Riscvemulate
open Trap_test_utils

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
  Stdio.print_endline "Trap and interrupt fuzzing: all scenarios good"
;;
