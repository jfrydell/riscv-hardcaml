open! Core
open Riscvemulate
open Trap_test_utils

let test_synchronous_traps_and_returns () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x80
          ; csrw mtvec 1
          ; Ecall
          ; addi ~rd:10 ~rs1:0 1
          ; Ebreak
          ; addi ~rd:11 ~rs1:0 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; ( 0x40
        , [ addi ~rd:12 ~rs1:0 3
          ; Ecall
          ; addi ~rd:13 ~rs1:0 4
          ; Mret
          ; addi ~rd:14 ~rs1:0 5
          ; addi ~rd:31 ~rs1:0 1
          ; halt
          ] )
      ; 0x80, exception_handler
      ]
  in
  let sim, commits =
    run_hardware memory ~done_:(fun sim -> Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  Sim.Cpu.flush sim;
  let emulator =
    run_emulator memory ~done_:(fun emulator -> Int32.equal emulator.regs.(31) Int32.one)
  in
  let regs = Sim.Cpu.regs sim in
  if not (Array.equal Int32.equal regs emulator.regs)
  then
    raise_s
      [%message
        "Hardware/emulator mismatch after synchronous traps"
          (regs : int32 array)
          (emulator.regs : int32 array)];
  check_reg regs 10 1;
  check_reg regs 11 2;
  check_reg regs 12 3;
  check_reg regs 13 4;
  check_reg regs 14 5;
  check_reg regs 20 2;
  check_reg regs 21 0x50;
  check_reg regs 22 0x30200073;
  check_reg regs 24 4;
  check_reg regs 25 0xb382;
  List.iter [ 0x08; 0x10; 0x44; 0x4c ] ~f:(fun exception_pc ->
    if List.mem commits (int exception_pc) ~equal:Int32.equal
    then
      raise_s
        [%message
          "Faulting instruction incorrectly committed"
            (exception_pc : int)
            (commits : int32 list)])
;;

let test_masking_and_vectored_machine_interrupt () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x101
          ; csrw mtvec 1
          ; addi ~rd:2 ~rs1:0 8
          ; csrw mstatus 2
          ; addi ~rd:10 ~rs1:0 1
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mie 2
          ; addi ~rd:11 ~rs1:0 1
          ; addi ~rd:12 ~rs1:12 1
          ; Branch (Eq, { rs1 = 0; rs2 = 0; imm = int (-4) })
          ] )
      ; 0x12c, interrupt_handler
      ]
  in
  let clear_after_handler = ref false in
  let sim, _ =
    run_hardware
      memory
      ~before_cycle:(fun sim ->
        if Int32.equal (Sim.Cpu.regs sim).(30) Int32.one then clear_after_handler := true;
        Sim.Cpu.set_interrupt sim (not !clear_after_handler))
      ~done_:(fun sim ->
        let regs = Sim.Cpu.regs sim in
        !clear_after_handler
        && Int32.equal regs.(24) Int32.one
        && Int32.(regs.(12) >= of_int_exn 2))
  in
  let regs = Sim.Cpu.regs sim in
  check_reg regs 10 1;
  check_reg regs 11 1;
  check_reg regs 20 (-2147483637);
  check_reg regs 21 0x24;
  check_reg regs 22 0x1880;
  check_reg regs 24 1
;;

let test_interrupt_from_user_ignores_machine_global_enable () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x80
          ; csrw mtvec 1
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mie 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; 0x40, [ addi ~rd:10 ~rs1:0 1; addi ~rd:11 ~rs1:0 1; addi ~rd:31 ~rs1:0 1; halt ]
      ; 0x80, interrupt_handler
      ]
  in
  let clear_after_handler = ref false in
  let sim, _ =
    run_hardware
      memory
      ~before_cycle:(fun sim ->
        if Int32.equal (Sim.Cpu.regs sim).(30) Int32.one then clear_after_handler := true;
        Sim.Cpu.set_interrupt sim (not !clear_after_handler))
      ~done_:(fun sim ->
        !clear_after_handler && Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  let regs = Sim.Cpu.regs sim in
  check_reg regs 10 1;
  check_reg regs 11 1;
  check_reg regs 20 (-2147483637);
  check_reg regs 21 0x44;
  check_reg regs 22 0;
  check_reg regs 24 1
;;

let test_delegated_supervisor_exceptions_and_sret () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x200
          ; csrw stvec 1
          ; addi ~rd:2 ~rs1:0 ((1 lsl 2) lor (1 lsl 3) lor (1 lsl 8) lor (1 lsl 9))
          ; csrw medeleg 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mstatus 2
          ; Mret
          ] )
      ; ( 0x40
        , [ csrr 13 mstatus
          ; Ecall
          ; addi ~rd:10 ~rs1:0 1
          ; addi ~rd:2 ~rs1:0 0x80
          ; csrw sepc 2
          ; csrw sstatus 0
          ; Sret
          ] )
      ; ( 0x80
        , [ Ebreak
          ; addi ~rd:11 ~rs1:0 2
          ; Ecall
          ; addi ~rd:12 ~rs1:0 3
          ; addi ~rd:31 ~rs1:0 1
          ; halt
          ] )
      ; 0x200, supervisor_exception_handler
      ]
  in
  let sim, commits =
    run_hardware memory ~done_:(fun sim -> Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  Sim.Cpu.flush sim;
  let emulator =
    run_emulator memory ~done_:(fun emulator -> Int32.equal emulator.regs.(31) Int32.one)
  in
  let regs = Sim.Cpu.regs sim in
  if not (Array.equal Int32.equal regs emulator.regs)
  then
    raise_s
      [%message
        "Hardware/emulator mismatch after delegated supervisor traps"
          (regs : int32 array)
          (emulator.regs : int32 array)];
  check_reg regs 10 1;
  check_reg regs 11 2;
  check_reg regs 12 3;
  check_reg regs 13 0;
  check_reg regs 20 8;
  check_reg regs 21 0x8c;
  check_reg regs 22 0;
  check_reg regs 23 0;
  check_reg regs 24 4;
  check_reg regs 25 0x2938;
  List.iter [ 0x40; 0x44; 0x80; 0x88 ] ~f:(fun exception_pc ->
    if List.mem commits (int exception_pc) ~equal:Int32.equal
    then
      raise_s
        [%message
          "Delegated faulting instruction incorrectly committed"
            (exception_pc : int)
            (commits : int32 list)])
;;

let test_undelegated_traps_stay_in_machine_mode () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x180
          ; csrw mtvec 1
          ; addi ~rd:1 ~rs1:0 0x200
          ; csrw stvec 1
          ; addi ~rd:2 ~rs1:0 (1 lsl 9)
          ; csrw medeleg 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mstatus 2
          ; Mret
          ] )
      ; 0x40, [ Ebreak; addi ~rd:10 ~rs1:0 1; addi ~rd:31 ~rs1:0 1; halt ]
      ; 0x180, exception_handler
      ; 0x200, supervisor_exception_handler
      ]
  in
  let sim, _ =
    run_hardware memory ~done_:(fun sim -> Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  Sim.Cpu.flush sim;
  let emulator =
    run_emulator memory ~done_:(fun emulator -> Int32.equal emulator.regs.(31) Int32.one)
  in
  let regs = Sim.Cpu.regs sim in
  if not (Array.equal Int32.equal regs emulator.regs)
  then
    raise_s
      [%message
        "Hardware/emulator mismatch after undelegated trap"
          (regs : int32 array)
          (emulator.regs : int32 array)];
  check_reg regs 10 1;
  check_reg regs 20 3;
  check_reg regs 21 0x44;
  check_reg regs 24 1
;;

let test_delegated_vectored_interrupt_and_supervisor_enable () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 0x201
          ; csrw stvec 1
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mideleg 2
          ; addi ~rd:2 ~rs1:0 0x40
          ; csrw mepc 2
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw mstatus 2
          ; Mret
          ] )
      ; ( 0x40
        , [ addi ~rd:10 ~rs1:0 1
          ; addi ~rd:11 ~rs1:11 1
          ; addi ~rd:11 ~rs1:11 1
          ; addi ~rd:2 ~rs1:0 2047
          ; addi ~rd:2 ~rs1:2 1
          ; csrw sie 2
          ; addi ~rd:2 ~rs1:0 2
          ; csrw sstatus 2
          ; addi ~rd:12 ~rs1:0 3
          ; addi ~rd:31 ~rs1:0 1
          ; halt
          ] )
      ; 0x22c, supervisor_interrupt_handler
      ]
  in
  let request_active = ref false in
  let acknowledged = ref false in
  let sim, _ =
    run_hardware
      memory
      ~before_cycle:(fun sim ->
        let regs = Sim.Cpu.regs sim in
        if Int32.equal regs.(10) Int32.one then request_active := true;
        if Int32.equal regs.(30) Int32.one
        then (
          request_active := false;
          acknowledged := true);
        Sim.Cpu.set_interrupt sim !request_active)
      ~done_:(fun sim -> !acknowledged && Int32.equal (Sim.Cpu.regs sim).(31) Int32.one)
  in
  let regs = Sim.Cpu.regs sim in
  check_reg regs 10 1;
  check_reg regs 11 2;
  check_reg regs 12 3;
  check_reg regs 20 (-2147483637);
  check_reg regs 22 0x120;
  check_reg regs 24 1;
  check_reg regs 26 0x800
;;

let () =
  test_synchronous_traps_and_returns ();
  test_masking_and_vectored_machine_interrupt ();
  test_interrupt_from_user_ignores_machine_global_enable ();
  test_delegated_supervisor_exceptions_and_sret ();
  test_undelegated_traps_stay_in_machine_mode ();
  test_delegated_vectored_interrupt_and_supervisor_enable ();
  Stdio.print_endline "Trap instructions and interrupts: all scenarios good"
;;
