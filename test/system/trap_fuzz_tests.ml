open! Core
open Hardcaml
open Riscv_isa.Insn
open Trap_test_utils

let bram_start = 0x40000000
let bram_start_bits = Int32.of_int_exn bram_start

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
      let system = System.create ~initial_pc:bram_start ~scope ~clocking () in
      System.attach_bram_memory ~start_addr:bram_start ~size_bytes:0x8000 system;
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

let system_interrupt_program =
  let open Csr_address in
  program
    [ ( bram_start
      , [ Lui { rd = 2; imm = Int32.of_int_exn 0x40008000 }
        ; Jal { rd = 1; imm = int 0x98 }
        ; addi ~rd:10 ~rs1:10 1
        ; Branch (Eq, { rs1 = 0; rs2 = 0; imm = int (-4) })
        ] )
    ; ( bram_start + 0x28
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
        ; Load (Word, Unsigned, { rd = 13; rs1 = 3; imm = int 0x200 })
        ; addi ~rd:13 ~rs1:13 1
        ; Store (Word, { rs1 = 3; rs2 = 13; imm = int 0x200 })
        ; Store (Word, { rs1 = 15; rs2 = 13; imm = Int32.zero })
        ; Store (Word, { rs1 = 15; rs2 = 12; imm = int 4 })
        ; Load (Word, Unsigned, { rd = 12; rs1 = 2; imm = int 12 })
        ; Load (Word, Unsigned, { rd = 13; rs1 = 2; imm = int 8 })
        ; Load (Word, Unsigned, { rd = 14; rs1 = 2; imm = int 4 })
        ; Load (Word, Unsigned, { rd = 15; rs1 = 2; imm = Int32.zero })
        ; addi ~rd:2 ~rs1:2 16
        ; Mret
        ] )
    ; ( bram_start + 0x9c
      , [ Lui { rd = 3; imm = bram_start_bits }
        ; Lui { rd = 15; imm = Int32.of_string "0x80000000" }
        ; addi ~rd:15 ~rs1:15 0
        ; addi ~rd:14 ~rs1:0 42
        ; Store (Word, { rs1 = 15; rs2 = 14; imm = Int32.zero })
        ; addi ~rd:14 ~rs1:3 0x28
        ; csrw mtvec 14
        ; addi ~rd:14 ~rs1:0 2047
        ; addi ~rd:14 ~rs1:14 1
        ; csrs mie 14
        ; addi ~rd:14 ~rs1:0 8
        ; csrs mstatus 14
        ; Load (Word, Unsigned, { rd = 14; rs1 = 3; imm = int 0x200 })
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
  System_test_utils.preload_program sim system_interrupt_program;
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

let%test_unit "system interrupt timing fuzzing" =
  run_mmio_interrupt_scenario
    { initial_button = false; delays = List.init 96 ~f:(fun index -> index mod 32) };
  run_mmio_interrupt_scenario
    { initial_button = true; delays = List.init 96 ~f:(fun index -> index mod 32) };
  Quickcheck.test
    ~seed:(`Deterministic "mmio-interrupt-ack-timing-fuzz")
    ~trials:8
    ~sexp_of:[%sexp_of: mmio_interrupt_scenario]
    mmio_interrupt_scenario_generator
    ~f:run_mmio_interrupt_scenario
;;
