open! Core
open Hardcaml
module Insn = Riscv_isa.Insn

module Csr_bank_sim =
  Cyclesim.With_interface (Privileged.Csr_bank.I) (Privileged.Csr_bank.O)

let csr_address = Privileged.Csrs.addresses.custom0
let addresses = Privileged.Csrs.addresses

let program =
  let open Insn in
  [ Csr { op = Csrrw; rd = 1; src = Imm (Int32.of_int_exn 5); csr = csr_address }
  ; Csr { op = Csrrs; rd = 2; src = Imm (Int32.of_int_exn 2); csr = csr_address }
  ; Csr { op = Csrrc; rd = 3; src = Imm (Int32.of_int_exn 1); csr = csr_address }
  ; IntImm (Add, { rd = 8; rs1 = 0; imm = Int32.of_int_exn 0xa5 })
  ; Csr { op = Csrrw; rd = 4; src = Reg 8; csr = csr_address }
  ; IntImm (Add, { rd = 9; rs1 = 0; imm = Int32.of_int_exn 0x0f })
  ; Csr { op = Csrrs; rd = 6; src = Reg 9; csr = csr_address }
  ; Csr { op = Csrrc; rd = 7; src = Reg 9; csr = csr_address }
  ; Csr { op = Csrrs; rd = 10; src = Reg 0; csr = csr_address }
  ; Csr { op = Csrrc; rd = 11; src = Imm Int32.zero; csr = csr_address }
  ; IntImm (Add, { rd = 12; rs1 = 0; imm = Int32.of_int_exn 0x83 })
  ; Csr { op = Csrrw; rd = 13; src = Reg 12; csr = addresses.mtvec }
  ; Csr { op = Csrrs; rd = 14; src = Reg 0; csr = addresses.mtvec }
  ; IntImm (Add, { rd = 15; rs1 = 0; imm = Int32.minus_one })
  ; Csr { op = Csrrw; rd = 16; src = Reg 15; csr = addresses.mie }
  ; Csr { op = Csrrs; rd = 17; src = Reg 0; csr = addresses.mie }
  ; IntImm (Add, { rd = 18; rs1 = 0; imm = Int32.of_int_exn 0x105 })
  ; Csr { op = Csrrw; rd = 19; src = Reg 18; csr = addresses.mepc }
  ; Csr { op = Csrrs; rd = 20; src = Reg 0; csr = addresses.mepc }
  ; Lui { rd = 21; imm = Int32.of_int_exn 0x1000 }
  ; Csr { op = Csrrw; rd = 22; src = Reg 21; csr = addresses.mstatus }
  ; Csr { op = Csrrs; rd = 23; src = Reg 0; csr = addresses.mstatus }
  ; IntImm (Add, { rd = 24; rs1 = 0; imm = Int32.minus_one })
  ; Csr { op = Csrrw; rd = 25; src = Reg 24; csr = addresses.medeleg }
  ; Csr { op = Csrrs; rd = 26; src = Reg 0; csr = addresses.medeleg }
  ; Csr { op = Csrrw; rd = 25; src = Reg 24; csr = addresses.mideleg }
  ; Csr { op = Csrrs; rd = 27; src = Reg 0; csr = addresses.mideleg }
  ; Csr { op = Csrrw; rd = 25; src = Reg 0; csr = addresses.mie }
  ; Csr { op = Csrrw; rd = 25; src = Reg 24; csr = addresses.sie }
  ; Csr { op = Csrrs; rd = 28; src = Reg 0; csr = addresses.mie }
  ; Csr { op = Csrrs; rd = 29; src = Reg 0; csr = addresses.sie }
  ; IntImm (Add, { rd = 24; rs1 = 0; imm = Int32.of_int_exn 0x122 })
  ; Csr { op = Csrrw; rd = 25; src = Reg 24; csr = addresses.sstatus }
  ; Csr { op = Csrrs; rd = 30; src = Reg 0; csr = addresses.mstatus }
  ; Csr { op = Csrrs; rd = 31; src = Reg 0; csr = addresses.sstatus }
  ]
;;

let expected_regs =
  let regs = Array.create ~len:32 Int32.zero in
  List.iter
    [ 2, 0x5
    ; 3, 0x7
    ; 4, 0x6
    ; 6, 0xa5
    ; 7, 0xaf
    ; 8, 0xa5
    ; 9, 0x0f
    ; 10, 0xa0
    ; 11, 0xa0
    ; 12, 0x83
    ; 14, 0x80
    ; 15, -1
    ; 17, 0x800
    ; 18, 0x105
    ; 20, 0x104
    ; 21, 0x1000
    ; 23, 0x1800
    ; 24, 0x122
    ; 26, 0x30c
    ; 27, 0x800
    ; 28, 0x800
    ; 29, 0x800
    ; 30, 0x1922
    ; 31, 0x122
    ]
    ~f:(fun (register, value) -> regs.(register) <- Int32.of_int_exn value);
  regs
;;

let () =
  let emulator = Riscvemulate.State.init ~insns:program ~addr:Int32.zero in
  let sim = Sim.Cpu.create ~memory:(Hashtbl.copy emulator.memory) No_waves in
  List.iter program ~f:(fun _ ->
    Sim.Cpu.cycle_insn sim;
    Riscvemulate.Unpriv.step emulator);
  Sim.Cpu.flush sim;
  let sim_regs = Sim.Cpu.regs sim
  and emulator_regs = Riscvemulate.State.regs emulator in
  if not (Array.equal Int32.equal sim_regs expected_regs)
  then raise_s [%message "CSR hardware result mismatch" (sim_regs : int32 array)];
  if not (Array.equal Int32.equal emulator_regs expected_regs)
  then raise_s [%message "CSR emulator result mismatch" (emulator_regs : int32 array)];
  Stdio.print_string "CSR program: all modes good\n"
;;
