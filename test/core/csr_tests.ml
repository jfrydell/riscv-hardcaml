open! Core

let csr_address = 0x123

let program =
  let open Riscvemulate in
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
  ]
;;

let expected_regs =
  let regs = Array.create ~len:32 Int32.zero in
  List.iter
    [ 2, 0x5; 3, 0x7; 4, 0x6; 6, 0xa5; 7, 0xaf; 8, 0xa5; 9, 0x0f; 10, 0xa0; 11, 0xa0 ]
    ~f:(fun (register, value) -> regs.(register) <- Int32.of_int_exn value);
  regs
;;

let () =
  let emulator = Riscvemulate.init ~insns:program ~addr:Int32.zero in
  let sim = Sim.Cpu.create ~memory:(Hashtbl.copy emulator.memory) No_waves in
  List.iter program ~f:(fun _ ->
    Sim.Cpu.cycle_insn sim;
    Riscvemulate.step emulator);
  Sim.Cpu.flush sim;
  let sim_regs = Sim.Cpu.regs sim
  and emulator_regs = Riscvemulate.regs emulator in
  if not (Array.equal Int32.equal sim_regs expected_regs)
  then raise_s [%message "CSR hardware result mismatch" (sim_regs : int32 array)];
  if not (Array.equal Int32.equal emulator_regs expected_regs)
  then raise_s [%message "CSR emulator result mismatch" (emulator_regs : int32 array)];
  Stdio.print_string "CSR program: all modes good\n"
;;
