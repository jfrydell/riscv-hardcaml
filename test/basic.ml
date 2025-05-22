open! Base

(* Run a basic program *)
let program =
  Riscvemulate.
    [ IntImm (Add, { rd = 1; rs1 = 0; imm = Int32.of_int_exn 7 })
    ; IntReg (Add, { rd = 2; rs1 = 1; rs2 = 1 })
    ]
;;

let emulator = Riscvemulate.init ~insns:program ~addr:Int32.zero
let sim_cpu = Sim.Cpu.create No_waves
let sim_mem = Hashtbl.copy emulator.memory

(* Run for 2 cycles and check regs *)
let regs = Array.create ~len:32 Int32.zero

let _ =
  Sim.Cpu.cycle_insn sim_cpu sim_mem;
  regs.(1) <- Int32.of_int_exn 7
;;

let _ =
  if Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) regs
  then Stdio.print_string "Basic program: cycle 1 good\n"
  else failwith "Basic cycle 1 failed"
;;

let _ =
  Sim.Cpu.cycle_insn sim_cpu sim_mem;
  regs.(2) <- Int32.of_int_exn 14
;;

let _ =
  if Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) regs
  then Stdio.print_string "Basic program: cycle 2 good\n"
  else failwith "Basic cycle 2 failed"
;;
