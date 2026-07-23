open! Base

let basic_test = Test_definitions.Basic.get_exn 0
let program = basic_test.program
let emulator = Riscvemulate.init ~insns:program ~addr:Int32.zero
let sim = Sim.Cpu.create ~memory:(Hashtbl.copy emulator.memory) No_waves

(* Run for 2 cycles and check regs *)
let regs = Array.create ~len:32 Int32.zero

let _ =
  Sim.Cpu.cycle_insn sim;
  regs.(1) <- Int32.of_int_exn 7
;;

let _ =
  if Array.equal Int32.equal (Sim.Cpu.regs sim) regs
  then Stdio.print_string "Basic program: cycle 1 good\n"
  else failwith "Basic cycle 1 failed"
;;

let _ =
  Sim.Cpu.cycle_insn sim;
  regs.(2) <- Int32.of_int_exn 14
;;

let _ =
  if Array.equal Int32.equal (Sim.Cpu.regs sim) regs
  then Stdio.print_string "Basic program: cycle 2 good\n"
  else failwith "Basic cycle 2 failed"
;;

let _ =
  Sim.Cpu.cycle_insn sim;
  Sim.Cpu.cycle_insn sim;
  regs.(3) <- Int32.of_int_exn 7;
  if Array.equal Int32.equal (Sim.Cpu.regs sim) regs
  then Stdio.print_string "Basic program: cycle 3-4 good\n"
  else raise_s [%message "Basic cycle 3-4 failed" (Sim.Cpu.regs sim : int32 array)]
;;
