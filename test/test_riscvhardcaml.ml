
open! Base
open Hardcaml

(* Try running a program *)
let program = Riscvemulate.[
  IntImm (Add, {rd = 1; rs1 = 0; imm = Int32.of_int_exn 7});
  IntReg (Add, {rd = 2; rs1 = 1; rs2 = 1});
  nop;
  nop;
  nop;
  nop;
  nop;
]

let emulator = Riscvemulate.init ~insns:program ~addr:Int32.zero
let sim_cpu = Sim.Cpu.create ()
let sim_mem = Hashtbl.copy emulator.memory

(* Simulation testing *)
let print_outputs cpu =
  Stdio.print_s (
    Riscvhardcaml.Cpu.O.sexp_of_t (fun b -> sexp_of_int (Bits.to_int !b)) (Cyclesim.outputs cpu)
  )

let _ = for _ = 0 to 10 do
  Sim.Cpu.cycle sim_cpu sim_mem;
  print_outputs sim_cpu;
  Stdio.printf "%d\n" (Int32.to_int_exn (Sim.Cpu.regs sim_cpu).(2))
done
