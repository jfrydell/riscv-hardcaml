open! Base
open Hardcaml

(* let circuit =
  let open Riscvhardcaml in
  Circuit.create_with_interface (module Cpu.I) (module Cpu.O) ~name:"cpu" Cpu.cpu

let () = Rtl.print Verilog circuit *)

(* Debug with waveform *)
let program = Riscvemulate.[
  IntImm (Sltu, {rd = 5; rs1 = 3; imm = Int32.of_int_exn 173});
  Jalr {rd = 1; rs1 = 1; imm = Int32.of_int_exn 8};
  nop;
  Jalr {rd = 2; rs1 = 0; imm = Int32.of_int_trunc 16};
  nop;
  nop;
  nop;
  nop;
]

let mem = (Riscvemulate.init ~insns:program ~addr:Int32.zero).memory
let sim = Sim.Cpu.create ~config:(Cyclesim.Config.trace `Everything) ()
let waves, sim = Hardcaml_waveterm.Waveform.create sim
let _ = for _ = 1 to 10 do
  Sim.Cpu.cycle sim mem;
  (* Print outputs *)
  Stdio.print_s (
    Riscvhardcaml.Cpu.O.sexp_of_t (fun b -> sexp_of_int (Bits.to_int !b)) (Cyclesim.outputs sim)
  );
  (* Print reg *)
  Stdio.printf "%d\n" (Int32.to_int_exn (Sim.Cpu.regs sim).(5))
done
(* let _ = Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int mem)
let _ = Stdio.printf "mem b0: %d\n" (Riscvemulate.load ~memory:mem ~addr:Int32.zero ~size:2 ~extend:Riscvemulate.Unsigned |> Int32.to_int_exn) *)
let term = true
let _ = if term then Hardcaml_waveterm_interactive.run waves
