open! Base
open Hardcaml

(* let circuit =
  let open Riscvhardcaml in
  Circuit.create_with_interface (module Cpu.I) (module Cpu.O) ~name:"cpu" Cpu.cpu

let () = Rtl.print Verilog circuit *)

(* Debug with waveform *)
let program = Riscvemulate.[
  AuiPc {rd = 6; imm = Int32.of_int_trunc (-3731456)};
  Load (Byte, Unsigned, {rd = 3; rs1 = 2; imm = Int32.of_int_trunc 1021});
  IntImm (Add, {rd = 1; rs1 = 5; imm = Int32.of_int_trunc (-102)});
  Branch (Eq, {rs1 = 6; rs2 = 3; imm = Int32.of_int_trunc (-1328)});
  Store (Half, {rs1 = 3; rs2 = 4; imm = Int32.of_int_trunc 1016});
  Branch (Ge Signed, {rs1 = 5; rs2 = 0; imm = Int32.of_int_trunc (-88)});
  IntImm (Add, {rd = 2; rs1 = 6; imm = Int32.of_int_trunc (-222)});
  IntReg (And, {rd = 6; rs1 = 7; rs2 = 6});
  nop;
  nop;
  nop;
  nop;
]

let mem = (Riscvemulate.init ~insns:program ~addr:Int32.zero).memory
let sim = Sim.Cpu.create ~config:(Cyclesim.Config.trace `Everything) ()
let waves, sim = Hardcaml_waveterm.Waveform.create sim
let _ = for _ = 0 to 10 do
  Sim.Cpu.cycle sim mem;
  (* Print outputs *)
  Stdio.print_s (
    Riscvhardcaml.Cpu.O.sexp_of_t (fun b -> sexp_of_int (Bits.to_int !b)) (Cyclesim.outputs sim)
  );
  (* Print reg *)
  Stdio.printf "%d\n" (Int32.to_int_exn (Sim.Cpu.regs sim).(3))
done
(* let _ = Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int mem)
let _ = Stdio.printf "mem b0: %d\n" (Riscvemulate.load ~memory:mem ~addr:Int32.zero ~size:2 ~extend:Riscvemulate.Unsigned |> Int32.to_int_exn) *)
let term = true
let _ = if term then Hardcaml_waveterm_interactive.run waves
