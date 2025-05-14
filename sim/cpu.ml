open! Base
open Hardcaml

module Sim = Cyclesim.With_interface (Riscvhardcaml.Cpu.I) (Riscvhardcaml.Cpu.O)

(* Create simulation, running combinational logic so outputs are visible (doesn't do anything i think because default is 0) *)
let create () =
  let cpu = Sim.create ~config:(Cyclesim.Config.trace `All_named) Riscvhardcaml.Cpu.cpu in
  Cyclesim.cycle_check cpu;
  Cyclesim.cycle_before_clock_edge cpu;
  cpu

(* Run a cycle of simulation, feeding in correct insn/memory values & updating memory on store before cycling CPU.
Assumes current outputs reflect state of registers, so no need to `cycle_after_clock_edge`. *)
let cycle (cpu: Sim.t) memory =
  let inputs = Cyclesim.inputs cpu
  and outputs = Cyclesim.outputs cpu in

  (* Fetch instruction from memory, or use provided custom `insn` getter *)
  let insn = Riscvemulate.load ~memory ~addr:(Bits.to_int32 !(outputs.pc)) ~size:4 ~extend:Riscvemulate.Unsigned in
  inputs.insn := Bits.of_int32 ~width:32 insn;

  (* Process load *)
  let addr = Bits.to_int32 !(outputs.access.addr)
  and size = match Bits.to_int !(outputs.access.size) with 0 -> 1 | 1 -> 2 | 2 -> 4 | _ -> 0 in
  let data = Riscvemulate.load ~memory ~addr ~size ~extend:Riscvemulate.Unsigned in
  inputs.data := Bits.of_int32 ~width:32 data;

  (* Process store *)
  if Bits.to_bool !(outputs.access.store) && Bits.to_bool !(outputs.access.valid) then
    Riscvemulate.store ~memory ~addr ~size ~value:(Bits.to_int32 !(outputs.access.store_data));

  (* Run standard cycle, propagating inputs, updating regs, and propagating outputs *)
  Cyclesim.cycle cpu

(* Runs simulation until the next instruction commits, throwing an exception if this doesn't occur within 5 cycles. *)
let cycle_insn (cpu: Sim.t) memory =
  List.range 0 5 |> List.find ~f:(fun _ ->
    let committed = Bits.to_bool !((Cyclesim.outputs cpu).will_commit) in
    cycle cpu memory;
    committed
  )

(* Extract all register values from the simulation *)
let regs (cpu: Sim.t) =
  Cyclesim.lookup_mem_by_name cpu "regfile"
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int32
