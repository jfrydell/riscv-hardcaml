open! Core
open Hardcaml
open Hardcaml_waveterm
module Sim = Cyclesim.With_interface (Riscvhardcaml.Cpu.I) (Riscvhardcaml.Cpu.O)

type 'a waves_return =
  | Waves : (Sim.t * Waveform.t) waves_return
  | No_waves : Sim.t waves_return

(* Create simulation, running combinational logic so outputs are visible (doesn't do anything i think because default is 0) *)
let create (type a) ?(config = Cyclesim.Config.trace `All_named) (waves : a waves_return)
  : a
  =
  let scope = Scope.create ~flatten_design:true () in
  let cpu = Sim.create ~config (Riscvhardcaml.Cpu.hierarchical ~scope) in
  Cyclesim.cycle_check cpu;
  Cyclesim.cycle_before_clock_edge cpu;
  match waves with
  | No_waves -> cpu
  | Waves -> Waveform.create cpu |> Tuple2.swap
;;

(* Processes CPU outputs (pc & data access), updating memory and feeding in correct inputs (insn & load data).
Should be called before a `Cyclesim.cycle`. *)
let cycle_external (cpu : Sim.t) memory =
  let inputs = Cyclesim.inputs cpu
  and outputs = Cyclesim.outputs cpu in
  (* Fetch instruction from memory, or use provided custom `insn` getter *)
  let insn =
    Riscvemulate.load
      ~memory
      ~addr:(Bits.to_int32_trunc !(outputs.pc))
      ~size:4
      ~extend:Riscvemulate.Unsigned
  in
  inputs.insn := Bits.of_int32_trunc ~width:32 insn;
  inputs.insn_valid := Bits.vdd;
  (* DEBUG *)
  (* Stdio.printf "sim PC %d = %08x\n" (Bits.to_int !(outputs.pc)) (Bits.to_int !(inputs.insn)); *)

  (* Process load *)
  let addr = Bits.to_int32_trunc !(outputs.access.addr)
  and size =
    match Bits.to_int_trunc !(outputs.access.size) with
    | 0 -> 1
    | 1 -> 2
    | 2 -> 4
    | _ -> 0
  in
  let data = Riscvemulate.load ~memory ~addr ~size ~extend:Riscvemulate.Unsigned in
  inputs.data := Bits.of_int32_trunc ~width:32 data;
  (* Process store *)
  if Bits.to_bool !(outputs.access.store) && Bits.to_bool !(outputs.access.valid)
  then
    Riscvemulate.store
      ~memory
      ~addr
      ~size
      ~value:(Bits.to_int32_trunc !(outputs.access.store_data))
;;

(* Runs simulation until the next instruction commits, throwing an exception if this doesn't occur within 5 cycles.
Runs the given function prior to each cycle (for example, to inject instructions at PC for hacky testing) *)
let cycle_insn ?f:(cycle_fn = fun _ -> ()) (cpu : Sim.t) memory =
  match
    List.range 0 5
    |> List.find ~f:(fun _ ->
      cycle_fn ();
      let committed = Bits.to_bool !((Cyclesim.outputs cpu).will_commit) in
      cycle_external cpu memory;
      Cyclesim.cycle cpu;
      committed)
  with
  | Some _cycle -> ()
  | None -> failwith "CPU didn't report instruction commit for 5 cycles"
;;

(* Flushes any instructions still in the CPU without updating state *)
let flush cpu memory =
  for _ = 1 to 5 do
    cycle_external cpu memory;
    (Cyclesim.inputs cpu).insn_valid := Bits.gnd;
    Cyclesim.cycle cpu
  done
;;

(* Extract all register values from the simulation *)
let regs (cpu : Sim.t) =
  Cyclesim.lookup_mem_by_name cpu "regfile"
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int32_trunc
;;

(* Get the current PC from the simulator *)
let pc (cpu : Sim.t) = Bits.to_int32_trunc !((Cyclesim.outputs cpu).pc)
