open! Core
open Hardcaml
open Hardcaml_waveterm
module Sim = Cyclesim.With_interface (Riscvhardcaml.Cpu.I) (Riscvhardcaml.Cpu.O)

type t =
  { sim : Sim.t
  ; fill_addr : int32 option ref
  ; memory : int Int32.Table.t
  }

type 'a waves_return =
  | Waves : (t * Waveform.t) waves_return
  | No_waves : t waves_return

(* Create simulation, running combinational logic so outputs are visible (doesn't do anything i think because default is 0) *)
let create
  (type a)
  ?(memory = Int32.Table.create ())
  ?(config = Cyclesim.Config.trace `All_named)
  (waves : a waves_return)
  : a
  =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config (Riscvhardcaml.Cpu.hierarchical ~scope) in
  Cyclesim.cycle_check sim;
  Cyclesim.cycle_before_clock_edge sim;
  match waves with
  | No_waves -> { sim; memory; fill_addr = ref None }
  | Waves ->
    let waves, sim = Waveform.create sim in
    { sim; memory; fill_addr = ref None }, waves
;;

(* Processes CPU outputs (pc & data access), updating memory and feeding in correct inputs (insn & load data).
Should be called before a `Cyclesim.cycle`. *)
let cycle_external { sim; memory; fill_addr } =
  let inputs = Cyclesim.inputs sim
  and outputs = Cyclesim.outputs sim in
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

  (* Process store *)
  let addr = Bits.to_int32_trunc !(outputs.to_memory.addr) in
  (* TODO: not always instant store. *)
  inputs.from_memory.store_ready := Bits.vdd;
  if Bits.to_bool !(outputs.to_memory.store)
  then
    Riscvemulate.store
      ~memory
      ~addr
      ~size:(1 lsl Bits.to_int_trunc !(outputs.to_memory.store_size))
      ~value:(Bits.to_int32_trunc !(outputs.to_memory.store_data));
  (* Process load, filling cache block. *)
  let mask =
    (1 lsl Riscvhardcaml.Memory.bits_word_offset) - 1 |> Int32.of_int_exn |> Int32.lnot
  in
  if Bits.to_bool !(outputs.to_memory.load)
  then (
    let addr =
      match !fill_addr with
      | None -> Int32.(addr land mask)
      | Some addr -> Int32.(addr + of_int_exn 1)
    in
    let last = Int32.(mask land addr <> mask land (addr + of_int_exn 1)) in
    inputs.from_memory.addr := Bits.of_int32_trunc ~width:32 addr;
    inputs.from_memory.valid := Bits.vdd;
    inputs.from_memory.last := Bits.of_bool last;
    (inputs.from_memory.data
     := let load_byte addr =
          Hashtbl.find memory addr
          |> Option.value ~default:0
          |> Bits.of_int_trunc ~width:8
        in
        let bytes =
          List.init (Riscvhardcaml.Memory.bus_width / 8) ~f:(fun n ->
            load_byte Int32.(addr + of_int_exn n))
        in
        Bits.concat_lsb bytes);
    fill_addr := if last then None else Some addr)
  else inputs.from_memory.valid := Bits.gnd
;;

(* Runs simulation until the next instruction commits, throwing an exception if this doesn't occur within 5 cycles.
Runs the given function prior to each cycle (for example, to inject instructions at PC for hacky testing) *)
let cycle_insn ?f:(cycle_fn = fun _ -> ()) t =
  match
    List.range 0 5
    |> List.find ~f:(fun _ ->
      cycle_fn ();
      let committed = Bits.to_bool !((Cyclesim.outputs t.sim).will_commit) in
      cycle_external t;
      Cyclesim.cycle t.sim;
      committed)
  with
  | Some _cycle -> ()
  | None -> failwith "CPU didn't report instruction commit for 5 cycles"
;;

(* Flushes any instructions still in the CPU without updating state *)
let flush t =
  for _ = 1 to 5 do
    cycle_external t;
    (Cyclesim.inputs t.sim).insn_valid := Bits.gnd;
    Cyclesim.cycle t.sim
  done
;;

(* Extract all register values from the simulation *)
let regs { sim; _ } =
  Cyclesim.lookup_mem_by_name sim "regfile"
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int32_trunc
;;

(* Get the current PC from the simulator *)
let pc { sim; _ } = Bits.to_int32_trunc !((Cyclesim.outputs sim).pc)
