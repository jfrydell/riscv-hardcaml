open! Core
open Hardcaml
open Hardcaml_waveterm
module Sim = Cyclesim.With_interface (Riscv_core.Cpu.I) (Riscv_core.Cpu.O)

type t =
  { sim : Sim.t
  ; memory : int Int32.Table.t
  ; insn_fill_addr : int32 option ref
  ; data_fill_addr : int32 option ref
  ; store_stall_counter : int ref
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
  let sim = Sim.create ~config (Riscv_core.Cpu.hierarchical ~scope) in
  Cyclesim.cycle_check sim;
  Cyclesim.cycle_before_clock_edge sim;
  match waves with
  | No_waves ->
    { sim
    ; memory
    ; insn_fill_addr = ref None
    ; data_fill_addr = ref None
    ; store_stall_counter = ref 7
    }
  | Waves ->
    let waves, sim = Waveform.create sim in
    ( { sim
      ; memory
      ; insn_fill_addr = ref None
      ; data_fill_addr = ref None
      ; store_stall_counter = ref 7
      }
    , waves )
;;

(** Process a read request from memory, streaming a cache block back.

    Takes in request/response interface and address tracking state of currently-streaming
    block. *)
let process_read_request
  ~memory
  ~(request : _ Memory.Iface.Read_block.To_mem.t)
  ~(response : _ Memory.Iface.Read_block.From_mem.t)
  ~(fill_addr : int32 option ref)
  =
  let word_incr = Memory.Iface.cpu_bus_width / 8 |> Int32.of_int_exn in
  let block_mask =
    (Memory.Iface.block_size_bits / 8) - 1 |> Int32.of_int_exn |> Int32.lnot
  in
  response.valid := Bits.gnd;
  if Bits.to_bool !(request.load)
  then (
    let request_addr = Bits.to_int32_trunc !(request.addr) in
    let addr =
      match !fill_addr with
      | None -> Int32.(request_addr land block_mask)
      | Some addr -> Int32.(addr + word_incr)
    in
    let last = Int32.(block_mask land addr <> block_mask land (addr + word_incr)) in
    response.addr := Bits.of_int32_trunc ~width:Memory.Iface.addr_width addr;
    response.valid := Bits.vdd;
    response.last := Bits.of_bool last;
    (response.data
     := let load_byte addr =
          Hashtbl.find memory addr
          |> Option.value ~default:0
          |> Bits.of_int_trunc ~width:8
        in
        let bytes =
          List.init (Memory.Iface.cpu_bus_width / 8) ~f:(fun n ->
            load_byte Int32.(addr + of_int_exn n))
        in
        Bits.concat_lsb bytes);
    fill_addr := if last then None else Some addr)
;;

(* Processes CPU outputs, updating memory and feeding in responses.
Should be called before a `Cyclesim.cycle`. *)
let cycle_external { sim; memory; insn_fill_addr; data_fill_addr; store_stall_counter } =
  let inputs = Cyclesim.inputs sim
  and outputs = Cyclesim.outputs sim in
  (* Process store *)
  inputs.write_from_data_mem.store_ready := Bits.vdd;
  if Bits.to_bool !(outputs.write_to_data_mem.store)
  then (
    Int.decr store_stall_counter;
    if !store_stall_counter = 0
    then (
      inputs.write_from_data_mem.store_ready := Bits.gnd;
      store_stall_counter := 7)
    else
      Riscvemulate.store
        ~memory
        ~addr:(Bits.to_int32_trunc !(outputs.write_to_data_mem.addr))
        ~size:(1 lsl Bits.to_int_trunc !(outputs.write_to_data_mem.store_size))
        ~value:(Bits.to_int32_trunc !(outputs.write_to_data_mem.store_data)));
  process_read_request
    ~memory
    ~request:outputs.read_to_insn_mem
    ~response:inputs.read_from_insn_mem
    ~fill_addr:insn_fill_addr;
  process_read_request
    ~memory
    ~request:outputs.read_to_data_mem
    ~response:inputs.read_from_data_mem
    ~fill_addr:data_fill_addr
;;

(* Runs simulation until the next instruction commits, throwing an exception if this doesn't occur within 10 cycles.
Runs the given function prior to each cycle (for example, to inject instructions at PC for hacky testing) *)
let cycle_insn ?f:(cycle_fn = fun _ -> ()) t =
  match
    List.range 0 25
    |> List.find ~f:(fun _ ->
      cycle_fn ();
      let committed = Bits.to_bool !((Cyclesim.outputs t.sim).commit_pc.valid) in
      cycle_external t;
      Cyclesim.cycle t.sim;
      committed)
  with
  | Some _cycle -> ()
  | None -> failwith "CPU didn't report instruction commit for 25 cycles"
;;

(* Flushes any instructions still in the CPU without updating state *)
let flush t =
  for _ = 1 to 5 do
    cycle_external t;
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

let commit_pc { sim; _ } =
  let outputs = Cyclesim.outputs sim in
  if Bits.to_bool !(outputs.commit_pc.valid)
  then Some (Bits.to_int32_trunc !(outputs.commit_pc.value))
  else None
;;

let memory { memory; _ } = memory

let cycle t =
  cycle_external t;
  Cyclesim.cycle t.sim
;;

(* Get the current PC from the simulator *)
let pc { sim; _ } = Bits.to_int32_trunc !((Cyclesim.outputs sim).commit_pc.value)
