open! Core
open Hardcaml
open Hardcaml_waveterm

module Cpu = Riscv_system.Cpu.Make (struct
    let caches = Riscv_system.Cpu.Cache_config.L2
  end)

module Sim = Cyclesim.With_interface (Cpu.I) (Cpu.O)

module L2_cache_state = struct
  type t =
    { tags : Cyclesim.Memory.t
    ; data : Cyclesim.Memory.t
    ; dirty : Cyclesim.Memory.t
    }
end

let find_l2_cache_state sim =
  Option.both
    (Option.both
       (Cyclesim.lookup_mem_by_name sim "l2_tags")
       (Cyclesim.lookup_mem_by_name sim "l2_data"))
    (Cyclesim.lookup_mem_by_name sim "l2_dirty")
  |> Option.map ~f:(fun ((tags, data), dirty) -> { L2_cache_state.tags; data; dirty })
;;

let write_word_to_memory ~memory ~addr bits =
  let data = Bits.to_int64_trunc bits in
  for byte = 0 to (Memory.Iface.cpu_bus_width / 8) - 1 do
    let shift = 8 * byte in
    let value = Int64.(to_int_exn ((data lsr shift) land 0xffL)) in
    Hashtbl.set memory ~key:Int32.(addr + of_int_exn byte) ~data:value
  done
;;

let effective_memory ~backing_memory ~l2_cache_state =
  let memory = Hashtbl.copy backing_memory in
  Option.iter l2_cache_state ~f:(fun { L2_cache_state.tags; data; dirty } ->
    let tags = Cyclesim.Memory.read_all tags
    and data = Cyclesim.Memory.read_all data
    and dirty = Cyclesim.Memory.read_all dirty in
    let bits_block_offset = Memory.L2_cache.bits_block_offset
    and bits_index = Memory.L2_cache.bits_index
    and words_per_block = Memory.L2_cache.words_per_block
    and bits_word_offset = Memory.L2_cache.bits_word_offset in
    Array.iteri tags ~f:(fun index metadata ->
      if Bits.to_bool (Bits.select metadata ~high:0 ~low:0)
      then (
        let tag = Bits.drop_bottom ~width:1 metadata |> Bits.to_int32_trunc in
        for word = 0 to words_per_block - 1 do
          let data_index = (index * words_per_block) + word in
          if Bits.to_bool dirty.(data_index)
          then (
            let addr =
              Stdlib.Int32.logor
                (Stdlib.Int32.shift_left tag (bits_index + bits_block_offset))
                (Stdlib.Int32.logor
                   (Stdlib.Int32.shift_left (Int32.of_int_exn index) bits_block_offset)
                   (Stdlib.Int32.shift_left (Int32.of_int_exn word) bits_word_offset))
            in
            write_word_to_memory ~memory ~addr data.(data_index))
        done)));
  memory
;;

type t =
  { sim : Sim.t
  ; backing_memory : int Int32.Table.t
  ; l2_cache_state : L2_cache_state.t option
  ; fill_addr : int32 option ref
  ; writeback_stall_counter : int ref
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
  let sim = Sim.create ~config (Cpu.create scope) in
  let backing_memory = Hashtbl.copy memory in
  let l2_cache_state = find_l2_cache_state sim in
  Cyclesim.cycle_check sim;
  Cyclesim.cycle_before_clock_edge sim;
  match waves with
  | No_waves ->
    { sim
    ; backing_memory
    ; l2_cache_state
    ; fill_addr = ref None
    ; writeback_stall_counter = ref 7
    }
  | Waves ->
    let waves, sim = Waveform.create sim in
    ( { sim
      ; backing_memory
      ; l2_cache_state
      ; fill_addr = ref None
      ; writeback_stall_counter = ref 7
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
    match !fill_addr with
    | None ->
      (* L2 cache expects 1 cycle latency between request and response. *)
      fill_addr := Some Int32.(request_addr land block_mask)
    | Some addr ->
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
      fill_addr := if last then None else Some Int32.(addr + word_incr))
;;

let process_writeback_request
  ~memory
  ~(request : _ Memory.Iface.Write_back.To_mem.t)
  ~(response : _ Memory.Iface.Write_back.From_mem.t)
  ~(stall_counter : int ref)
  =
  response.ready := Bits.vdd;
  if Bits.to_bool !(request.write)
  then (
    Int.decr stall_counter;
    if !stall_counter = 0
    then (
      response.ready := Bits.gnd;
      stall_counter := 7)
    else
      write_word_to_memory
        ~memory
        ~addr:(Bits.to_int32_trunc !(request.addr))
        !(request.data))
;;

(* Processes CPU outputs, updating memory and feeding in responses.
Should be called before a `Cyclesim.cycle`. *)
let cycle_external { sim; backing_memory; fill_addr; writeback_stall_counter; _ } =
  let inputs = Cyclesim.inputs sim
  and outputs = Cyclesim.outputs sim in
  process_writeback_request
    ~memory:backing_memory
    ~request:(List.hd_exn outputs.wb_to_mem)
    ~response:(List.hd_exn inputs.wb_from_mem)
    ~stall_counter:writeback_stall_counter;
  process_read_request
    ~memory:backing_memory
    ~request:(List.hd_exn outputs.rd_to_mem)
    ~response:(List.hd_exn inputs.rd_from_mem)
    ~fill_addr
;;

(* Runs simulation until the next instruction commits, throwing an exception if this doesn't occur within 10 cycles.
Runs the given function prior to each cycle (for example, to inject instructions at PC for hacky testing) *)
let cycle_insn ?f:(cycle_fn = fun _ -> ()) t =
  match
    List.range 0 100
    |> List.find ~f:(fun _ ->
      cycle_fn ();
      let committed = Bits.to_bool !((Cyclesim.outputs t.sim).commit_pc.valid) in
      cycle_external t;
      Cyclesim.cycle t.sim;
      committed)
  with
  | Some _cycle -> ()
  | None -> failwith "CPU didn't report instruction commit for 100 cycles"
;;

(* Flushes any instructions still in the CPU without updating state *)
let flush t =
  for _ = 1 to 100 do
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

let memory { backing_memory; l2_cache_state; _ } =
  effective_memory ~backing_memory ~l2_cache_state
;;

let cycle t =
  cycle_external t;
  Cyclesim.cycle t.sim
;;

(* Get the current PC from the simulator *)
let pc { sim; _ } = Bits.to_int32_trunc !((Cyclesim.outputs sim).commit_pc.value)
