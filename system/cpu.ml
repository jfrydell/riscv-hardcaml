(** Code to instantiate the CPU, integrating the core and caches. *)

open! Core
open! Hardcaml

module Cache_config = struct
  (* TODO: actual config, with sizes and (eventually) topology and stuff *)
  type t =
    | L1s
    | L2
end

module type Config = sig
  val caches : Cache_config.t
end

module Make (Config : Config) = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request_interrupt : 'a
      ; from_mem : 'a Memory.Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { to_mem : 'a Memory.Bus.To_mem.t
      ; commit_pc : 'a With_valid.t [@bits 32]
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request_interrupt; from_mem } : _ I.t) =
    (* Exercise translation stalls through both L1 caches until real page-table
       translation is implemented. *)
    let mmu_state =
      { Mmu.State.translation_mode =
          Mmu.State.Translation_mode.Binary.Of_signal.of_enum
            Mmu.State.Translation_mode.Cases.Bare_debug
      ; asid = Signal.zero Mmu.State.asid_width
      ; page_table_root = Signal.zero Mmu.State.addr_width
      }
    in
    (* Instantiate core, with wires for L1 cache outputs. *)
    let%hw.Memory.L1d_cache.To_pipe.Of_signal core_from_l1d =
      Memory.L1d_cache.To_pipe.Of_signal.wires ()
    in
    let%hw.Memory.L1i_cache.To_pipe.Of_signal core_from_l1i =
      Memory.L1i_cache.To_pipe.Of_signal.wires ()
    in
    let%hw.Riscv_core.Cpu.O.Of_signal core =
      Riscv_core.Cpu.hierarchical
        ~scope
        { clocking
        ; from_l1d = core_from_l1d
        ; from_l1i = core_from_l1i
        ; request_interrupt
        }
    in
    (* Instantiate L1 I-cache. *)
    let%hw.Memory.Bus.From_mem.Of_signal l1i_cache_from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.Bus.From_mem.Of_signal l1i_walker_from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.L1i_cache.O.Of_signal l1i =
      Memory.L1i_cache.hierarchical
        ~scope
        { clocking
        ; mmu_state
        ; cache_from_mem = l1i_cache_from_mem
        ; from_pipeline = core.to_l1i
        ; walker_from_mem = l1i_walker_from_mem
        }
    in
    Memory.L1i_cache.To_pipe.Of_signal.assign core_from_l1i l1i.to_pipeline;
    (* Instantiate L1 D-cache. *)
    let%hw.Memory.Bus.From_mem.Of_signal l1d_cache_from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.Bus.From_mem.Of_signal l1d_walker_from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.L1d_cache.O.Of_signal l1d =
      Memory.L1d_cache.hierarchical
        ~scope
        { clocking
        ; mmu_state
        ; cache_from_mem = l1d_cache_from_mem
        ; walker_from_mem = l1d_walker_from_mem
        ; from_pipeline = core.to_l1d
        }
    in
    Memory.L1d_cache.To_pipe.Of_signal.assign core_from_l1d l1d.to_pipeline;
    (* Instantiate L2 cache if necessary, or otherwise connect L1s to memory I/O. *)
    match Config.caches with
    | L1s ->
      let to_mem, responses =
        Memory.Arbiters.hierarchical
          ~scope
          ~clocking
          ~reqs:
            [ l1i.cache_to_mem; l1i.walker_to_mem; l1d.cache_to_mem; l1d.walker_to_mem ]
          ~resp:from_mem
      in
      let l1i_cache, l1i_walker, l1d_cache, l1d_walker =
        match responses with
        | [ l1i_cache; l1i_walker; l1d_cache; l1d_walker ] ->
          l1i_cache, l1i_walker, l1d_cache, l1d_walker
        | _ -> raise_s [%message "arbiter returned unexpected number of response ports"]
      in
      Memory.Bus.From_mem.Of_signal.assign l1i_cache_from_mem l1i_cache;
      Memory.Bus.From_mem.Of_signal.assign l1i_walker_from_mem l1i_walker;
      Memory.Bus.From_mem.Of_signal.assign l1d_cache_from_mem l1d_cache;
      Memory.Bus.From_mem.Of_signal.assign l1d_walker_from_mem l1d_walker;
      ({ to_mem; commit_pc = core.commit_pc } : _ O.t)
    | L2 ->
      let%hw.Memory.Bus.From_mem.Of_signal l2_to_l1 =
        Memory.Bus.From_mem.Of_signal.wires ()
      in
      let l2_from_l1, cache_responses =
        Memory.Arbiters.hierarchical
          ~scope
          ~clocking
          ~reqs:[ l1i.cache_to_mem; l1d.cache_to_mem ]
          ~resp:l2_to_l1
      in
      let l1i_cache, l1d_cache =
        match cache_responses with
        | [ l1i; l1d ] -> l1i, l1d
        | _ -> raise_s [%message "arbiter returned unexpected number of response ports"]
      in
      Memory.Bus.From_mem.Of_signal.assign l1i_cache_from_mem l1i_cache;
      Memory.Bus.From_mem.Of_signal.assign l1d_cache_from_mem l1d_cache;
      let%hw.Memory.Bus.From_mem.Of_signal l2_from_mem =
        Memory.Bus.From_mem.Of_signal.wires ()
      in
      let%hw.Memory.L2_cache.O.Of_signal l2 =
        Memory.L2_cache.hierarchical
          ~scope
          { clocking; from_l1 = l2_from_l1; from_mem = l2_from_mem }
      in
      Memory.Bus.From_mem.Of_signal.assign l2_to_l1 l2.to_l1;
      let to_mem, memory_responses =
        Memory.Arbiters.hierarchical
          ~scope
          ~clocking
          ~reqs:[ l2.to_mem; l1i.walker_to_mem; l1d.walker_to_mem ]
          ~resp:from_mem
      in
      let l2_response, l1i_walker, l1d_walker =
        match memory_responses with
        | [ l2; l1i; l1d ] -> l2, l1i, l1d
        | _ -> raise_s [%message "arbiter returned unexpected number of response ports"]
      in
      Memory.Bus.From_mem.Of_signal.assign l2_from_mem l2_response;
      Memory.Bus.From_mem.Of_signal.assign l1i_walker_from_mem l1i_walker;
      Memory.Bus.From_mem.Of_signal.assign l1d_walker_from_mem l1d_walker;
      ({ to_mem; commit_pc = core.commit_pc } : _ O.t)
  ;;
end
