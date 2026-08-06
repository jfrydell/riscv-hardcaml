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

  (** When set, disables address translation, forcing Bare mode. (TODO: reflect in CSR
      behavior) *)
  val disable_address_translation : bool
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
      ; csrs : 'a Privileged.Csrs.t
      ; pipeline_debugging : 'a Riscv_core.Cpu.Pipeline_status.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request_interrupt; from_mem } : _ I.t) : _ O.t =
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
    let mmu_state =
      let mmu_state = Mmu.State.of_csrs core.csrs in
      { mmu_state with
        translation_mode =
          (if Config.disable_address_translation
           then Mmu.State.Translation_mode.Binary.Of_signal.of_enum Bare
           else mmu_state.translation_mode)
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
    (* Arbitrate between L1 memory requests. *)
    let%hw.Memory.Bus.From_mem.Of_signal l1s_from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.Bus.Arbiter.Four.O.Of_signal l1s_arb =
      Memory.Bus.Arbiter.Four.hierarchical
        ~scope
        { clocking
        ; up_req =
            [ l1i.cache_to_mem; l1i.walker_to_mem; l1d.cache_to_mem; l1d.walker_to_mem ]
        ; dn_resp = l1s_from_mem
        }
    in
    List.iter2_exn
      [ l1i_cache_from_mem; l1i_walker_from_mem; l1d_cache_from_mem; l1d_walker_from_mem ]
      l1s_arb.up_resp
      ~f:Memory.Bus.From_mem.Of_signal.assign;
    (* Instantiate L2 cache if necessary, or otherwise connect L1s to memory I/O. *)
    let to_mem =
      match Config.caches with
      | L1s ->
        Memory.Bus.From_mem.Of_signal.assign l1s_from_mem from_mem;
        l1s_arb.dn_req
      | L2 ->
        let%hw.Memory.Bus.From_mem.Of_signal l2_from_mem =
          Memory.Bus.From_mem.Of_signal.wires ()
        in
        let%hw.Memory.L2_cache.O.Of_signal l2 =
          Memory.L2_cache.hierarchical
            ~scope
            { clocking; from_l1 = l1s_arb.dn_req; from_mem = l2_from_mem }
        in
        Memory.Bus.From_mem.Of_signal.assign l1s_from_mem l2.to_l1;
        Memory.Bus.From_mem.Of_signal.assign l2_from_mem from_mem;
        l2.to_mem
    in
    { to_mem
    ; commit_pc = core.commit_pc
    ; csrs = core.csrs
    ; pipeline_debugging = core.pipeline_status
    }
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical create
  ;;
end
