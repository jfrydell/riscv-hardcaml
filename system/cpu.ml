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
  (** Number of cache block read, write-through, and write-back interfaces needed at the
      top level. *)
  let cache_rds, cache_wts, cache_wbs =
    match Config.caches with
    | L1s -> 2, 1, 0
    | L2 -> 1, 0, 1
  ;;

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; rd_from_mem : 'a Memory.Iface.Read_block.From_mem.t list [@length cache_rds]
      ; wt_from_mem : 'a Memory.Iface.Write_through.From_mem.t list [@length cache_wts]
      ; wb_from_mem : 'a Memory.Iface.Write_back.From_mem.t list [@length cache_wbs]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { rd_to_mem : 'a Memory.Iface.Read_block.To_mem.t list [@length cache_rds]
      ; wt_to_mem : 'a Memory.Iface.Write_through.To_mem.t list [@length cache_wts]
      ; wb_to_mem : 'a Memory.Iface.Write_back.To_mem.t list [@length cache_wbs]
      ; commit_pc : 'a With_valid.t [@bits 32]
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; rd_from_mem; wt_from_mem; wb_from_mem } : _ I.t) =
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
        { clocking; from_l1d = core_from_l1d; from_l1i = core_from_l1i }
    in
    (* Instantiate L1 I-cache. *)
    let%hw.Memory.Iface.Read_block.From_mem.Of_signal l1i_read_from_mem =
      Memory.Iface.Read_block.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.L1i_cache.O.Of_signal l1i =
      Memory.L1i_cache.hierarchical
        ~scope
        { clocking; read_from_mem = l1i_read_from_mem; from_pipeline = core.to_l1i }
    in
    Memory.L1i_cache.To_pipe.Of_signal.assign core_from_l1i l1i.to_pipeline;
    (* Instantiate L1 D-cache. *)
    let%hw.Memory.Iface.Read_block.From_mem.Of_signal l1d_read_from_mem =
      Memory.Iface.Read_block.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.Iface.Write_through.From_mem.Of_signal l1d_write_from_mem =
      Memory.Iface.Write_through.From_mem.Of_signal.wires ()
    in
    let%hw.Memory.L1d_cache.O.Of_signal l1d =
      Memory.L1d_cache.hierarchical
        ~scope
        { clocking
        ; read_from_mem = l1d_read_from_mem
        ; write_from_mem = l1d_write_from_mem
        ; from_pipeline = core.to_l1d
        }
    in
    Memory.L1d_cache.To_pipe.Of_signal.assign core_from_l1d l1d.to_pipeline;
    (* Instantiate L2 cache if necessary, or otherwise connect L1s to memory I/O. *)
    match Config.caches with
    | L1s ->
      List.iter2_exn
        [ l1i_read_from_mem; l1d_read_from_mem ]
        rd_from_mem
        ~f:Memory.Iface.Read_block.From_mem.Of_signal.assign;
      Memory.Iface.Write_through.From_mem.Of_signal.assign
        l1d_write_from_mem
        (List.hd_exn wt_from_mem);
      ({ rd_to_mem = [ l1i.read_to_mem; l1d.read_to_mem ]
       ; wt_to_mem = [ l1d.write_to_mem ]
       ; wb_to_mem = []
       ; commit_pc = core.commit_pc
       }
       : _ O.t)
    | L2 ->
      (* Create arbiter for multiple L1 reads. *)
      let%hw.Memory.Iface.Read_block.From_mem.Of_signal l2_read_to_arb =
        Memory.Iface.Read_block.From_mem.Of_signal.wires ()
      in
      let read_to_l2, reads_from_l2 =
        Memory.Arbiters.arb_rd
          ~scope
          ~clocking
          ~reqs:[ l1i.read_to_mem; l1d.read_to_mem ]
          ~resp:l2_read_to_arb
      in
      List.iter2_exn
        [ l1i_read_from_mem; l1d_read_from_mem ]
        reads_from_l2
        ~f:Memory.Iface.Read_block.From_mem.Of_signal.assign;
      (* Instantiate L2. *)
      let%hw.Memory.L2_cache.O.Of_signal l2 =
        Memory.L2_cache.hierarchical
          ~scope
          { clocking
          ; read_from_l1 = read_to_l2
          ; write_from_l1 = l1d.write_to_mem
          ; read_from_mem = List.hd_exn rd_from_mem
          ; write_from_mem = List.hd_exn wb_from_mem
          }
      in
      Memory.Iface.Read_block.From_mem.Of_signal.assign l2_read_to_arb l2.read_to_l1;
      (* Writes connect directly. *)
      Memory.Iface.Write_through.From_mem.Of_signal.assign
        l1d_write_from_mem
        l2.write_to_l1;
      ({ rd_to_mem = [ l2.read_to_mem ]
       ; wt_to_mem = []
       ; wb_to_mem = [ l2.write_to_mem ]
       ; commit_pc = core.commit_pc
       }
       : _ O.t)
  ;;
end
