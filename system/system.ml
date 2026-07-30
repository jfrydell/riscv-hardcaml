open! Core
open! Hardcaml

module type Config = sig
  module Cpu : Cpu.Config

  val memory_size : int
end

module Make (Config : Config) = struct
  module Cpu = Cpu.Make (Config.Cpu)

  module Main_memory = Memory.Main_memory_bram.Make (struct
      let size = Config.memory_size
    end)

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request_interrupt : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { commit_pc : 'a With_valid.t [@bits 32] } [@@deriving hardcaml]
  end

  let create scope ({ clocking; request_interrupt } : _ I.t) =
    let%hw.Memory.Bus.From_mem.Of_signal from_mem =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let cpu = Cpu.create scope { clocking; request_interrupt; from_mem } in
    let%hw.Main_memory.O.Of_signal main_memory =
      Main_memory.hierarchical ~scope { clocking; from_cpu = cpu.to_mem }
    in
    Memory.Bus.From_mem.Of_signal.assign from_mem main_memory.to_cpu;
    ({ commit_pc = cpu.commit_pc } : _ O.t)
  ;;
end
