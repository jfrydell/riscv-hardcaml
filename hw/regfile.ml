(* Simple 32-register (1 zero), 2 read port, 1 write port register file.
Written on rising edge (intended to be aligned with pipeline latches for easier simulation),
writes are forwarded to read to give write-before-read semantics anyway. *)

open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; rd : 'a [@bits 5]
    ; rdval : 'a [@bits 32]
    ; rs : 'a array [@bits 5] [@length 2]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { rsval : 'a array [@bits 32] [@length 2] } [@@deriving hardcaml]
end

(* TODO: reset? need to replace `multiport_memory` with set of registers *)
let create scope ({ clocking; rd; rdval; rs } : _ I.t) =
  let write_port =
    Write_port.
      { write_clock = clocking.clock
      ; write_address = rd
      ; write_data = rdval
      ; write_enable = rd <>: zero 5
      }
  in
  let%hw_array rsval_raw =
    multiport_memory ~name:"regfile" ~write_ports:[| write_port |] ~read_addresses:rs 32
  in
  let%hw_array rsval_fwd =
    Array.map2_exn rs rsval_raw ~f:(fun rs regval ->
      mux2 (rs ==: rd &: (rd <>: zero 5)) rdval regval)
  in
  O.{ rsval = rsval_fwd }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
