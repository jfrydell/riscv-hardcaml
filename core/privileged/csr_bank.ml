open! Core
open Hardcaml
open Signal

let csr_latency = 2

module Write = struct
  type 'a t =
    { value : 'a [@bits 32]
    ; mask : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module Writes = Csrs.Wrap (Write)

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; writes : 'a Writes.t
    }
  [@@deriving hardcaml]
end

module O = Csrs.Values

let create scope ({ clocking; writes } : _ I.t) =
  let%hw.Writes.Of_signal buffered_writes =
    Writes.map writes ~f:(Types.Clocking.pipeline ~n:csr_latency clocking)
  in
  (* Define how each CSR is updated so as to always hold legal values. *)
  let all_legal ~old_value:_ ~new_value = new_value in
  let update_rules : _ Csrs.t =
    { custom0 = all_legal; custom1 = all_legal; custom2 = all_legal; custom3 = all_legal }
  in
  Csrs.map2
    ~f:(fun update ({ value; mask } : _ Write.t) ->
      Types.Clocking.reg_fb clocking ~width:32 ~f:(fun old_value ->
        let new_value = old_value &: ~:mask |: (value &: mask) in
        mux2 (mask <>:. 0) (update ~old_value ~new_value) old_value))
    update_rules
    buffered_writes
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
