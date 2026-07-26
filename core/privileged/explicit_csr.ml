(** Utilities for representing and applying CSR updates for explicit CSR accesses. *)

open! Core
open Hardcaml
open Signal

(** Info for a CSR update, to be routed to all CSRs. *)
module Update = struct
  type 'a t =
    { mask : 'a [@bits 32]
    ; value : 'a [@bits 32]
    ; enable : 'a Csrs.Mask.t
    }
  [@@deriving hardcaml]
end

(** Decode a CSR instruction into an [Update] to be passed to CSRs. *)
module Decode = struct
  module I = struct
    type 'a t =
      { insn : 'a [@bits 32]
      ; rs1 : 'a [@bits 32]
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O = Update

  let create scope ({ insn; rs1; valid } : _ I.t) =
    let%hw address = insn.:[31, 20] in
    let%hw funct3 = insn.:[14, 12] in
    let%hw operand = mux2 funct3.:(2) (uresize ~width:32 insn.:[19, 15]) rs1 in
    let%hw operation = funct3.:[1, 0] in
    let%hw op_overwrite = operation ==:. 1 in
    let%hw op_set = operation ==:. 2 in
    let%hw op_clear = operation ==:. 3 in
    ({ value = mux2 op_set (ones 32) (mux2 op_clear (zero 32) operand)
     ; mask = mux2 op_overwrite (ones 32) operand
     ; enable = Csrs.map Csrs.addresses ~f:(fun addr -> valid &&: (address ==:. addr))
     }
     : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical create
  ;;
end

(** Apply CSR writes to registers, respecting any read-only and read-legal requirements. *)
let update ~(update : _ Update.t) ~(old_values : _ Csrs.t) =
  let all_legal ~old_value:_ ~new_value = new_value in
  (* TODO: changes with current privilege level probably? haven't looked at how that works *)
  let update_funs : _ Csrs.t =
    { mstatus =
        (fun ~old_value:_ ~new_value ->
          (* Only illegal value is MPP = 2. *)
          mux2
            (new_value &: Csrs.Mstatus.mpp ==:. 1 lsl 12)
            (new_value |: Csrs.Mstatus.wpri)
            new_value
          &: ~:Csrs.Mstatus.wpri)
    ; mstatush = (fun ~old_value:_ ~new_value:_ -> zero 32)
    ; mepc =
        (fun ~old_value:_ ~new_value ->
          (* Must preserve alignment. *)
          new_value &: ones 30 @: zero 2)
    ; mcause = all_legal
    ; mtval = all_legal
    ; custom0 = all_legal
    ; custom1 = all_legal
    ; custom2 = all_legal
    ; custom3 = all_legal
    }
  in
  Csrs.map3 update_funs update.enable old_values ~f:(fun update_fun enable old_value ->
    let new_value = old_value &: ~:(update.mask) |: (update.value &: update.mask) in
    mux2 enable (update_fun ~old_value ~new_value) old_value)
;;
