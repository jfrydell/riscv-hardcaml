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
    ; valid : 'a [@rtlname "update_valid"]
    (** True when a write is occurring, and used to time trap completion. The [enable]
        mask is used for actually enabling writes. *)
    }
  [@@deriving hardcaml]
end

(** Decode a CSR instruction into an [Update] to be passed to CSRs. *)
module Decode = struct
  module I = struct
    type 'a t =
      { insn : 'a [@bits 32]
      ; rs1 : 'a [@bits 32]
      ; mideleg : 'a [@bits 32]
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O = Update

  let create scope ({ insn; rs1; mideleg; valid } : _ I.t) =
    let%hw address = insn.:[31, 20] in
    let%hw is_sstatus = address ==:. Csrs.addresses.sstatus in
    let%hw is_sie = address ==:. Csrs.addresses.sie in
    let%hw write_address =
      mux2 is_sstatus (of_unsigned_int ~width:12 Csrs.addresses.mstatus)
      @@ mux2 is_sie (of_unsigned_int ~width:12 Csrs.addresses.mie)
      @@ address
    in
    let%hw funct3 = insn.:[14, 12] in
    let%hw operand = mux2 funct3.:(2) (uresize ~width:32 insn.:[19, 15]) rs1 in
    let%hw operation = funct3.:[1, 0] in
    let%hw op_overwrite = operation ==:. 1 in
    let%hw op_set = operation ==:. 2 in
    let%hw op_clear = operation ==:. 3 in
    let%hw visible_mask =
      mux2 is_sstatus Csrs.Mstatus.sstatus_mask
      @@ mux2 is_sie (mideleg &: Csrs.Interrupt.implemented_mask) (ones 32)
    in
    ({ value = mux2 op_set (ones 32) (mux2 op_clear (zero 32) operand)
     ; mask = mux2 op_overwrite (ones 32) operand &: visible_mask
     ; enable =
         Csrs.map Csrs.addresses ~f:(fun addr -> valid &&: (write_address ==:. addr))
     ; valid
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
  let unchanged ~old_value ~new_value:_ = old_value in
  let align_to ~lsbs ~old_value:_ ~new_value =
    new_value &: ones (32 - lsbs) @: zero lsbs
  in
  let legalize_mstatus ~old_value:_ ~new_value =
    let fields = Csrs.Mstatus.Fields.of_register new_value in
    let mpp = mux2 (fields.mpp ==:. 2) (ones 2) fields.mpp in
    Csrs.Mstatus.Fields.to_register { fields with mpp }
  in
  let legalize_tvec ~old_value:_ ~new_value =
    let mode = new_value.:[1, 0] in
    let legal_mode = mux2 (mode <=:. 1) mode (zero 2) in
    new_value.:[31, 2] @: legal_mode
  in
  let medeleg_mask =
    (* Illegal instruction, breakpoint, and U/S environment calls are the
       implemented synchronous exceptions which can originate below M. *)
    of_unsigned_int ~width:32 ((1 lsl 2) lor (1 lsl 3) lor (1 lsl 8) lor (1 lsl 9))
  in
  (* TODO: restrict which interrupts can be delegated from M (e.g., MEI shouldn't be able to, I believe) *)
  let update_funs : _ Csrs.t =
    { sstatus = unchanged
    ; sie = unchanged
    ; stvec = legalize_tvec
    ; sscratch = all_legal
    ; sepc = align_to ~lsbs:2
    ; scause = all_legal
    ; stval = all_legal
    ; sip = unchanged
    ; satp = all_legal
    ; mstatus = legalize_mstatus
    ; mstatush = (fun ~old_value:_ ~new_value:_ -> zero 32)
    ; medeleg = (fun ~old_value:_ ~new_value -> new_value &: medeleg_mask)
    ; mideleg =
        (fun ~old_value:_ ~new_value -> new_value &: Csrs.Interrupt.implemented_mask)
    ; mie = (fun ~old_value:_ ~new_value -> new_value &: Csrs.Interrupt.implemented_mask)
    ; mtvec = legalize_tvec
    ; mscratch = all_legal
    ; mepc = align_to ~lsbs:2
    ; mcause = all_legal
    ; mtval = all_legal
    ; mip = unchanged
    ; custom0 = all_legal
    ; custom1 = all_legal
    ; custom2 = all_legal
    ; custom3 = all_legal
    ; privilege = unchanged
    }
  in
  Csrs.map3 update_funs update.enable old_values ~f:(fun update_fun enable old_value ->
    let new_value = old_value &: ~:(update.mask) |: (update.value &: update.mask) in
    mux2 enable (update_fun ~old_value ~new_value) old_value)
;;
