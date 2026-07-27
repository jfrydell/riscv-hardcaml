(** Utilities for representing and applying CSR updates for traps and returns. *)

open! Core
open Hardcaml
open Signal

(** Information needed to update CSRs for a trap or trap return. *)
module Update = struct
  type 'a t =
    { epc : 'a [@bits 32]
    ; trap_value : 'a [@bits 32]
    ; cause : 'a [@bits 32]
    ; trap : 'a
    ; ret : 'a
    ; higher_priv_s : 'a (** The trap targets S mode, or the return is an SRET. *)
    }
  [@@deriving hardcaml]
end

(** Apply CSR updates for a trap or trap return. *)
let update ~(update : _ Update.t) ~(old_values : _ Csrs.t) =
  let old_mstatus = Csrs.Mstatus.Fields.of_register old_values.mstatus in
  let higher_priv_s = update.higher_priv_s in
  let trap_mstatus : _ Csrs.Mstatus.Fields.t =
    { old_mstatus with
      sie = mux2 higher_priv_s gnd old_mstatus.sie
    ; mie = mux2 higher_priv_s old_mstatus.mie gnd
    ; spie = mux2 higher_priv_s old_mstatus.sie old_mstatus.spie
    ; mpie = mux2 higher_priv_s old_mstatus.mpie old_mstatus.mie
    ; spp = mux2 higher_priv_s old_values.privilege.:(0) old_mstatus.spp
    ; mpp = mux2 higher_priv_s old_mstatus.mpp old_values.privilege.:[1, 0]
    }
  in
  let m_return_mprv = mux2 (old_mstatus.mpp ==:. 3) old_mstatus.mprv gnd in
  let return_mstatus : _ Csrs.Mstatus.Fields.t =
    { old_mstatus with
      sie = mux2 higher_priv_s old_mstatus.spie old_mstatus.sie
    ; mie = mux2 higher_priv_s old_mstatus.mie old_mstatus.mpie
    ; spie = mux2 higher_priv_s vdd old_mstatus.spie
    ; mpie = mux2 higher_priv_s old_mstatus.mpie vdd
    ; spp = mux2 higher_priv_s gnd old_mstatus.spp
    ; mpp = mux2 higher_priv_s old_mstatus.mpp (zero 2)
    ; mprv = mux2 higher_priv_s gnd m_return_mprv
    }
  in
  let new_mstatus =
    Csrs.Mstatus.Fields.Of_signal.mux2 update.trap trap_mstatus return_mstatus
    |> Csrs.Mstatus.Fields.to_register
  in
  let trap_privilege = mux2 higher_priv_s (of_unsigned_int ~width:2 1) (ones 2) in
  let return_privilege = mux2 higher_priv_s (gnd @: old_mstatus.spp) old_mstatus.mpp in
  let new_privilege =
    mux2 update.trap trap_privilege return_privilege |> uresize ~width:32
  in
  let update_enabled = update.trap ||: update.ret in
  let trap_to_s = update.trap &&: higher_priv_s in
  let trap_to_m = update.trap &&: ~:higher_priv_s in
  let mstatus = mux2 update_enabled new_mstatus old_values.mstatus in
  { old_values with
    mstatus
  ; sepc = mux2 trap_to_s (update.epc &: ones 30 @: zero 2) old_values.sepc
  ; scause = mux2 trap_to_s update.cause old_values.scause
  ; stval = mux2 trap_to_s update.trap_value old_values.stval
  ; mepc = mux2 trap_to_m (update.epc &: ones 30 @: zero 2) old_values.mepc
  ; mcause = mux2 trap_to_m update.cause old_values.mcause
  ; mtval = mux2 trap_to_m update.trap_value old_values.mtval
  ; privilege = mux2 update_enabled new_privilege old_values.privilege
  }
;;
