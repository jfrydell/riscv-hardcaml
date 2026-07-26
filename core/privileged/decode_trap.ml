(** Decode trap delegation, interrupt enable, CSR updates, and redirect PCs. *)

open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { epc : 'a [@bits 32]
    ; trap_value : 'a [@bits 32]
    ; exception_cause : 'a [@bits 32]
    ; interrupt_cause : 'a [@bits 4]
    ; csrs : 'a Csrs.t
    ; trap : 'a
    ; interrupt : 'a
    ; mret : 'a
    ; sret : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { update : 'a Trap_csr.Update.t
    ; handler_pc : 'a [@bits 32]
    ; interrupt_enabled : 'a
    }
  [@@deriving hardcaml]
end

let select_delegation_bit delegation cause =
  mux (uresize ~width:5 cause) (List.init 32 ~f:(fun index -> delegation.:(index)))
;;

let create
  scope
  ({ epc
   ; trap_value
   ; exception_cause
   ; interrupt_cause
   ; csrs
   ; trap
   ; interrupt
   ; mret
   ; sret
   } :
    _ I.t)
  : _ O.t
  =
  let%hw cause = mux2 interrupt (vdd @: zero 27 @: interrupt_cause) exception_cause in
  let%hw privilege = csrs.privilege.:[1, 0] in
  let%hw exception_delegated =
    select_delegation_bit csrs.medeleg exception_cause.:[4, 0]
  in
  let%hw interrupt_delegated = select_delegation_bit csrs.mideleg interrupt_cause in
  (* Delegation only applies to traps originating below M. Traps never move to
     a less-privileged mode. *)
  let%hw trap_to_s =
    privilege <>:. 3 &&: mux2 interrupt interrupt_delegated exception_delegated
  in
  let%hw higher_priv_s = mux2 trap trap_to_s sret in
  let%hw.Csrs.Mstatus.Fields.Of_signal mstatus =
    Csrs.Mstatus.Fields.of_register csrs.mstatus
  in
  let%hw enabled_at_target =
    mux2
      interrupt_delegated
      (privilege <>:. 3 &&: (privilege <:. 1 ||: (privilege ==:. 1 &&: mstatus.sie)))
      (privilege <:. 3 ||: (privilege ==:. 3 &&: mstatus.mie))
  in
  let%hw interrupt_enabled =
    select_delegation_bit csrs.mie interrupt_cause &&: enabled_at_target
  in
  let%hw tvec = mux2 trap_to_s csrs.stvec csrs.mtvec in
  let%hw tvec_base = tvec.:[31, 2] @: zero 2 in
  let%hw vector_offset = uresize ~width:32 (uresize ~width:5 interrupt_cause @: zero 2) in
  let%hw use_vectored = interrupt &&: (tvec.:[1, 0] ==:. 1) in
  let%hw trap_handler_pc = mux2 use_vectored (tvec_base +: vector_offset) tvec_base in
  let%hw return_pc = mux2 sret csrs.sepc csrs.mepc in
  let%hw ret = mret ||: sret in
  let%hw handler_pc = mux2 ret return_pc trap_handler_pc in
  { update = { epc; trap_value; cause; trap; ret; higher_priv_s }
  ; handler_pc
  ; interrupt_enabled
  }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
