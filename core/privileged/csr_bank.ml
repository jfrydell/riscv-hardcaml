open! Core
open Hardcaml

(** Number of cycles to pipeline explicit CSR write decode (address match and route to
    CSRs) by. *)
let explicit_latency = 2

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; mip : 'a [@bits 32] (** Live interrupt-pending bits supplied by the platform. *)
    ; explicit_write : 'a Explicit_csr.Decode.I.t
    (** Writes from CSR update instructions. *)
    ; trap_write : 'a Trap_csr.Update.t (** Writes caused by traps and trap returns. *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { csrs : 'a Csrs.t
    ; write_done : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; mip; explicit_write; trap_write } : _ I.t) =
  let spec = Types.Clocking.to_spec clocking in
  let%hw.Csrs.Of_signal csrs = Csrs.Of_signal.wires () in
  (* Process explicit CSR writes. *)
  let%hw.Explicit_csr.Update.Of_signal unbuffered_decoded_explicit =
    Explicit_csr.Decode.hierarchical ~scope explicit_write
  in
  let%hw.Explicit_csr.Update.Of_signal decoded_explicit =
    Explicit_csr.Decode.O.Of_signal.pipeline
      ~n:explicit_latency
      spec
      unbuffered_decoded_explicit
  in
  (* Apply writes. *)
  let%hw.Csrs.Of_signal after_explicit =
    Explicit_csr.update ~update:decoded_explicit ~old_values:csrs
  in
  let%hw.Csrs.Of_signal after_trap =
    Trap_csr.update ~update:trap_write ~old_values:after_explicit
  in
  let clear_values =
    { (Csrs.map Csrs.addresses ~f:(fun _ -> Signal.zero 32)) with
      privilege = Signal.of_int_trunc ~width:32 3
    }
  in
  Csrs.Of_signal.assign
    csrs
    (Csrs.map2 after_trap clear_values ~f:(fun value clear_to ->
       Types.Clocking.reg clocking ~clear_to value));
  let%hw.Csrs.Of_signal visible_csrs =
    { csrs with
      sstatus = Csrs.Mstatus.sstatus_view csrs.mstatus
    ; sie = Csrs.Interrupt.sie_view ~mie:csrs.mie ~mideleg:csrs.mideleg
    ; sip = Csrs.Interrupt.sip_view ~mip ~mideleg:csrs.mideleg
    ; mip
    }
  in
  ({ csrs = visible_csrs
   ; write_done = Signal.(decoded_explicit.valid |: trap_write.trap |: trap_write.ret)
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
