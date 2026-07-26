open! Core
open Hardcaml
open Signal

(** The state of the M stage instruction, used to time the triggering of traps correctly.

    If [valid] and not [stall], the instruction in M is completing this cycle. *)
module M_status = struct
  type 'a t =
    { valid : 'a
    ; stall : 'a
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; m_status : 'a M_status.t
    ; pc : 'a [@bits 32] (** PC of the instruction in M. *)
    ; next_pc : 'a [@bits 32] (** PC which will execute after the instruction in M. *)
    ; exception_request : 'a (** The instruction in M has encountered an exception. *)
    ; interrupt_request : 'a (** Requests an interrupt to be triggered. *)
    ; explicit_csr : 'a Explicit_csr.Decode.I.t
    (** The instruction in M is a CSR update, triggering the same trap mechanism to squash
        later instructions, but then resuming execution at the next instruction instead of
        at a trap handler. *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { trap_active : 'a
    (** A trap has been triggered, so all instructions later than that in M should be
        squashed. Held until the exception is complete. *)
    ; squash_M : 'a
    (** The instruction in M itself has encountered an exception, so it should not move on
        to W. *)
    ; handler_pc : 'a With_valid.t [@bits 32]
    (** A new value for PC to begin executing the trap handler. Set on the last cycle
        [trap_active] is high, so the correct PC will be in place as soon as execution
        resumes. *)
    ; csrs : 'a Csrs.t
    }
  [@@deriving hardcaml]
end

let create
  scope
  ({ clocking; m_status; pc; next_pc; exception_request; interrupt_request; explicit_csr } :
    _ I.t)
  : _ O.t
  =
  (* As long as M is not stalled (in which case we must wait for it to complete
     successfully before interrupt or complete with an exception), we can
     trigger a trap. *)
  let%hw trap_started = wire 1 in
  let%hw trap_allowed = ~:trap_started &&: m_status.valid &&: ~:(m_status.stall) in
  let%hw start_trap =
    trap_allowed &&: (exception_request ||: interrupt_request ||: explicit_csr.valid)
  in
  (* TODO: not sure on priority if multiple occur on same cycle. CSR must come
     before interrupt (as read will commit), but we could accept the interrupt
     and squash_M and it would be fine. if CSR can have exception, should handle
     exception first. other than that, doesn't matter (so if squash_M on CSR +
     interrupt, then we could but interrupt first) *)
  let%hw _start_exception = trap_allowed &&: exception_request in
  let%hw start_explicit_csr_update =
    trap_allowed &&: ~:exception_request &&: explicit_csr.valid
  in
  let%hw _start_interrupt =
    trap_allowed &&: ~:(exception_request ||: explicit_csr.valid) &&: ~:interrupt_request
  in
  (* If the instruction in M has an exception, it cannot be allowed to
     complete, and our EPC is that instruction. Otherwise, it is allowed to
     proceed, and EPC is the next instruction after the last one to finish M
     (including the one currently completing). *)
  let%hw squash_M = exception_request in
  let%hw next_pc_latched =
    Types.Clocking.cut_through_reg
      clocking
      ~enable:(m_status.valid &&: ~:(m_status.stall))
      next_pc
  in
  let%hw epc =
    Types.Clocking.reg clocking ~enable:start_trap (mux2 squash_M pc next_pc_latched)
  in
  (* Update CSRs when trap begins. *)
  let%hw.Csr_bank.O.Of_signal csr_bank =
    Csr_bank.hierarchical
      ~scope
      { clocking
      ; explicit_write = { explicit_csr with valid = start_explicit_csr_update }
      ; trap_write = Trap_csr.Decode.I.Of_signal.zero ()
      }
  in
  (* The trap is finished once the CSR update goes through. *)
  trap_started <-- Utils.sr ~set:start_trap ~reset:csr_bank.write_done clocking;
  ({ trap_active = start_trap ||: trap_started
   ; squash_M
   ; handler_pc = { value = epc; valid = csr_bank.write_done }
   ; csrs = csr_bank.csrs
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
