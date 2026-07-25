open Base
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
    (** A new value for PC to begin executing the trap handler. Guaranteed to be set on
        the last cycle [trap_active] is high, so the correct PC will be in place as soon
        as execution resumes. *)
    }
  [@@deriving hardcaml]
end

let create
  scope
  ({ clocking; m_status; pc; next_pc; exception_request; interrupt_request } : _ I.t)
  : _ O.t
  =
  (* Traps are always raised as an instruction finishes the M stage. We can't
     raise earlier because page faults are detected in M, but can't wait until
     commit because stores write to the cache in M. Because both of these
     happen in the same stage, but one requires the trap to occur before the
     instruction is executed, and the other requires the instruction to
     complete (since it's already written to memory) before the trap, we
     distinguish two types of traps:
    - Exceptions on an instruction in M don't commit that instruction, setting epc to that PC.
    - Interrupts allow the instruction in M to proceed to W (waiting if it is stalled), and point epc to the following instruction. *)
  (* As long as M is not stalled (in which case we must wait for it to complete
     successfully before interrupt or complete with an exception), we can
     trigger a trap. *)
  let%hw trap_started = wire 1 in
  let%hw trap_allowed = ~:(m_status.stall) in
  let%hw start_trap =
    ~:trap_started &&: trap_allowed &&: (exception_request ||: interrupt_request)
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
  let%hw _epc =
    Types.Clocking.reg clocking ~enable:start_trap (mux2 squash_M pc next_pc_latched)
  in
  let%hw trap_finished = gnd in
  trap_started <-- Utils.sr ~set:trap_started ~reset:trap_finished clocking;
  ({ trap_active = start_trap ||: trap_started
   ; squash_M = gnd
   ; handler_pc = { value = zero 32; valid = gnd }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
