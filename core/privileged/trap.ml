open! Core
open Hardcaml
open Signal

(** The state of the M stage instruction, used to time the triggering of traps correctly.

    If [valid] and not [stall], the instruction in M is completing this cycle. *)
module M_stage_state = struct
  type 'a t =
    { valid : 'a
    ; stall : 'a
    }
  [@@deriving hardcaml]
end

module Action = struct
  type 'a t =
    { exception_ : 'a
    ; explicit_csr : 'a
    ; mret : 'a
    ; sret : 'a
    ; interrupt : 'a
    }
  [@@deriving hardcaml]

  (** Select at most one action, in synchronous instruction order followed by an interrupt
      at the resulting instruction boundary. *)
  let prioritize possible =
    let exception_ = possible.exception_ in
    let explicit_csr = possible.explicit_csr &&: ~:exception_ in
    let mret = possible.mret &&: ~:(exception_ ||: explicit_csr) in
    let sret = possible.sret &&: ~:(exception_ ||: explicit_csr ||: mret) in
    let interrupt =
      possible.interrupt &&: ~:(exception_ ||: explicit_csr ||: mret ||: sret)
    in
    { exception_; explicit_csr; mret; sret; interrupt }
  ;;

  let any t = reduce ~f:( |: ) (to_list t)
  let is_trap t = t.exception_ ||: t.interrupt
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; m_stage : 'a M_stage_state.t
    ; pc : 'a [@bits 32] (** PC of the instruction in M. *)
    ; next_pc : 'a [@bits 32] (** PC which will execute after the instruction in M. *)
    ; detect_exception : 'a Detect_exception.I.t
    ; interrupt_request : 'a With_valid.t [@bits 4]
    (** Requests an interrupt with the given number (11 = machine external interrupt is
        the only one we support right now). *)
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
  ({ clocking; m_stage; pc; next_pc; detect_exception; interrupt_request } : _ I.t)
  : _ O.t
  =
  (* As long as M is not stalled (in which case we must wait for it to complete
     successfully before interrupt or complete with an exception), we can
     trigger a trap. *)
  let%hw trap_started = wire 1 in
  let%hw.Csrs.Of_signal csrs = Csrs.Of_signal.wires () in
  let%hw.Detect_exception.O.Of_signal detected =
    Detect_exception.hierarchical ~scope detect_exception
  in
  let%hw interrupt_enabled = wire 1 in
  let%hw can_start = ~:trap_started &&: m_stage.valid &&: ~:(m_stage.stall) in
  let%hw.Action.Of_signal start =
    Action.prioritize
      { exception_ = detected.exception_request.valid
      ; explicit_csr = detected.explicit_csr.valid
      ; mret = detected.mret
      ; sret = detected.sret
      ; interrupt = interrupt_request.valid &&: interrupt_enabled
      }
    |> Action.map ~f:(fun requested -> can_start &&: requested)
  in
  let%hw action_start = Action.any start in
  (* If the instruction in M has an exception, it cannot be allowed to
     complete, and our EPC is that instruction. Otherwise, it is allowed to
     proceed, and EPC is the next instruction after the last one to finish M
     (including the one currently completing). *)
  let%hw squash_M = start.exception_ in
  let%hw next_pc_latched =
    Types.Clocking.cut_through_reg clocking ~enable:can_start next_pc
  in
  let%hw epc = mux2 squash_M pc next_pc_latched in
  let%hw.Decode_trap.O.Of_signal decoded_trap =
    Decode_trap.hierarchical
      ~scope
      { epc
      ; trap_value = mux2 start.exception_ detected.exception_request.value (zero 32)
      ; exception_cause = detected.exception_request.cause
      ; interrupt_cause = interrupt_request.value
      ; csrs
      ; trap = Action.is_trap start
      ; interrupt = start.interrupt
      ; mret = start.mret
      ; sret = start.sret
      }
  in
  interrupt_enabled <-- decoded_trap.interrupt_enabled;
  (* Go to the decoded trap/return PC, or to the sequential PC if this is just
     an explicit CSR update. *)
  let%hw redirect_pc =
    Types.Clocking.cut_through_reg
      clocking
      ~enable:action_start
      (mux2 start.explicit_csr next_pc_latched decoded_trap.handler_pc)
  in
  (* TODO: figure out interrupt pending interface *)
  let%hw mip =
    mux2 interrupt_request.valid (of_unsigned_int ~width:32 (1 lsl 11)) (zero 32)
  in
  (* Update CSRs when trap begins. *)
  let%hw.Csr_bank.O.Of_signal csr_bank =
    Csr_bank.hierarchical
      ~scope
      { clocking
      ; mip
      ; explicit_write = { detected.explicit_csr with valid = start.explicit_csr }
      ; trap_write = decoded_trap.update
      }
  in
  Csrs.Of_signal.assign csrs csr_bank.csrs;
  (* The trap is finished once the CSR update goes through. *)
  trap_started <-- Utils.sr ~set:action_start ~reset:csr_bank.write_done clocking;
  ({ trap_active = action_start ||: trap_started
   ; squash_M
   ; handler_pc = { value = redirect_pc; valid = csr_bank.write_done }
   ; csrs
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
