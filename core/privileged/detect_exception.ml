(** Detect synchronous exceptions, returns, and explicit CSR accesses from an instruction. *)

open! Core
open Hardcaml
open Signal

module Exception_request = struct
  type 'a t =
    { valid : 'a
    ; cause : 'a [@bits 32]
    ; value : 'a [@bits 32] (** Value written to [mtval]. *)
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; rs1 : 'a [@bits 32]
    ; csrs : 'a Csrs.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { exception_request : 'a Exception_request.t
    ; explicit_csr : 'a Explicit_csr.Decode.I.t
    ; mret : 'a
    }
  [@@deriving hardcaml]
end

(* These will come from the shared decoded instruction after the planned decode refactor.
   Keeping them local for now avoids a dependency cycle from [privileged] back to the core
   library. *)
let env_opcode = of_bit_string "1110011"
let ecall_insn = of_hex ~width:32 "00000073"
let ebreak_insn = of_hex ~width:32 "00100073"
let mret_insn = of_hex ~width:32 "30200073"

let create scope ({ insn; rs1; csrs } : _ I.t) =
  let%hw opcode = insn.:[6, 0] in
  let%hw funct3 = insn.:[14, 12] in
  let%hw is_csr = opcode ==: env_opcode &&: (funct3.:[1, 0] <>:. 0) in
  let%hw is_ecall = insn ==: ecall_insn in
  let%hw is_ebreak = insn ==: ebreak_insn in
  let%hw is_mret = insn ==: mret_insn in
  let%hw is_non_csr_system = opcode ==: env_opcode &&: (funct3 ==:. 0) in
  let%hw is_illegal_system =
    is_non_csr_system &&: ~:(is_ecall ||: is_ebreak ||: is_mret)
  in
  let%hw illegal_mret = is_mret &&: (csrs.privilege <>:. 3) in
  let%hw ecall_cause =
    cases
      ~default:(of_int_trunc ~width:32 11)
      csrs.privilege.:[1, 0]
      [ of_int_trunc ~width:2 0, of_int_trunc ~width:32 8
      ; of_int_trunc ~width:2 1, of_int_trunc ~width:32 9
      ]
  in
  let%hw illegal_instruction = is_illegal_system ||: illegal_mret in
  let%hw.Exception_request.Of_signal exception_request =
    { valid = is_ecall ||: is_ebreak ||: illegal_instruction
    ; cause =
        mux2
          illegal_instruction
          (of_int_trunc ~width:32 2)
          (mux2 is_ebreak (of_int_trunc ~width:32 3) ecall_cause)
    ; value = mux2 illegal_instruction insn (zero 32)
    }
  in
  ({ exception_request
   ; explicit_csr = { insn; rs1; valid = is_csr }
   ; mret = is_mret &&: ~:illegal_mret
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
