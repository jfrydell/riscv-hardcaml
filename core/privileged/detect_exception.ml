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
    ; sret : 'a
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
let sret_insn = of_hex ~width:32 "10200073"

let create scope ({ insn; rs1; csrs } : _ I.t) =
  let%hw opcode = insn.:[6, 0] in
  let%hw funct3 = insn.:[14, 12] in
  let%hw is_csr = opcode ==: env_opcode &&: (funct3.:[1, 0] <>:. 0) in
  let%hw is_ecall = insn ==: ecall_insn in
  let%hw is_ebreak = insn ==: ebreak_insn in
  let%hw is_mret = insn ==: mret_insn in
  let%hw is_sret = insn ==: sret_insn in
  let%hw is_non_csr_system = opcode ==: env_opcode &&: (funct3 ==:. 0) in
  let%hw is_illegal_system =
    is_non_csr_system
    &&: ~:(is_ecall ||: is_ebreak ||: is_mret ||: is_sret)
    ||: (opcode ==: env_opcode &&: (funct3 ==:. 4))
  in
  let%hw illegal_mret = is_mret &&: (csrs.privilege <>:. 3) in
  let%hw.Csrs.Mstatus.Fields.Of_signal mstatus =
    Csrs.Mstatus.Fields.of_register csrs.mstatus
  in
  let%hw illegal_sret =
    is_sret &&: (csrs.privilege.:[1, 0] <:. 1 ||: (csrs.privilege ==:. 1 &&: mstatus.tsr))
  in
  let%hw ecall_cause =
    cases
      ~default:(of_int_trunc ~width:32 11)
      csrs.privilege.:[1, 0]
      [ of_int_trunc ~width:2 0, of_int_trunc ~width:32 8
      ; of_int_trunc ~width:2 1, of_int_trunc ~width:32 9
      ]
  in
  let%hw csr_address = insn.:[31, 20] in
  (* TODO: move to CSR execution, where other address matching happens *)
  let%hw csr_is_implemented =
    Csrs.to_list Csrs.addresses
    |> List.filter ~f:(fun address -> address <> Csrs.addresses.privilege)
    |> List.map ~f:(fun address -> csr_address ==:. address)
    |> reduce ~f:( |: )
  in
  let%hw csr_privilege_allowed = csrs.privilege.:[1, 0] >=: csr_address.:[9, 8] in
  (* TODO: move to decode *)
  let%hw csr_writes =
    funct3.:[1, 0] ==:. 1 ||: (funct3.:[1, 0] >=:. 2 &&: (insn.:[19, 15] <>:. 0))
  in
  let%hw csr_is_read_only = csr_address.:[11, 10] ==:. 3 in
  let%hw illegal_csr =
    is_csr
    &&: (~:csr_is_implemented
         ||: ~:csr_privilege_allowed
         ||: (csr_writes &&: csr_is_read_only))
  in
  let%hw illegal_instruction =
    is_illegal_system ||: illegal_mret ||: illegal_sret ||: illegal_csr
  in
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
   ; explicit_csr =
       { insn; rs1; mideleg = csrs.mideleg; valid = is_csr &&: ~:illegal_csr }
   ; mret = is_mret &&: ~:illegal_mret
   ; sret = is_sret &&: ~:illegal_sret
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
