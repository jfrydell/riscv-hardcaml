open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { decoded : 'a Decoded.t
    ; privilege : 'a [@bits 32]
    ; mstatus : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { illegal : 'a } [@@deriving hardcaml]
end

let create scope ({ decoded; privilege; mstatus } : _ I.t) =
  let%hw funct3 = decoded.funct3 in
  let%hw funct7 = decoded.funct7 in
  let%hw csr_address = decoded.csr_addr in
  let%hw privilege = privilege.:[1, 0] in
  (* R-type funct7 can only be set for add and shift. *)
  let%hw legal_int_r =
    funct7 ==:. 0 ||: (funct7 ==:. 0x20 &&: (funct3 ==:. 0 ||: (funct3 ==:. 5)))
  in
  (* I-type funct7 is part of immediate, but not for shift. *)
  let%hw legal_int_i =
    funct3
    <>:. 1
    &&: (funct3 <>:. 5)
    ||: (funct3 ==:. 1 &&: (funct7 ==:. 0))
    ||: (funct3 ==:. 5 &&: (funct7 ==:. 0 ||: (funct7 ==:. 0x20)))
  in
  let%hw legal_load =
    funct3
    ==:. 0
    ||: (funct3 ==:. 1)
    ||: (funct3 ==:. 2)
    ||: (funct3 ==:. 4)
    ||: (funct3 ==:. 5)
  in
  let%hw legal_store = funct3 <=:. 2 in
  let%hw legal_branch = funct3 <=:. 1 ||: (funct3 >=:. 4) in
  let%hw legal_system =
    decoded.is_ecall ||: decoded.is_ebreak ||: decoded.is_mret ||: decoded.is_sret
  in
  let%hw legal_opcode =
    decoded.is_int_r
    &&: legal_int_r
    ||: (decoded.is_int_i &&: legal_int_i)
    ||: (decoded.is_load &&: legal_load)
    ||: (decoded.is_store &&: legal_store)
    ||: (decoded.is_branch &&: legal_branch)
    ||: (decoded.is_jalr &&: (funct3 ==:. 0))
    ||: decoded.is_jal
    ||: decoded.is_lui
    ||: decoded.is_auipc
    ||: (decoded.is_system &&: legal_system)
    ||: decoded.is_csr
  in
  let%hw csr_privilege_allowed = privilege >=: csr_address.:[9, 8] in
  let%hw csr_is_read_only = csr_address.:[11, 10] ==:. 3 in
  let%hw illegal_csr_access =
    decoded.is_csr
    &&: (~:csr_privilege_allowed ||: (decoded.csr_writes &&: csr_is_read_only))
  in
  let mstatus = Privileged.Csrs.Mstatus.Fields.of_register mstatus in
  let%hw illegal_mret = decoded.is_mret &&: (privilege <>:. 3) in
  let%hw illegal_sret =
    decoded.is_sret &&: (privilege <:. 1 ||: (privilege ==:. 1 &&: mstatus.tsr))
  in
  let%hw illegal_instruction =
    ~:legal_opcode ||: illegal_csr_access ||: illegal_mret ||: illegal_sret
  in
  ({ illegal = illegal_instruction } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
