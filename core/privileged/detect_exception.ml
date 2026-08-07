(** Detect synchronous exceptions, returns, and explicit CSR accesses from an instruction. *)

open! Core
open Hardcaml
open Signal

(** Info needed by trap & CSR logic to trigger and process an exception. *)
module Exception_request = struct
  type 'a t =
    { valid : 'a
    ; cause : 'a [@bits 32] (** Cause written to [mcause] or [scause]. *)
    ; value : 'a [@bits 32] (** Value written to [mtval] or [stval]. *)
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; decoded : 'a Decoded.t
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

let create scope ({ insn; decoded; rs1; csrs } : _ I.t) =
  let%hw is_non_csr_system =
    decoded.opcode ==: Riscv_isa.Of_signal.Op.env &&: (decoded.funct3 ==:. 0)
  in
  (* TODO: move to general Decoded.is_illegal. *)
  let%hw is_illegal_system =
    is_non_csr_system
    &&: ~:(decoded.is_ecall ||: decoded.is_ebreak ||: decoded.is_mret ||: decoded.is_sret)
    ||: (decoded.opcode ==: Riscv_isa.Of_signal.Op.env &&: (decoded.funct3 ==:. 4))
  in
  let%hw illegal_mret = decoded.is_mret &&: (csrs.privilege <>:. 3) in
  let%hw.Csrs.Mstatus.Fields.Of_signal mstatus =
    Csrs.Mstatus.Fields.of_register csrs.mstatus
  in
  let%hw illegal_sret =
    decoded.is_sret
    &&: (csrs.privilege.:[1, 0] <:. 1 ||: (csrs.privilege ==:. 1 &&: mstatus.tsr))
  in
  let%hw ecall_cause =
    cases
      ~default:(of_unsigned_int ~width:32 11)
      csrs.privilege.:[1, 0]
      [ of_unsigned_int ~width:2 0, of_unsigned_int ~width:32 8
      ; of_unsigned_int ~width:2 1, of_unsigned_int ~width:32 9
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
  let%hw csr_is_read_only = csr_address.:[11, 10] ==:. 3 in
  let%hw illegal_csr =
    decoded.is_csr
    &&: (~:csr_is_implemented
         ||: ~:csr_privilege_allowed
         ||: (decoded.csr_writes &&: csr_is_read_only))
  in
  let%hw illegal_instruction =
    is_illegal_system ||: illegal_mret ||: illegal_sret ||: illegal_csr
  in
  let%hw.Exception_request.Of_signal exception_request =
    { valid = decoded.is_ecall ||: decoded.is_ebreak ||: illegal_instruction
    ; cause =
        mux2
          illegal_instruction
          (of_unsigned_int ~width:32 2)
          (mux2 decoded.is_ebreak (of_unsigned_int ~width:32 3) ecall_cause)
    ; value = mux2 illegal_instruction insn (zero 32)
    }
  in
  ({ exception_request
   ; explicit_csr =
       { insn; rs1; mideleg = csrs.mideleg; valid = decoded.is_csr &&: ~:illegal_csr }
   ; mret = decoded.is_mret &&: ~:illegal_mret
   ; sret = decoded.is_sret &&: ~:illegal_sret
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
