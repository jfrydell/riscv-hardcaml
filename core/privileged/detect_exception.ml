(** Detect synchronous exceptions, returns, and explicit CSR accesses from an instruction. *)

open! Core
open Hardcaml
open Signal

(** Exceptional conditions detected throughout the datapath. *)
module Triggers = struct
  type 'a t =
    { fetch_fault : 'a
    ; memory_fault : 'a
    ; branch_unaligned : 'a
    ; access_unaligned : 'a
    }
  [@@deriving hardcaml]
end

(** Info needed by trap & CSR logic to trigger and process an exception. *)
module Exception_request = struct
  module T = struct
    type 'a t =
      { cause : 'a [@bits 32] (** Cause written to [mcause] or [scause]. *)
      ; value : 'a [@bits 32] (** Value written to [mtval] or [stval]. *)
      }
    [@@deriving hardcaml]
  end

  include With_valid.Wrap.Make (T)
end

module I = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; decoded : 'a Decoded.t
    ; rs1 : 'a [@bits 32] (** Value in rs1 for CSR write. *)
    ; pc : 'a [@bits 32] (** Address of this instruction. *)
    ; next_pc : 'a [@bits 32] (** Address selected by this instruction. *)
    ; memory_addr : 'a [@bits 32] (** Effective virtual address for a load or store. *)
    ; csrs : 'a Csrs.t
    ; triggers : 'a Triggers.t
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

(* TODO: move to general Decoded.is_illegal *)
let detect_illegal_insn scope ~(decoded : _ Decoded.t) ~(csrs : _ Csrs.t) =
  let%hw is_non_csr_system =
    decoded.opcode ==: Riscv_isa.Of_signal.Op.env &&: (decoded.funct3 ==:. 0)
  in
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
  (* TODO: move to CSR execution, where other address matching happens *)
  let%hw csr_is_implemented =
    Csrs.to_list Csrs.addresses
    |> List.filter ~f:(fun address -> address <> Csrs.addresses.privilege)
    |> List.map ~f:(fun address -> decoded.csr_addr ==:. address)
    |> reduce ~f:( |: )
  in
  let%hw csr_privilege_allowed = csrs.privilege.:[1, 0] >=: decoded.csr_addr.:[9, 8] in
  let%hw csr_is_read_only = decoded.csr_addr.:[11, 10] ==:. 3 in
  let%hw illegal_csr =
    decoded.is_csr
    &&: (~:csr_is_implemented
         ||: ~:csr_privilege_allowed
         ||: (decoded.csr_writes &&: csr_is_read_only))
  in
  let%hw illegal_instruction =
    is_illegal_system ||: illegal_mret ||: illegal_sret ||: illegal_csr
  in
  illegal_instruction
;;

let create
  scope
  ({ insn; decoded; rs1; pc; next_pc; memory_addr; csrs; triggers } : _ I.t)
  =
  let%hw illegal_instruction =
    detect_illegal_insn (Scope.sub_scope scope "detect_illegal") ~csrs ~decoded
  in
  let%hw.Exception_request.Of_signal exception_request =
    Exception_request.T.Of_signal.priority_select
      [ { valid = triggers.fetch_fault (* TODO: page vs access fault *)
        ; value = { cause = of_unsigned_int ~width:32 12; value = pc }
        }
      ; { valid = illegal_instruction
        ; value = { cause = of_unsigned_int ~width:32 2; value = insn }
        }
      ; { valid = triggers.branch_unaligned
        ; value = { cause = of_unsigned_int ~width:32 0; value = next_pc }
        }
      ; { valid = decoded.is_ecall
        ; value =
            { cause = of_unsigned_int ~width:32 8 |: csrs.privilege; value = zero 32 }
        }
      ; { valid = decoded.is_ebreak
        ; value = { cause = of_unsigned_int ~width:32 3; value = zero 32 }
        }
      ; { valid = triggers.access_unaligned
        ; value =
            { cause =
                mux2
                  (decoded.opcode ==: Riscv_isa.Of_signal.Op.load)
                  (of_unsigned_int ~width:32 4)
                  (of_unsigned_int ~width:32 6)
            ; value = memory_addr
            }
        }
      ; { valid = triggers.memory_fault
        ; value =
            { cause =
                mux2
                  (decoded.opcode ==: Riscv_isa.Of_signal.Op.load)
                  (of_unsigned_int ~width:32 13)
                  (of_unsigned_int ~width:32 15)
            ; value = memory_addr
            }
        }
      ]
  in
  (* TODO: prioritize mret/sret/explicit_csr correctly too? or are they always after possible exceptions (for that insn type)? *)
  ({ exception_request
   ; explicit_csr =
       { insn
       ; rs1
       ; mideleg = csrs.mideleg
       ; valid = decoded.is_csr &&: ~:(exception_request.valid)
       }
   ; mret = decoded.is_mret &&: ~:(exception_request.valid)
   ; sret = decoded.is_sret &&: ~:(exception_request.valid)
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
