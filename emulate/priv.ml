(* CSR, trap, and privileged-instruction helpers for the RISC-V emulator. *)

open! Base
open State
module Insn = Riscv_isa.Insn

(** Convert a CSR write into a legal value to match processor behavior for WARL, RARL, and
    read-legal registers. *)
let legalize_csr_write ~csr value =
  let open Insn.Csr_address in
  if csr = mstatus
  then (
    let allowed_mask = Int32.of_int_exn 0x007e19aa in
    let value = Int32.(value land allowed_mask) in
    let mpp = Int32.(to_int_exn ((value lsr 11) land of_int_exn 3)) in
    if mpp = 2 then Int32.(value lor (of_int_exn 3 lsl 11)) else value)
  else if csr = mstatush
  then Int32.zero
  else if csr = mie
  then Int32.(value land (of_int_exn 1 lsl 11))
  else if csr = mideleg
  then Int32.(value land (of_int_exn 1 lsl 11))
  else if csr = medeleg
  then (
    let mask = Int32.of_int_exn 0xb35d in
    Int32.(value land mask))
  else if csr = mtvec || csr = stvec
  then (
    let mode = Int32.(to_int_exn (value land of_int_exn 3)) in
    if mode <= 1 then value else Int32.(value land lnot (of_int_exn 3)))
  else if csr = mepc || csr = sepc
  then Int32.(value land lnot (of_int_exn 3))
  else value
;;

(** Read a CSR, including restricted supervisor aliases. *)
let read_csr { csrs; _ } csr =
  let open Insn.Csr_address in
  if csr = sstatus
  then Int32.(csrs.(mstatus) land of_int_exn 0x000c0122)
  else if csr = sie
  then Int32.(csrs.(mie) land csrs.(mideleg) land (of_int_exn 1 lsl 11))
  else if csr = sip
  then Int32.(csrs.(mip) land csrs.(mideleg) land (of_int_exn 1 lsl 11))
  else csrs.(csr)
;;

(** Write a CSR, including restricted supervisor aliases. *)
let write_csr { csrs; _ } csr value =
  let open Insn.Csr_address in
  if csr = sstatus
  then (
    let mask = Int32.of_int_exn 0x000c0122 in
    let mstatus_value = Int32.(csrs.(mstatus) land lnot mask lor (value land mask)) in
    csrs.(mstatus) <- legalize_csr_write ~csr:mstatus mstatus_value)
  else if csr = sie
  then (
    let mask = Int32.(csrs.(mideleg) land (of_int_exn 1 lsl 11)) in
    csrs.(mie) <- Int32.(csrs.(mie) land lnot mask lor (value land mask)))
  else if csr = sip || csr = mip
  then ()
  else csrs.(csr) <- legalize_csr_write ~csr value
;;

let trap_vector csrs ~tvec ~cause ~interrupt =
  let value = csrs.(tvec) in
  let base = Int32.(value land lnot (of_int_exn 3)) in
  let vectored = interrupt && Int32.(value land of_int_exn 3 = one) in
  if vectored
  then (
    let cause_number = Int32.(to_int_exn (cause land of_int_exn 0x7fff_ffff)) in
    Int32.(base + of_int_exn Int.(4 * cause_number)))
  else base
;;

let delegation_bit csrs ~delegation_csr cause =
  let cause_number = Int32.(to_int_exn (cause land of_int_exn 0x7fff_ffff)) in
  cause_number < 32 && Int32.(csrs.(delegation_csr) land (one lsl cause_number) <> zero)
;;

(** Update CSRs for a trap, returning the trap handler PC to execute from. *)
let take_trap { csrs; privilege; _ } ~epc ~cause ~trap_value ~interrupt =
  let open Insn.Csr_address in
  let delegation_csr = if interrupt then mideleg else medeleg in
  let trap_to_s = !privilege <> 3 && delegation_bit csrs ~delegation_csr cause in
  let mstatus_value = csrs.(mstatus) in
  if trap_to_s
  then (
    let sie_value = Int32.(to_int_exn ((mstatus_value lsr 1) land one)) in
    let cleared_fields =
      Int32.(
        mstatus_value
        land lnot ((of_int_exn 1 lsl 1) lor (of_int_exn 1 lsl 5) lor (of_int_exn 1 lsl 8)))
    in
    csrs.(mstatus)
    <- Int32.(
         cleared_fields
         lor (of_int_exn sie_value lsl 5)
         lor (of_int_exn (Int.bit_and !privilege 1) lsl 8));
    csrs.(sepc) <- Int32.(epc land lnot (of_int_exn 3));
    csrs.(scause) <- cause;
    csrs.(stval) <- trap_value;
    privilege := 1;
    trap_vector csrs ~tvec:stvec ~cause ~interrupt)
  else (
    let mie_value = Int32.(to_int_exn ((mstatus_value lsr 3) land one)) in
    let cleared_fields =
      Int32.(
        mstatus_value
        land lnot ((of_int_exn 1 lsl 3) lor (of_int_exn 1 lsl 7) lor (of_int_exn 3 lsl 11)))
    in
    csrs.(mstatus)
    <- Int32.(
         cleared_fields lor (of_int_exn mie_value lsl 7) lor (of_int_exn !privilege lsl 11));
    csrs.(mepc) <- Int32.(epc land lnot (of_int_exn 3));
    csrs.(mcause) <- cause;
    csrs.(mtval) <- trap_value;
    privilege := 3;
    trap_vector csrs ~tvec:mtvec ~cause ~interrupt)
;;

(** Update CSRs for a legal mret instruction, returning the PC to resume from. *)
let execute_mret_csrs { csrs; privilege; _ } =
  let open Insn.Csr_address in
  let mstatus_value = csrs.(mstatus) in
  let mpie = Int32.(to_int_exn ((mstatus_value lsr 7) land one)) in
  let mpp = Int32.(to_int_exn ((mstatus_value lsr 11) land of_int_exn 3)) in
  let clear_mask =
    Int32.(
      (of_int_exn 1 lsl 3)
      lor (of_int_exn 1 lsl 7)
      lor (of_int_exn 3 lsl 11)
      lor if Int.equal mpp 3 then zero else of_int_exn 1 lsl 17)
  in
  csrs.(mstatus)
  <- Int32.(
       mstatus_value
       land lnot clear_mask
       lor (of_int_exn mpie lsl 3)
       lor (of_int_exn 1 lsl 7));
  privilege := mpp;
  csrs.(mepc)
;;

(** Update CSRs for a legal sret instruction, returning the PC to resume from. *)
let execute_sret_csrs { csrs; privilege; _ } =
  let open Insn.Csr_address in
  let mstatus_value = csrs.(mstatus) in
  let spie = Int32.(to_int_exn ((mstatus_value lsr 5) land one)) in
  let spp = Int32.(to_int_exn ((mstatus_value lsr 8) land one)) in
  let clear_mask =
    Int32.(
      (of_int_exn 1 lsl 1)
      lor (of_int_exn 1 lsl 5)
      lor (of_int_exn 1 lsl 8)
      lor (of_int_exn 1 lsl 17))
  in
  csrs.(mstatus)
  <- Int32.(
       mstatus_value
       land lnot clear_mask
       lor (of_int_exn spie lsl 1)
       lor (of_int_exn 1 lsl 5));
  privilege := spp;
  csrs.(sepc)
;;

let implemented_csr csr =
  let open Insn.Csr_address in
  List.mem
    [ sstatus
    ; sie
    ; stvec
    ; sscratch
    ; sepc
    ; scause
    ; stval
    ; sip
    ; mstatus
    ; mstatush
    ; medeleg
    ; mideleg
    ; mie
    ; mtvec
    ; mscratch
    ; mepc
    ; mcause
    ; mtval
    ; mip
    ; 0x7c0
    ; 0x7c1
    ; 0x7c2
    ; 0x7c3
    ]
    csr
    ~equal:Int.equal
;;

let csr_access_legal ~privilege ({ op; src; csr; _ } : Insn.csr) =
  let open Insn in
  let required_privilege = (csr lsr 8) land 3 in
  let read_only = (csr lsr 10) land 3 = 3 in
  let writes =
    match op, src with
    | Csrrw, _ -> true
    | (Csrrs | Csrrc), Reg rs1 -> rs1 <> 0
    | (Csrrs | Csrrc), Imm imm -> Int32.(imm <> zero)
  in
  implemented_csr csr && privilege >= required_privilege && not (read_only && writes)
;;

(* Execute the privileged part of a CSR instruction, returning its next PC. *)
let execute_csr
  ({ regs; pc; privilege; _ } as state)
  ({ op; rd; src; csr } as csr_insn : Insn.csr)
  =
  let open Insn in
  if not (csr_access_legal ~privilege:!privilege csr_insn)
  then
    take_trap
      state
      ~epc:!pc
      ~cause:(Int32.of_int_exn 2)
      ~trap_value:(Insn.to_int32 (Insn.Csr csr_insn))
      ~interrupt:false
  else (
    let old_value = read_csr state csr in
    let operand =
      match src with
      | Insn.Reg rs1 -> regs.(rs1)
      | Imm imm -> imm
    in
    let writes =
      match op, src with
      | Csrrw, _ -> true
      | (Csrrs | Csrrc), Reg rs1 -> rs1 <> 0
      | (Csrrs | Csrrc), Imm imm -> Int32.(imm <> zero)
    in
    let new_value =
      match op with
      | Csrrw -> operand
      | Csrrs -> Int32.(old_value lor operand)
      | Csrrc -> Int32.(old_value land lnot operand)
    in
    regs.(rd) <- old_value;
    if writes then write_csr state csr new_value;
    Int32.(!pc + of_int_exn 4))
;;

(* Execute an environment call or breakpoint trap, returning the trap vector. *)
let execute_ecall ({ privilege; pc; _ } as state) =
  let cause =
    Int32.of_int_exn
      (match !privilege with
       | 0 -> 8
       | 1 -> 9
       | _ -> 11)
  in
  take_trap state ~epc:!pc ~cause ~trap_value:Int32.zero ~interrupt:false
;;

let execute_ebreak ({ pc; _ } as state) =
  take_trap
    state
    ~epc:!pc
    ~cause:(Int32.of_int_exn 3)
    ~trap_value:Int32.zero
    ~interrupt:false
;;

(* Execute mret or its illegal-instruction trap. *)
let execute_mret ({ privilege; pc; _ } as state) =
  let open Insn in
  if !privilege = 3
  then execute_mret_csrs state
  else
    take_trap
      state
      ~epc:!pc
      ~cause:(Int32.of_int_exn 2)
      ~trap_value:(Insn.to_int32 Mret)
      ~interrupt:false
;;

(* Execute sret or its illegal-instruction trap. *)
let execute_sret ({ csrs; privilege; pc; _ } as state) =
  let open Insn in
  let tsr = Int32.(csrs.(Insn.Csr_address.mstatus) land (of_int_exn 1 lsl 22) <> zero) in
  if !privilege >= 1 && ((not tsr) || !privilege = 3)
  then execute_sret_csrs state
  else
    take_trap
      state
      ~epc:!pc
      ~cause:(Int32.of_int_exn 2)
      ~trap_value:(Insn.to_int32 Sret)
      ~interrupt:false
;;
