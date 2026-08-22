(* Unprivileged instruction emulation, including dispatch to privileged helpers. *)

open! Base
open State
module Insn = Riscv_isa.Insn

(* Utility to compare two Int32s as unsigned. *)
let less_than_unsigned a b =
  Int32.(
    match is_negative a, is_negative b with
    | true, false -> false
    | false, true -> true
    | _ -> a < b)
;;

let mem_size = function
  | Insn.Word -> 4
  | Insn.Half -> 2
  | Insn.Byte -> 1
;;

(* Instruction at the current PC. *)
let current_pc_insn { pc; memory; address_translation; _ } =
  let insn = load ~memory ~addr:(address_translation !pc) ~size:4 ~extend:Insn.Unsigned in
  match Insn.of_int32 insn with
  | Ok insn -> insn
  | Error error ->
    raise_s [%message "Failed to get current PC insn" (error : Error.t) (!pc : Int32.t)]
;;

(* Get the next PC the emulator would reach if it executed the given instruction. *)
let next_pc ~regs ~pc ~insn =
  match insn with
  | Insn.Jal { imm; _ } -> Int32.(pc + imm)
  | Jalr { imm; rs1; _ } -> Int32.(regs.(rs1) + imm)
  | Branch (brop, { rs1; rs2; imm }) ->
    let s1 = regs.(rs1)
    and s2 = regs.(rs2) in
    if match brop with
       | Eq -> Int32.(s1 = s2)
       | Neq -> Int32.(s1 <> s2)
       | Lt Signed -> Int32.(s1 < s2)
       | Lt Unsigned -> less_than_unsigned s1 s2
       | Ge Signed -> Int32.(s1 >= s2)
       | Ge Unsigned -> not (less_than_unsigned s1 s2)
    then Int32.(pc + imm)
    else Int32.(pc + of_int_exn 4)
  | _ -> Int32.(pc + of_int_exn 4)
;;

(** Get the next instruction's access address and size (in bytes), if it is a memory
    access. *)
let access_addr_and_size ~regs ~insn =
  match insn with
  | Insn.Store (size, { rs1; imm; _ }) | Insn.Load (size, _, { rs1; imm; _ }) ->
    Some (Int32.(regs.(rs1) + imm), mem_size size)
  | _ -> None
;;

(** Get a function determining if an address would be accessed by the given instruction. *)
let next_access ~regs ~insn =
  match access_addr_and_size ~regs ~insn with
  | Some (access_addr, size) ->
    fun addr ->
      Int32.(between (addr - access_addr) ~low:zero ~high:(Int.(size - 1) |> of_int_exn))
  | None -> fun _ -> false
;;

(** Return [true] if the next instruction is an unaligned access. *)
let is_unaligned_access ~regs ~insn =
  match access_addr_and_size ~regs ~insn with
  | Some (addr, size) -> (size - 1) land Int32.to_int_exn addr <> 0
  | None -> false
;;

(* Execute one instruction on the emulator, updating its state. *)
let step ({ regs; pc; memory; address_translation; _ } as state) =
  let open Insn in
  let insn = current_pc_insn state in
  let alu rd rs1 src2 op =
    let src1 = regs.(rs1) in
    match op with
    | Add -> regs.(rd) <- Int32.(src1 + src2)
    | Sub -> regs.(rd) <- Int32.(src1 - src2)
    | Sll -> regs.(rd) <- Int32.( lsl ) src1 (Int32.to_int_exn src2 % 32)
    | Slt -> regs.(rd) <- Int32.(if src1 < src2 then one else zero)
    | Sltu -> regs.(rd) <- Int32.(if less_than_unsigned src1 src2 then one else zero)
    | Xor -> regs.(rd) <- Int32.(src1 lxor src2)
    | Srl -> regs.(rd) <- Int32.( lsr ) src1 (Int32.to_int_exn src2 % 32)
    | Sra -> regs.(rd) <- Int32.( asr ) src1 (Int32.to_int_exn src2 % 32)
    | Or -> regs.(rd) <- Int32.(src1 lor src2)
    | And -> regs.(rd) <- Int32.(src1 land src2)
  in
  (* Calculate this before the main computation for jalr updating the same register it reads. *)
  let next_pc = ref (next_pc ~regs ~pc:!pc ~insn) in
  (match insn with
   | IntReg (op, { rd; rs1; rs2 }) -> alu rd rs1 regs.(rs2) op
   | IntImm (op, { rd; rs1; imm }) -> alu rd rs1 imm op
   | Load (size, sign, { rd; rs1; imm }) ->
     let addr = address_translation Int32.(regs.(rs1) + imm) in
     touch ~memory ~addr ~size:(mem_size size);
     regs.(rd) <- load ~memory ~addr ~size:(mem_size size) ~extend:sign
   | Store (size, { rs1; rs2; imm }) ->
     store
       ~memory
       ~addr:(address_translation Int32.(regs.(rs1) + imm))
       ~size:(mem_size size)
       ~value:regs.(rs2)
   | Branch _ -> ()
   | Jal { rd; _ } -> Int32.(regs.(rd) <- !pc + of_int_exn 4)
   | Jalr { rd; _ } -> Int32.(regs.(rd) <- !pc + of_int_exn 4)
   | Lui { rd; imm } -> regs.(rd) <- imm
   | AuiPc { rd; imm } -> Int32.(regs.(rd) <- !pc + imm)
   | Csr csr_insn -> next_pc := Priv.execute_csr state csr_insn
   | Ecall -> next_pc := Priv.execute_ecall state
   | Ebreak -> next_pc := Priv.execute_ebreak state
   | Mret -> next_pc := Priv.execute_mret state
   | Sret -> next_pc := Priv.execute_sret state
   | Fencei -> ());
  regs.(0) <- Int32.zero;
  pc := !next_pc
;;
