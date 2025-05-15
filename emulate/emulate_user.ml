(* Emulator for unprivileged RISC-V base ISA for running tests against. *)

open! Base

(* Mutable processor state *)
type state = {
  regs: int32 Array.t;
  pc: int32 ref;
  memory: (int32, int) Hashtbl.t; (* Byte-addressed memory (range 0-255), defaulting to zero *)
}
[@@deriving sexp_of]

(* Load a value from memory, extending as necessary to 32 bits *)
let load ~memory ~addr ~size ~extend =
  (* Load necessary number of bytes *)
  let load_byte addr = Hashtbl.find memory addr |> Option.value ~default:0 in
  let bytes = List.init size ~f:(fun n -> load_byte Int32.(addr + of_int_exn n)) in
  (* Build zero-extended (little-endian) literal *)
  let value = List.fold_right bytes ~init:0 ~f:(fun b v -> 256*v + b) |> Int32.of_int_trunc in
  (* Sign-extend if necessary *)
  match extend with
  | Riscv.Signed -> Int32.shift_right (Int32.shift_left value (8*size)) (8*size)
  | Riscv.Unsigned -> value

(* Store a value to memory *)
let store ~memory ~addr ~value ~size =
  let value = Int32.to_int_exn value in
  for b = 0 to size - 1 do
    Hashtbl.set memory ~key:Int32.(addr + of_int_exn b) ~data:((value lsr (8*b)) land 255)
  done

(* Instruction at current PC *)
let current_pc_insn {pc; memory; _} =
  let insn = load ~memory ~addr:!pc ~size:4 ~extend:Unsigned in
  (* Stdio.printf "emulate PC %d = %08x\n" (Int32.to_int_exn !pc) (Int32.to_int_exn insn); *)
  Riscv.of_int32_exn insn

let step {regs; pc; memory} =
  let open Riscv in
  (* Read instruction and convert to expected format *)
  let insn = current_pc_insn {regs; pc; memory} in

  (* Interpeter helpers *)
  let less_than_unsigned a b = Int32.(match is_negative a, is_negative b with
      | true, false -> false | false, true -> true | _ -> a < b) in
  let alu rd rs1 src2 op = let src1 = regs.(rs1) in match op with
    | Add -> regs.(rd) <- Int32.(src1 + src2)
    | Sub -> regs.(rd) <- Int32.(src1 - src2)
    | Sll -> regs.(rd) <- Int32.(lsl) src1 (Int32.to_int_exn src2 % 32)
    | Slt -> regs.(rd) <- Int32.(if src1 < src2 then one else zero)
    | Sltu -> regs.(rd) <- Int32.(if less_than_unsigned src1 src2 then one else zero)
    | Xor -> regs.(rd) <- Int32.(src1 lxor src2)
    | Srl -> regs.(rd) <- Int32.(lsr) src1 (Int32.to_int_exn src2 % 32)
    | Sra -> regs.(rd) <- Int32.(asr) src1 (Int32.to_int_exn src2 % 32)
    | Or -> regs.(rd) <- Int32.(src1 lor src2)
    | And -> regs.(rd) <- Int32.(src1 land src2)
  and mem_size = function Riscv.Word -> 4 | Riscv.Half -> 2 | Riscv.Byte -> 1
  in

  (* Main interpreter *)
  (match insn with
    | IntReg (op, {rd; rs1; rs2}) -> alu rd rs1 regs.(rs2) op
    | IntImm (op, {rd; rs1; imm}) -> alu rd rs1 imm op
    | Load (size, sign, {rd; rs1; imm}) ->
        regs.(rd) <- load ~memory ~addr:Int32.(regs.(rs1) + imm) ~size:(mem_size size) ~extend:sign
    | Store (size, {rs1; rs2; imm}) ->
        store ~memory ~addr:Int32.(regs.(rs1) + imm) ~size:(mem_size size) ~value:regs.(rs2)
    | Branch (brop, {rs1; rs2; imm}) ->
        let s1 = regs.(rs1) and s2 = regs.(rs2) in
        if (match brop with
        | Eq -> Int32.(s1 = s2)
        | Neq -> Int32.(s1 <> s2)
        | Lt Signed -> Int32.(s1 < s2)
        | Lt Unsigned -> less_than_unsigned s1 s2
        | Ge Signed -> Int32.(s1 >= s2)
        | Ge Unsigned -> not (less_than_unsigned s1 s2)
        ) then pc := Int32.(!pc + imm)
    | Jal {rd; imm} -> Int32.(regs.(rd) <- !pc + of_int_exn 4; pc := !pc + imm)
    | Jalr {rd; rs1; imm} -> Int32.(regs.(rd) <- !pc + of_int_exn 4; pc := regs.(rs1) + imm)
    | Lui {rd; imm} -> regs.(rd) <- imm
    | AuiPc {rd; imm} -> Int32.(regs.(rd) <- !pc + imm)
    | Env -> failwith "Env call"
  );
  (* Preserve r0 *)
  regs.(0) <- Int32.zero
