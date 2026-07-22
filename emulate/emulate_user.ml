(* Emulator for the implemented RISC-V ISA subset used by the tests. *)

open! Base

(* Mutable processor state *)
type state =
  { regs : int32 Array.t
  ; csrs : int32 Array.t
  ; pc : int32 ref
  ; memory : (int32, int) Hashtbl.t
  (* Byte-addressed memory (range 0-255), defaulting to zero *)
  }
[@@deriving sexp_of]

let regs { regs; _ } = regs
let csrs { csrs; _ } = csrs
let pc { pc; _ } = !pc
let memory { memory; _ } = memory

(* Load a value from memory, extending as necessary to 32 bits *)
let load ~memory ~addr ~size ~extend =
  (* Load necessary number of bytes *)
  let load_byte addr = Hashtbl.find memory addr |> Option.value ~default:0 in
  let bytes = List.init size ~f:(fun n -> load_byte Int32.(addr + of_int_exn n)) in
  (* Build zero-extended (little-endian) literal *)
  let value =
    List.fold_right bytes ~init:0 ~f:(fun b v -> (256 * v) + b) |> Int32.of_int_trunc
  in
  (* Sign-extend if necessary *)
  match extend with
  | Riscv.Signed ->
    Int32.shift_right (Int32.shift_left value (32 - (8 * size))) (32 - (8 * size))
  | Riscv.Unsigned -> value
;;

(* Replace an implicitly-zero address not present in the hash table with an actual zero,
making it visible that it's been accessed. Done on loads so testing logic has guarantee
that addresses not in hash table have not yet been accessed by program. *)
let touch ~memory ~addr ~size =
  let touch_byte addr =
    Hashtbl.update memory addr ~f:(function
      | Some b -> b
      | None -> 0)
  in
  let _ = List.init size ~f:(fun n -> touch_byte Int32.(addr + of_int_exn n)) in
  ()
;;

(* Store a value to memory *)
let store ~memory ~addr ~value ~size =
  let value = Int32.to_int_exn value in
  for b = 0 to size - 1 do
    Hashtbl.set
      memory
      ~key:Int32.(addr + of_int_exn b)
      ~data:((value lsr (8 * b)) land 255)
  done
;;

(* Utility to compare two Int32s as unsigned *)
let less_than_unsigned a b =
  Int32.(
    match is_negative a, is_negative b with
    | true, false -> false
    | false, true -> true
    | _ -> a < b)
;;

let mem_size = function
  | Riscv.Word -> 4
  | Riscv.Half -> 2
  | Riscv.Byte -> 1
;;

(* Instruction at current PC *)
let current_pc_insn { pc; memory; _ } =
  let insn = load ~memory ~addr:!pc ~size:4 ~extend:Unsigned in
  match Riscv.of_int32 insn with
  | Ok insn -> insn
  | Error error ->
    raise_s [%message "Failed to get current PC insn" (error : Error.t) (!pc : Int32.t)]
;;

(* Get the next PC the emulator would reach if it were to execute the given instruction. *)
let next_pc ~regs ~pc ~insn =
  match insn with
  | Riscv.Jal { imm; _ } -> Int32.(pc + imm)
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
  | Riscv.Store (size, { rs1; imm; _ }) | Riscv.Load (size, _, { rs1; imm; _ }) ->
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

(** Execute 1 instruction on the emulator, updating its state *)
let step { regs; csrs; pc; memory } =
  let open Riscv in
  (* Read instruction and convert to expected format *)
  let insn = current_pc_insn { regs; csrs; pc; memory } in
  (* Stdlib.print_endline (Sexp.to_string_hum (sexp_of_insn insn)); *)
  (* Interpeter helpers *)
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
  (* Calculate next PC (must happen before main computation for jalr updating same reg it reads) *)
  let next_pc = next_pc ~regs ~pc:!pc ~insn in
  (* Instruction dispatch *)
  (match insn with
   | IntReg (op, { rd; rs1; rs2 }) -> alu rd rs1 regs.(rs2) op
   | IntImm (op, { rd; rs1; imm }) -> alu rd rs1 imm op
   | Load (size, sign, { rd; rs1; imm }) ->
     touch ~memory ~addr:Int32.(regs.(rs1) + imm) ~size:(mem_size size);
     regs.(rd)
     <- load ~memory ~addr:Int32.(regs.(rs1) + imm) ~size:(mem_size size) ~extend:sign
   | Store (size, { rs1; rs2; imm }) ->
     store ~memory ~addr:Int32.(regs.(rs1) + imm) ~size:(mem_size size) ~value:regs.(rs2)
   | Branch _ -> ()
   | Jal { rd; _ } -> Int32.(regs.(rd) <- !pc + of_int_exn 4)
   | Jalr { rd; _ } -> Int32.(regs.(rd) <- !pc + of_int_exn 4)
   | Lui { rd; imm } -> regs.(rd) <- imm
   | AuiPc { rd; imm } -> Int32.(regs.(rd) <- !pc + imm)
   | Csr { op; rd; src; csr } ->
     let old_value = csrs.(csr) in
     let operand =
       match src with
       | Reg rs1 -> regs.(rs1)
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
     if writes then csrs.(csr) <- new_value
   | Env -> failwith "Env call");
  (* Preserve r0 *)
  regs.(0) <- Int32.zero;
  (* Update PC *)
  pc := next_pc
;;
