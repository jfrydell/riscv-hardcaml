(* Tools for generating random RISC-V programs *)

open! Base

let aluop ~subenabled () = Riscv.(
  match Random.int (if subenabled then 10 else 9) with
    | 0 -> Add | 1 -> Xor | 2 -> Or | 3 -> And | 4 -> Sll
    | 5 -> Srl | 6 -> Sra | 7 -> Slt | 8 -> Sltu | 9 -> Sub
    | _ -> failwith "invalid random"
  )

let sign () = if Random.bool () then Riscv.Signed else Riscv.Unsigned

let memsize () = Riscv.(
  match Random.int 3 with
    | 0 -> Byte | 1 -> Half | 2 -> Word
    | _ -> failwith "invalid random"
  )

let branchop () = Riscv.(
  match Random.int 4 with
    | 0 -> Eq | 1 -> Neq | 2 -> Lt (sign ()) | 3 -> Ge (sign ())
    | _ -> failwith "invalid random"
  )


(* Generates a single random instruction with the given constraints on register and memory offset choice
(small range should make hazards more common) *)
let instruction ~mem_range:(memmin, memmax) ?reg_max:(reg_max = 32) () =
  let reg () = Random.int reg_max in
  let imm maxbit minbit = Int32.(lsl) (Random.int32 (Int.pow 2 (maxbit-minbit+1) |> Int32.of_int_exn)) minbit in
  let addr () = Random.int32_incl memmin memmax in
  match Random.int 9 with
  | 0 -> Riscv.IntReg (aluop ~subenabled:true (), {rd = reg (); rs1 = reg (); rs2 = reg ()})
  | 1 -> Riscv.IntImm (aluop ~subenabled:false (), {rd = reg (); rs1 = reg (); imm = imm 11 0})
  | 2 -> Riscv.Load (memsize (), sign (), {rd = reg (); rs1 = reg (); imm = addr ()})
  | 3 -> Riscv.Store (memsize (), {rs1 = reg (); rs2 = reg (); imm = addr ()})
  | 4 -> Riscv.Branch (branchop (), {rs1 = reg (); rs2 = reg (); imm = imm 12 1})
  | 5 -> Riscv.Jal {rd = reg (); imm = imm 20 2}
  | 6 -> Riscv.Jalr {rs1 = reg (); rd = reg (); imm = imm 11 0}
  | 7 -> Riscv.Lui {rd = reg (); imm = imm 31 12}
  | 8 -> Riscv.AuiPc {rd = reg (); imm = imm 31 12}
  | _ -> failwith "invalid random"
