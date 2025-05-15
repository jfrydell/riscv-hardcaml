(* Types representing RISC-V instructions, along with conversion to/from binary *)

open! Base

(* A register *)
type reg = int [@@deriving equal, sexp]

(* Signed or unsigned *)
type signedness = Signed | Unsigned
  [@@deriving equal, sexp]

(* Type of integer ALU operation (immediate or register) *)
type aluop =
  | Add | Sub | Xor | Or | And
  | Sll | Srl | Sra | Slt | Sltu
  [@@deriving equal, sexp]

(* Type of branch *)
type branchop =
  | Eq | Neq | Lt of signedness | Ge of signedness [@@deriving equal, sexp]
(* Size of a memory op *)
type memsize = Byte | Half | Word [@@deriving equal, sexp]

(* The registers (2 in 1 out) specified by an R-type instruction *)
type regs21 = { rd: reg; rs1: reg; rs2: reg } [@@deriving equal, sexp]
(* Registers (1 in 1 out) and immediate specified by an I-type instruction *)
type regs11 = { rd: reg; rs1: reg; imm: int32 } [@@deriving equal, sexp]
(* Registers (2 in 0 out) and immediate for B-type or S-type instructions *)
type regs20 = { rs1: reg; rs2: reg; imm: int32 } [@@deriving equal, sexp]
(* Registers (0 in 1 out) and immediate for U-type or J-type instructions *)
type regs01 = { rd: reg; imm: int32 } [@@deriving equal, sexp]

type insn =
  | IntReg of aluop * regs21
  | IntImm of aluop * regs11
  | Load of memsize * signedness * regs11
  | Store of memsize * regs20
  | Branch of branchop * regs20
  | Jal of regs01
  | Jalr of regs11
  | Lui of regs01
  | AuiPc of regs01
  | Env (* TODO: ecall vs ebreak *)
  [@@deriving equal, sexp]

(* Nop = addi $0, $0, 0 *)
let nop = IntImm (Add, { rd = 0; rs1 = 0; imm = Int32.zero })


(* Useful defs for binary *)
module Binary = struct
  module Op = struct
    let intR = Int32.of_string "0b0110011"
    let intI = Int32.of_string "0b0010011"
    let load = Int32.of_string "0b0000011"
    let store = Int32.of_string "0b0100011"
    let branch = Int32.of_string "0b1100011"
    let jal = Int32.of_string "0b1101111"
    let jalr = Int32.of_string "0b1100111"
    let lui = Int32.of_string "0b0110111"
    let auiPc = Int32.of_string "0b0010111"
    let env = Int32.of_string "0b1110011"
  end

  module Funct3 = struct
    (* Array of branch ops indexed by funct3 (None if not specified) *)
    let branchop = [| Some Eq; Some Neq; None; None;
                      Some (Lt Signed); Some (Ge Signed);
                      Some (Lt Unsigned); Some (Ge Unsigned) |]

    (* Array of ALU ops indexed by funct3 (not including sra or sub which are funct7-determined) *)
    let aluop = [| Add; Sll; Slt; Sltu; Xor; Srl; Or; And |]

  end
end

(* Int32 helpers *)
(* Extract bit range from `min` to `max` (inclusive, zero-indexed) from an int32 as an int *)
let bits insn max min = Int32.to_int_exn
  (Int32.(land) (Int32.(lsr) insn min) (Int32.of_int_trunc (2 ** (max-min+1) - 1)))
(* Sign-extend an int with the given number of bits to an int32 *)
let of_int_sign ~w value = Int32.(asr) (Int32.(lsl) (Int32.of_int_trunc value) (32 - w)) (32 - w)

(* Convert binary instruction to `insn` *)
let of_int32_exn insn =
  let opcode = bits insn 6 0 in
  let funct7 = bits insn 31 25 in
  let funct3 = bits insn 14 12 in
  let rd = bits insn 11 7 in
  let rs1 = bits insn 19 15 in
  let rs2 = bits insn 24 20 in
  let immi = of_int_sign ~w:12 (bits insn 31 20) in
  let imms = of_int_sign ~w:12 ((bits insn 31 25) * 0x20 + bits insn 11 7) in
  let immb = of_int_sign ~w:13 ((bits insn 31 31) * 0x1000 + (bits insn 7 7) * 0x800 +
                                (bits insn 30 25) * 0x20 + (bits insn 11 8) * 0x2) in
  let immj = of_int_sign ~w:21 ((bits insn 31 31) * 0x100000 + (bits insn 19 12) * 0x1000 +
                                (bits insn 20 20) * 0x800 + (bits insn 30 21) * 0x2) in
  let immu = of_int_sign ~w:32 (0x1000 * bits insn 31 12) in
  if opcode = (Int32.to_int_exn Binary.Op.intR) then
    IntReg (
      (match Binary.Funct3.aluop.(funct3) with
        | Add when funct7 = 0x20 -> Sub
        | Srl when funct7 = 0x20 -> Sra
        | op -> op),
      { rd; rs1; rs2 }
    )
  else if opcode = (Int32.to_int_exn Binary.Op.intI) then
    let op, mask = match Binary.Funct3.aluop.(funct3) with
        | Sll -> Sll, 0x1f (* Shifts only take 5 imm bits and don't sign-extend *)
        | Srl when funct7 = 0x20 -> Sra, 0x1f
        | Srl -> Srl, 0x1f
        | op -> op, 0xffffffff (* Others take all bits, sign-extended *)
    in
    IntImm (op, {rd; rs1; imm = Int32.(immi land of_int_trunc mask)})
  else let mem_size () = match bits insn 13 12 with
      | 0 -> Byte
      | 1 -> Half
      | 2 -> Word
      | _ -> failwith "illegal instruction: mem size" in
  if opcode = (Int32.to_int_exn Binary.Op.load) then
    Load (
      mem_size (),
      (match bits insn 14 14 with
        | 0 -> Signed
        | 1 -> Unsigned
        | _ -> failwith "impossible"),
      { rd; rs1; imm = immi; }
    )
  else if opcode = (Int32.to_int_exn Binary.Op.store) then
    Store (
      mem_size (),
      { rs1; rs2; imm = imms }
    )
  else if opcode = (Int32.to_int_exn Binary.Op.branch) then
    Branch (
      Option.value_or_thunk ~default:(fun _ -> failwith "illegal instruction: branch type")
        (Binary.Funct3.branchop.(funct3)),
      { rs1; rs2; imm = immb }
    )
  else if opcode = (Int32.to_int_exn Binary.Op.jal) then
    Jal { rd; imm = immj }
  else if opcode = (Int32.to_int_exn Binary.Op.jalr) then
    Jalr { rd; rs1; imm = immi }
  else if opcode = (Int32.to_int_exn Binary.Op.lui) then
    Lui { rd; imm = immu }
  else if opcode = (Int32.to_int_exn Binary.Op.auiPc) then
    AuiPc { rd; imm = immu }
  else if opcode = (Int32.to_int_exn Binary.Op.env) then
    Env
  else failwith "illegal instruction: opcode"


(* Move a set of bits from an int32 to another location within the int32 *)
let move_bits value max_ min_ loc =
  let mask = Int32.of_int_trunc (2 ** (max_ - min_ + 1) - 1) in
  Int32.(((value lsr min_) land mask) lsl loc)

(* Convert `insn` to binary *)
let to_int32 =
  let regsr {rd; rs1; rs2} = Int32.(of_int_exn rd lsl 7 + of_int_exn rs1 lsl 15 + of_int_exn rs2 lsl 20) in
  let regsi {rd; rs1; imm} = Int32.(of_int_exn rd lsl 7 + of_int_exn rs1 lsl 15 + imm lsl 20) in
  let regss {rs1; rs2; imm} = Int32.(move_bits imm 4 0 7 + of_int_exn rs1 lsl 15 + of_int_exn rs2 lsl 20 + move_bits imm 11 5 25) in
  let regsb {rs1; rs2; imm} = Int32.(move_bits imm 11 11 7 + move_bits imm 4 1 8 + of_int_exn rs1 lsl 15 +
                                    of_int_exn rs2 lsl 20 + move_bits imm 10 5 25 + move_bits imm 12 12 31) in
  let regsu {rd; imm} = Int32.(of_int_exn rd lsl 7 + move_bits imm 31 12 12) in
  let regsj {rd; imm} = Int32.(of_int_exn rd lsl 7 + move_bits imm 19 12 12 + move_bits imm 11 11 20 +
                                move_bits imm 10 1 21 + move_bits imm 20 20 31) in

  let aluop op =
    let op, extra = match op with
      | Sub -> Add, Int32.(of_int_exn 0x20 lsl 25)
      | Sra -> Srl, Int32.(of_int_exn 0x20 lsl 25)
      | op -> op, Int32.zero in
    let opbits, _ = Option.value_exn (Array.findi Binary.Funct3.aluop
      ~f:(fun _ o -> equal_aluop o op)) in
    Int32.(of_int_exn opbits lsl 12 + extra)
  and mem_size s = Int32.(of_int_exn (match s with Byte -> 0 | Half -> 1 | Word -> 2) lsl 12)
  and mem_sign s = Int32.(of_int_exn (match s with Signed -> 0 | Unsigned -> 1) lsl 14)
  and brop op =
    let f3 = match op with
      | Eq -> 0 | Neq -> 1 | Lt Signed -> 4 | Ge Signed -> 5 | Lt Unsigned -> 6 | Ge Unsigned -> 7
    in Int32.(of_int_exn f3 lsl 12)
  in
  function
  | IntReg (op, r) -> Int32.(+) Int32.(regsr r + aluop op) Binary.Op.intR
  | IntImm (op, r) -> Int32.(+) Int32.(regsi r + aluop op) Binary.Op.intI
  | Load (size, sgn, r) -> Int32.(+) Int32.(regsi r + mem_size size + mem_sign sgn) Binary.Op.load
  | Store (size, r) -> Int32.(+) Int32.(regss r + mem_size size) Binary.Op.store
  | Branch (op, r) -> Int32.(+) Int32.(regsb r + brop op) Binary.Op.branch
  | Jal r -> Int32.(+) (regsj r) Binary.Op.jal
  | Jalr r -> Int32.(+) (regsi r) Binary.Op.jalr
  | Lui r -> Int32.(+) (regsu r) Binary.Op.lui
  | AuiPc r -> Int32.(+) (regsu r) Binary.Op.auiPc
  | Env -> Binary.Op.env


(* Test bits *)
let%test "bits 1" = bits Int32.(of_string "0x00708093") 6 0 = 19
let%test "bits 2" = bits Int32.(of_string "0x00012980") 6 0 = 0
let%test "bits 3" = bits Int32.(of_string "0x7fc71317") 31 12 = 523377

let%test "of_int_sign 1" = of_int_sign ~w:32 (0x1000 * 523377) |> Int32.(=) (Int32.of_int_exn 2143752192)

(* Check some basic translation *)
let%test "binary nop" = Int32.(=) (Int32.of_string "0x13") (to_int32 nop)
let%test "binary basic addi" = Int32.(=)
                              (Int32.of_string "0x00700093")
                              (to_int32 (IntImm (Add, {rd = 1; rs1 = 0; imm = Int32.of_int_exn 7})))

(* Check round-trips *)
let roundtrip insn = equal_insn (of_int32_exn (to_int32 insn)) insn
let roundtrip_bin b = Int32.(=) (to_int32 (of_int32_exn b)) b

let%test "roundtrip nop" = roundtrip nop
let%test "roundtrip basic addi" = roundtrip (IntImm (Add, {rd = 1; rs1 = 1; imm = Int32.of_int_exn 12}))
let%test "roundtrip basic add" = roundtrip (IntReg (Add, {rd = 2; rs1 = 1; rs2 = 1}))
let%test "roundtrip sw" = roundtrip (Store (Word, {rs1 = 2; rs2 = 7; imm = Int32.of_int_exn 2}))
let%test "roundtrip auipc" = roundtrip (AuiPc {rd = 6; imm = Int32.of_int_exn 2143752192})
let%test "roundtrip sra" = roundtrip (IntImm (Sra, {rd = 5; rs1 = 5; imm = Int32.of_int_trunc 31}))

(* let failing_insn = IntImm (Sra, {rd = 5; rs1 = 5; imm = Int32.of_int_trunc 31})
let _ = Stdio.printf "Original sexp:  "; Stdio.print_s (sexp_of_insn failing_insn)
let _ = Stdio.printf "Original to binary: %08x\n" (Int32.to_int_exn (to_int32 failing_insn))
let _ = Stdio.printf "Roundtrip sexp: "; Stdio.print_s (sexp_of_insn (of_int32_exn (to_int32 failing_insn))) *)
