(* Types representing RISC-V instructions, along with conversion to/from binary *)

open! Core

(* A register *)
type reg = int [@@deriving equal, sexp]

let quickcheck_generator_reg = Int.gen_uniform_incl 0 31
let quickcheck_observer_reg = Int.quickcheck_observer
let quickcheck_shrinker_reg = Quickcheck.Shrinker.empty ()

(* Signed or unsigned *)
type signedness =
  | Signed
  | Unsigned
[@@deriving equal, sexp, quickcheck]

(* Type of integer ALU operation (immediate or register) *)
type aluop =
  | Add
  | Sub
  | Xor
  | Or
  | And
  | Sll
  | Srl
  | Sra
  | Slt
  | Sltu
[@@deriving equal, sexp, quickcheck]

(* Type of branch *)
type branchop =
  | Eq
  | Neq
  | Lt of signedness
  | Ge of signedness
[@@deriving equal, sexp, quickcheck]

(* Size of a memory op *)
type memsize =
  | Byte
  | Half
  | Word
[@@deriving equal, sexp, quickcheck]

(* The registers (2 in 1 out) specified by an R-type instruction *)
type regs21 =
  { rd : reg
  ; rs1 : reg
  ; rs2 : reg
  }
[@@deriving equal, sexp, quickcheck]

(* Registers (1 in 1 out) and immediate specified by an I-type instruction *)
type regs11 =
  { rd : reg
  ; rs1 : reg
  ; imm : int32
  }
[@@deriving equal, sexp, quickcheck]

(* Registers (2 in 0 out) and immediate for B-type or S-type instructions *)
type regs20 =
  { rs1 : reg
  ; rs2 : reg
  ; imm : int32
  }
[@@deriving equal, sexp, quickcheck]

(* Registers (0 in 1 out) and immediate for U-type or J-type instructions *)
type regs01 =
  { rd : reg
  ; imm : int32
  }
[@@deriving equal, sexp, quickcheck]

(* A CSR source is either a register or a zero-extended five-bit immediate.
   The immediate is represented as a 32-bit value so it can be used directly
   by the emulator's read-modify-write logic. *)
type csr_src =
  | Reg of reg
  | Imm of int32
[@@deriving equal, sexp, quickcheck]

type csr_op =
  | Csrrw
  | Csrrs
  | Csrrc
[@@deriving equal, sexp, quickcheck]

type csr =
  { op : csr_op
  ; rd : reg
  ; src : csr_src
  ; csr : int
  }
[@@deriving equal, sexp, quickcheck]

module Csr_address = struct
  let sstatus = 0x100
  let sie = 0x104
  let stvec = 0x105
  let sscratch = 0x140
  let sepc = 0x141
  let scause = 0x142
  let stval = 0x143
  let sip = 0x144
  let misa = 0x301
  let mstatus = 0x300
  let mstatush = 0x310
  let medeleg = 0x302
  let mideleg = 0x303
  let mie = 0x304
  let mtvec = 0x305
  let mscratch = 0x340
  let mepc = 0x341
  let mcause = 0x342
  let mtval = 0x343
  let mip = 0x344
  let mvendorid = 0xf11
  let marchid = 0xf12
  let mimpid = 0xf13
  let mhartid = 0xf14
end

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
  | Csr of csr
  | Ecall
  | Ebreak
  | Mret
  | Sret
[@@deriving equal, sexp, quickcheck]

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
    let branchop =
      [| Some Eq
       ; Some Neq
       ; None
       ; None
       ; Some (Lt Signed)
       ; Some (Ge Signed)
       ; Some (Lt Unsigned)
       ; Some (Ge Unsigned)
      |]
    ;;

    (* Array of ALU ops indexed by funct3 (not including sra or sub which are funct7-determined) *)
    let aluop = [| Add; Sll; Slt; Sltu; Xor; Srl; Or; And |]
  end
end

(* Int32 helpers *)
(* Extract bit range from `min` to `max` (inclusive, zero-indexed) from an int32 as an int *)
let bits insn max min =
  Int32.to_int_exn
    (Int32.( land )
       (Int32.( lsr ) insn min)
       (Int32.of_int_exn (Int.pow 2 (max - min + 1) - 1)))
;;

(* Sign-extend an int with the given number of bits to an int32 *)
let of_int_sign ~w value =
  Int32.( asr ) (Int32.( lsl ) (Int32.of_int_trunc value) (32 - w)) (32 - w)
;;

(* Convert binary instruction to `insn` *)
let of_int32 insn =
  let opcode = bits insn 6 0 in
  let funct7 = bits insn 31 25 in
  let funct3 = bits insn 14 12 in
  let rd = bits insn 11 7 in
  let rs1 = bits insn 19 15 in
  let rs2 = bits insn 24 20 in
  let immi = of_int_sign ~w:12 (bits insn 31 20) in
  let imms = of_int_sign ~w:12 ((bits insn 31 25 * 0x20) + bits insn 11 7) in
  let immb =
    of_int_sign
      ~w:13
      ((bits insn 31 31 * 0x1000)
       + (bits insn 7 7 * 0x800)
       + (bits insn 30 25 * 0x20)
       + (bits insn 11 8 * 0x2))
  in
  let immj =
    of_int_sign
      ~w:21
      ((bits insn 31 31 * 0x100000)
       + (bits insn 19 12 * 0x1000)
       + (bits insn 20 20 * 0x800)
       + (bits insn 30 21 * 0x2))
  in
  let immu = of_int_sign ~w:32 (0x1000 * bits insn 31 12) in
  if opcode = Int32.to_int_exn Binary.Op.intR
  then
    Ok
      (IntReg
         ( (match Binary.Funct3.aluop.(funct3) with
            | Add when funct7 = 0x20 -> Sub
            | Srl when funct7 = 0x20 -> Sra
            | op -> op)
         , { rd; rs1; rs2 } ))
  else if opcode = Int32.to_int_exn Binary.Op.intI
  then (
    let op, mask =
      match Binary.Funct3.aluop.(funct3) with
      | Sll -> Sll, 0x1f (* Shifts only take 5 imm bits and don't sign-extend *)
      | Srl when funct7 = 0x20 -> Sra, 0x1f
      | Srl -> Srl, 0x1f
      | op -> op, -1 (* Others take all bits, sign-extended *)
    in
    Ok (IntImm (op, { rd; rs1; imm = Int32.(immi land of_int_exn mask) })))
  else (
    let mem_size () =
      match bits insn 13 12 with
      | 0 -> Byte
      | 1 -> Half
      | 2 -> Word
      | _ -> failwith "illegal instruction: mem size"
    in
    if opcode = Int32.to_int_exn Binary.Op.load
    then
      Ok
        (Load
           ( mem_size ()
           , (match bits insn 14 14 with
              | 0 -> Signed
              | 1 -> Unsigned
              | _ -> failwith "impossible")
           , { rd; rs1; imm = immi } ))
    else if opcode = Int32.to_int_exn Binary.Op.store
    then Ok (Store (mem_size (), { rs1; rs2; imm = imms }))
    else if opcode = Int32.to_int_exn Binary.Op.branch
    then
      Binary.Funct3.branchop.(funct3)
      |> Option.map ~f:(fun b -> Branch (b, { rs1; rs2; imm = immb }))
      |> Or_error.of_option ~error:(Error.of_string "illegal instruction: branch type")
    else if opcode = Int32.to_int_exn Binary.Op.jal
    then Ok (Jal { rd; imm = immj })
    else if opcode = Int32.to_int_exn Binary.Op.jalr
    then Ok (Jalr { rd; rs1; imm = immi })
    else if opcode = Int32.to_int_exn Binary.Op.lui
    then Ok (Lui { rd; imm = immu })
    else if opcode = Int32.to_int_exn Binary.Op.auiPc
    then Ok (AuiPc { rd; imm = immu })
    else if opcode = Int32.to_int_exn Binary.Op.env
    then (
      let csr = bits insn 31 20 in
      match funct3 with
      | 0 when Int32.equal insn (Int32.of_string "0x00000073") -> Ok Ecall
      | 0 when Int32.equal insn (Int32.of_string "0x00100073") -> Ok Ebreak
      | 0 when Int32.equal insn (Int32.of_string "0x30200073") -> Ok Mret
      | 0 when Int32.equal insn (Int32.of_string "0x10200073") -> Ok Sret
      | 1 -> Ok (Csr { op = Csrrw; rd; src = Reg rs1; csr })
      | 2 -> Ok (Csr { op = Csrrs; rd; src = Reg rs1; csr })
      | 3 -> Ok (Csr { op = Csrrc; rd; src = Reg rs1; csr })
      | 5 -> Ok (Csr { op = Csrrw; rd; src = Imm (Int32.of_int_exn rs1); csr })
      | 6 -> Ok (Csr { op = Csrrs; rd; src = Imm (Int32.of_int_exn rs1); csr })
      | 7 -> Ok (Csr { op = Csrrc; rd; src = Imm (Int32.of_int_exn rs1); csr })
      | _ -> Or_error.error_s [%message "illegal instruction: system" (insn : Int32.t)])
    else
      Or_error.error_s
        [%message "illegal instruction: opcode " (opcode : int) (insn : Int32.t)])
;;

let of_int32_exn = Fn.compose Or_error.ok_exn of_int32

(* Move a set of bits from an int32 to another location within the int32 *)
let move_bits value max_ min_ loc =
  let mask = Int32.of_int_exn (Int.pow 2 (max_ - min_ + 1) - 1) in
  Int32.(((value lsr min_) land mask) lsl loc)
;;

(* Convert `insn` to binary *)
let to_int32 =
  let regsr { rd; rs1; rs2 } =
    Int32.((of_int_exn rd lsl 7) + (of_int_exn rs1 lsl 15) + (of_int_exn rs2 lsl 20))
  in
  let regsi { rd; rs1; imm } =
    Int32.((of_int_exn rd lsl 7) + (of_int_exn rs1 lsl 15) + (imm lsl 20))
  in
  let regss { rs1; rs2; imm } =
    Int32.(
      move_bits imm 4 0 7
      + (of_int_exn rs1 lsl 15)
      + (of_int_exn rs2 lsl 20)
      + move_bits imm 11 5 25)
  in
  let regsb { rs1; rs2; imm } =
    Int32.(
      move_bits imm 11 11 7
      + move_bits imm 4 1 8
      + (of_int_exn rs1 lsl 15)
      + (of_int_exn rs2 lsl 20)
      + move_bits imm 10 5 25
      + move_bits imm 12 12 31)
  in
  let regsu { rd; imm } = Int32.((of_int_exn rd lsl 7) + move_bits imm 31 12 12) in
  let regsj { rd; imm } =
    Int32.(
      (of_int_exn rd lsl 7)
      + move_bits imm 19 12 12
      + move_bits imm 11 11 20
      + move_bits imm 10 1 21
      + move_bits imm 20 20 31)
  in
  let csr_src_value = function
    | Reg rs1 -> Int32.of_int_exn rs1
    | Imm imm -> Int32.(imm land of_int_exn 0x1f)
  in
  let csrop op = function
    | Reg _ ->
      (match op with
       | Csrrw -> 1
       | Csrrs -> 2
       | Csrrc -> 3)
    | Imm _ ->
      (match op with
       | Csrrw -> 5
       | Csrrs -> 6
       | Csrrc -> 7)
  in
  let aluop op =
    let op, extra =
      match op with
      | Sub -> Add, Int32.(of_int_exn 0x20 lsl 25)
      | Sra -> Srl, Int32.(of_int_exn 0x20 lsl 25)
      | op -> op, Int32.zero
    in
    let opbits, _ =
      Option.value_exn (Array.findi Binary.Funct3.aluop ~f:(fun _ o -> equal_aluop o op))
    in
    Int32.((of_int_exn opbits lsl 12) + extra)
  and mem_size s =
    Int32.(
      of_int_exn
        (match s with
         | Byte -> 0
         | Half -> 1
         | Word -> 2)
      lsl 12)
  and mem_sign s =
    Int32.(
      of_int_exn
        (match s with
         | Signed -> 0
         | Unsigned -> 1)
      lsl 14)
  and brop op =
    let f3 =
      match op with
      | Eq -> 0
      | Neq -> 1
      | Lt Signed -> 4
      | Ge Signed -> 5
      | Lt Unsigned -> 6
      | Ge Unsigned -> 7
    in
    Int32.(of_int_exn f3 lsl 12)
  in
  function
  | IntReg (op, r) -> Int32.( + ) Int32.(regsr r + aluop op) Binary.Op.intR
  | IntImm (op, r) -> Int32.( + ) Int32.(regsi r + aluop op) Binary.Op.intI
  | Load (size, sgn, r) ->
    (match size, sgn with
     | Word, Unsigned -> failwith "load-word-unsigned is not an instruction (use signed)"
     | _ -> Int32.( + ) Int32.(regsi r + mem_size size + mem_sign sgn) Binary.Op.load)
  | Store (size, r) -> Int32.( + ) Int32.(regss r + mem_size size) Binary.Op.store
  | Branch (op, r) -> Int32.( + ) Int32.(regsb r + brop op) Binary.Op.branch
  | Jal r -> Int32.( + ) (regsj r) Binary.Op.jal
  | Jalr r -> Int32.( + ) (regsi r) Binary.Op.jalr
  | Lui r -> Int32.( + ) (regsu r) Binary.Op.lui
  | AuiPc r -> Int32.( + ) (regsu r) Binary.Op.auiPc
  | Csr { op; rd; src; csr } ->
    Stdlib.Int32.add
      Int32.(
        (of_int_exn csr lsl 20)
        + (csr_src_value src lsl 15)
        + (of_int_exn rd lsl 7)
        + (of_int_exn (csrop op src) lsl 12))
      Binary.Op.env
  | Ecall -> Int32.of_string "0x00000073"
  | Ebreak -> Int32.of_string "0x00100073"
  | Mret -> Int32.of_string "0x30200073"
  | Sret -> Int32.of_string "0x10200073"
;;

(* Test bits *)
let%test "bits 1" = bits Int32.(of_string "0x00708093") 6 0 = 19
let%test "bits 2" = bits Int32.(of_string "0x00012980") 6 0 = 0
let%test "bits 3" = bits Int32.(of_string "0x7fc71317") 31 12 = 523377

let%test "of_int_sign 1" =
  of_int_sign ~w:32 (0x1000 * 523377) |> Int32.( = ) (Int32.of_int_exn 2143752192)
;;

(* Check some basic translation *)
let%test "binary nop" = Int32.( = ) (Int32.of_string "0x13") (to_int32 nop)

let%test "binary basic addi" =
  Int32.( = )
    (Int32.of_string "0x00700093")
    (to_int32 (IntImm (Add, { rd = 1; rs1 = 0; imm = Int32.of_int_exn 7 })))
;;

(* Check round-trips *)
let roundtrip insn = equal_insn (of_int32_exn (to_int32 insn)) insn
let roundtrip_bin b = Int32.( = ) (to_int32 (of_int32_exn b)) b
let%test "roundtrip nop" = roundtrip nop

let%test "roundtrip basic addi" =
  roundtrip (IntImm (Add, { rd = 1; rs1 = 1; imm = Int32.of_int_exn 12 }))
;;

let%test "roundtrip basic add" = roundtrip (IntReg (Add, { rd = 2; rs1 = 1; rs2 = 1 }))

let%test "roundtrip sw" =
  roundtrip (Store (Word, { rs1 = 2; rs2 = 7; imm = Int32.of_int_exn 2 }))
;;

let%test "roundtrip auipc" =
  roundtrip (AuiPc { rd = 6; imm = Int32.of_int_exn 2143752192 })
;;

let%test "roundtrip sra" =
  roundtrip (IntImm (Sra, { rd = 5; rs1 = 5; imm = Int32.of_int_exn 31 }))
;;

let%test "roundtrip csrrwi" =
  roundtrip (Csr { op = Csrrw; rd = 1; src = Imm (Int32.of_int_exn 5); csr = 0x123 })
;;

let%test "roundtrip system instructions" =
  List.for_all [ Ecall; Ebreak; Mret; Sret ] ~f:roundtrip
;;

(* let failing_insn = IntImm (Sra, {rd = 5; rs1 = 5; imm = Int32.of_int_exn 31})
let _ = Stdio.printf "Original sexp:  "; Stdio.print_s (sexp_of_insn failing_insn)
let _ = Stdio.printf "Original to binary: %08x\n" (Int32.to_int_exn (to_int32 failing_insn))
let _ = Stdio.printf "Roundtrip sexp: "; Stdio.print_s (sexp_of_insn (of_int32_exn (to_int32 failing_insn))) *)
