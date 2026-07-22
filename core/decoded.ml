(** Represent decoded instructions. *)

open! Core
open! Hardcaml

(** Register src/dst identifier, with 0 representing none. *)
module Regid = struct
  include Types.Scalar (struct
      let port_name = "regid"
      let port_width = 5
    end)
end

(** 4-bit ALU optype. The first 3 bits match funct3 for arithmetic instructions, and the
    last chooses between add/sub and sra/srl for ambiguous cases. *)
module Optype = struct
  include Types.Scalar (struct
      let port_name = "optype"
      let port_width = 4
    end)

  open Signal

  let add = Riscv.Funct3.add_or_sub @: gnd
  let sub = Riscv.Funct3.add_or_sub @: vdd
  let sll = Riscv.Funct3.sll @: gnd
  let slt = Riscv.Funct3.slt @: gnd
  let sltu = Riscv.Funct3.sltu @: gnd
  let xor = Riscv.Funct3.xor @: gnd
  let srl = Riscv.Funct3.srl_or_sra @: gnd
  let sra = Riscv.Funct3.srl_or_sra @: vdd
  let or' = Riscv.Funct3.or' @: gnd
  let and' = Riscv.Funct3.and' @: gnd

  (* Get optype from funct3 bits, along with the 2nd (to MSB) bit of funct7 and whether
   the instruction is an R-type or I-type arithmetic instruction (defaulting to 0 = add if neither). *)
  let of_funct3 ~arithr ~arithi ~f7second f3 =
    let is_sra = f3 ==: Riscv.Funct3.srl_or_sra &&: f7second in
    let is_sub = f3 ==: Riscv.Funct3.add_or_sub &&: f7second &&: arithr in
    mux2 (arithr ||: arithi) (f3 @: (is_sra ||: is_sub)) add
  ;;
end

type 'a t =
  { opcode : 'a Riscv.Op.t
  ; rs1 : 'a Regid.t
  ; rs2 : 'a Regid.t
  ; rd : 'a Regid.t
  ; imm : 'a [@bits 32]
  ; funct3 : 'a Riscv.Funct3.t
  ; funct7 : 'a [@bits 7]
  ; optype : 'a Optype.t
  ; is_csr : 'a
  }
[@@deriving hardcaml]
