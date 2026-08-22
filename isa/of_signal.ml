(** Hardcaml [Signal] values for RISC-V ISA.

    TODO: likely can consolidate with other parts of this library (initially were in
    completely separate directories) *)

open! Core
open Hardcaml
open Signal

module Op = struct
  include Types.Scalar (struct
      let port_name = "opcode"
      let port_width = 7
    end)

  let intR = of_bit_string "0110011"
  let intI = of_bit_string "0010011"
  let load = of_bit_string "0000011"
  let store = of_bit_string "0100011"
  let branch = of_bit_string "1100011"
  let jal = of_bit_string "1101111"
  let jalr = of_bit_string "1100111"
  let lui = of_bit_string "0110111"
  let auiPc = of_bit_string "0010111"
  let fence = of_bit_string "0001111"
  let env = of_bit_string "1110011"
end

(** Exact encodings of the supported non-CSR system instructions. *)
module System_insn = struct
  let ecall = of_hex ~width:32 "00000073"
  let ebreak = of_hex ~width:32 "00100073"
  let mret = of_hex ~width:32 "30200073"
  let sret = of_hex ~width:32 "10200073"
  let fencei = of_hex ~width:32 "0000100f"
end

(** Values of funct3 for branches and ALU operations. *)
module Funct3 = struct
  include Types.Scalar (struct
      let port_name = "funct3"
      let port_width = 3
    end)

  (* Branch types *)
  let beq = of_bit_string "000"
  let bne = of_bit_string "001"
  let blt = of_bit_string "100"
  let bge = of_bit_string "101"
  let bltu = of_bit_string "110"
  let bgeu = of_bit_string "111"

  (* ALU ops *)
  let add_or_sub = of_bit_string "000"
  let sll = of_bit_string "001"
  let slt = of_bit_string "010"
  let sltu = of_bit_string "011"
  let xor = of_bit_string "100"
  let srl_or_sra = of_bit_string "101"
  let or' = of_bit_string "110"
  let and' = of_bit_string "111"
end
