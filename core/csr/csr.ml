open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; insn : 'a [@bits 32]
    ; rs1 : 'a [@bits 32]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { rdval : 'a [@bits 32]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; insn; rs1; valid } : _ I.t) =
  (* The six CSR instructions are identified by funct3.  The high bit selects
     the immediate forms, whose rs1 field is a five-bit unsigned immediate. *)
  let%hw csr_address = insn.:[31, 20] in
  let%hw funct3 = insn.:[14, 12] in
  let%hw rs1_address = insn.:[19, 15] in
  let%hw is_csrrw = funct3 ==: of_bit_string "001" in
  let%hw is_csrrs = funct3 ==: of_bit_string "010" in
  let%hw is_csrrc = funct3 ==: of_bit_string "011" in
  let%hw is_csr = is_csrrw |: is_csrrs |: is_csrrc |: (funct3.:(2) &: (funct3.:[1, 0] <>: zero 2)) in
  let%hw is_immediate = funct3.:(2) in
  let%hw operand = mux2 is_immediate (uresize ~width:32 rs1_address) rs1 in
  let%hw request = valid &: is_csr in
  (* [result_valid] is also the one-cycle busy bit.  The core holds the
     instruction in execute while it is low, and the request is accepted only
     once while it is high. *)
  let%hw result_valid = wire 1 in
  let%hw accepted = request &: ~:result_valid in
  result_valid <-- Types.Clocking.reg clocking accepted;
  (* Latch the request fields until the response cycle, when the RAM result is
     combined with the operand and written back to the CSR array. *)
  let%hw saved_address = Types.Clocking.reg clocking ~enable:accepted csr_address in
  let%hw saved_funct3 = Types.Clocking.reg clocking ~enable:accepted funct3 in
  let%hw saved_operand = Types.Clocking.reg clocking ~enable:accepted operand in
  let%hw write_data = wire 32 in
  let%hw write_enable =
    result_valid
    &: ((saved_funct3 ==: of_bit_string "001")
        |: (saved_funct3 ==: of_bit_string "101")
        |: ((saved_funct3 ==: of_bit_string "010") &: (saved_operand <>: zero 32))
        |: ((saved_funct3 ==: of_bit_string "011") &: (saved_operand <>: zero 32))
        |: ((saved_funct3 ==: of_bit_string "110") &: (saved_operand <>: zero 32))
        |: ((saved_funct3 ==: of_bit_string "111") &: (saved_operand <>: zero 32)))
  in
  let ram =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:4096
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable
           ; write_address = saved_address
           ; write_data
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = accepted
           ; read_address = csr_address
           }
        |]
      ~name:"csrs"
      ()
  in
  (* The write data is defined after the RAM output is available.  CSRRS(I)
     and CSRRC(I) with a zero operand are reads without a write, as specified
     by the ISA. *)
  let%hw old_value = ram.(0) in
  let%hw write_value =
    mux2
      ((saved_funct3 ==: of_bit_string "010")
       |: (saved_funct3 ==: of_bit_string "110"))
      (old_value |: saved_operand)
      (mux2
         ((saved_funct3 ==: of_bit_string "011")
          |: (saved_funct3 ==: of_bit_string "111"))
         (old_value &: ~:saved_operand)
         saved_operand)
  in
  write_data <-- write_value;
  O.{ rdval = old_value; valid = result_valid }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
