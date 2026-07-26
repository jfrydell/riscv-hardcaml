open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; rs1 : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = Csr_bank.Writes

let create scope ({ insn; rs1 } : _ I.t) =
  let%hw csr_address = insn.:[31, 20] in
  let%hw funct3 = insn.:[14, 12] in
  let%hw operand = mux2 funct3.:(2) (uresize ~width:32 insn.:[19, 15]) rs1 in
  let%hw operation = funct3.:[1, 0] in
  let write =
    ({ value =
         mux2
           (operation ==: of_bit_string "10")
           (ones 32)
           (mux2 (operation ==: of_bit_string "11") (zero 32) operand)
     ; mask =
         mux2
           (operation ==: of_bit_string "01")
           (ones 32)
           (mux2
              (operation ==: of_bit_string "10" |: (operation ==: of_bit_string "11"))
              operand
              (zero 32))
     }
     : _ Csr_bank.Write.t)
  in
  Csrs.map Csrs.addresses ~f:(fun address ->
    { Csr_bank.Write.value = write.value
    ; mask =
        write.mask &: repeat (csr_address ==: of_int_trunc ~width:12 address) ~count:32
    })
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
