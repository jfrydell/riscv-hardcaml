open! Core
module Insn = Riscv_isa.Insn

module Basic = struct
  type t =
    { name : string
    ; program : Insn.insn list
    ; insn_count : int
    }

  let all =
    [ { name = "basic"
      ; program =
          Insn.
            [ IntImm (Add, { rd = 1; rs1 = 0; imm = Int32.of_int_exn 7 })
            ; IntReg (Add, { rd = 2; rs1 = 1; rs2 = 1 })
            ; Store (Half, { rs1 = 2; rs2 = 1; imm = Int32.of_int_exn (-14) })
            ; Load (Half, Unsigned, { rd = 3; rs1 = 2; imm = Int32.of_int_exn (-14) })
            ]
      ; insn_count = 4
      }
    ]
  ;;

  let get index = List.nth all index
  let get_exn index = get index |> Option.value_exn
end
