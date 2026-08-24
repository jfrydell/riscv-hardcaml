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

module Fencei = struct
  (* Address 8 initially contains [halt], so it is fetched and cached before the
     store at address 0 changes it.  [fence.i] must invalidate that cached copy. *)
  let addi = Insn.IntImm (Add, { rd = 3; rs1 = 0; imm = Int32.of_int_exn 42 })
  let halt = Insn.Branch (Eq, { rs1 = 0; rs2 = 0; imm = Int32.zero })

  let program =
    [ Insn.Store (Word, { rs1 = 1; rs2 = 2; imm = Int32.zero }); Insn.Fencei; halt; halt ]
  ;;

  (* The first instruction stores [addi] to address 8. *)
  let initial_registers = [ 1, Int32.of_int_exn 8; 2, Insn.to_int32 addi ]
end
