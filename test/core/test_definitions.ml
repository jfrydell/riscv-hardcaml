open! Core

module Basic = struct
  type t =
    { name : string
    ; program : Riscvemulate.insn list
    ; insn_count : int
    }

  let all =
    [ { name = "basic"
      ; program =
          Riscvemulate.
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

module Fuzz = struct
  type t =
    { name : string
    ; quickcheck_seed : string
    ; trials : int
    ; insn_count_low : int
    ; insn_count_high : int
    ; reg_max : int
    ; filter : Riscvemulate.insn -> bool
    }

  let small =
    { name = "small"
    ; quickcheck_seed = "small-fuzz"
    ; trials = 500
    ; insn_count_low = 10
    ; insn_count_high = 10
    ; reg_max = 4
    ; filter =
        (function
          | Riscvemulate.IntImm (Add, _) | Load _ | Store _ | Branch _ -> true
          | _ -> false)
    }
  ;;

  let hazards =
    { name = "hazards"
    ; quickcheck_seed = "hazard-fuzz"
    ; trials = 500
    ; insn_count_low = 100
    ; insn_count_high = 200
    ; reg_max = 4
    ; filter = Fn.const true
    }
  ;;

  let coverage =
    { name = "coverage"
    ; quickcheck_seed = "coverage-fuzz"
    ; trials = 500
    ; insn_count_low = 500
    ; insn_count_high = 2000
    ; reg_max = 32
    ; filter = Fn.const true
    }
  ;;

  let all = [ small; hazards; coverage ]
  let of_name name = List.find all ~f:(fun t -> String.equal t.name name)
  let of_name_exn name = of_name name |> Option.value_exn
end
