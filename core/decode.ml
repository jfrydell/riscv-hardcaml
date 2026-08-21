open Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; privilege : 'a [@bits 32]
    ; mstatus : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = Decoded

(* Extracts registers read/written by an instruction (based on its opcode) (zero for unused regs),
as well as assembling its immediate and other fields.*)
let create scope ({ insn; privilege; mstatus } : _ I.t) =
  (* Extract fields *)
  let%hw opcode = insn.:[6, 0] in
  let%hw rs1 = insn.:[19, 15] in
  let%hw rs2 = insn.:[24, 20] in
  let%hw rd = insn.:[11, 7] in
  let%hw immi = sresize ~width:32 insn.:[31, 20] in
  let%hw imms = sresize ~width:32 (insn.:[31, 25] @: insn.:[11, 7]) in
  let%hw immb =
    sresize ~width:32 (insn.:(31) @: insn.:(7) @: insn.:[30, 25] @: insn.:[11, 8] @: gnd)
  in
  let%hw immu = sresize ~width:32 (insn.:[31, 12] @: zero 12) in
  let%hw immj =
    sresize ~width:32 (insn.:(31) @: insn.:[19, 12] @: insn.:(20) @: insn.:[30, 21] @: gnd)
  in
  let funct3 = insn.:[14, 12] in
  let%hw is_env = opcode ==: Riscv_isa.Of_signal.Op.env in
  let%hw is_csr =
    is_env &&: (drop_top ~width:1 funct3 <>:. 0)
  in
  let%hw is_csr_imm = is_csr &&: msb funct3 in
  (* Extract immediate based on opcode *)
  let%hw imm =
    cases ~default:(zero 32) opcode
    @@ List.concat_map
         [ ( [ Riscv_isa.Of_signal.Op.intI
             ; Riscv_isa.Of_signal.Op.load
             ; Riscv_isa.Of_signal.Op.jalr
             ; Riscv_isa.Of_signal.Op.env
             ]
           , immi )
         ; [ Riscv_isa.Of_signal.Op.store ], imms
         ; [ Riscv_isa.Of_signal.Op.branch ], immb
         ; [ Riscv_isa.Of_signal.Op.jal ], immj
         ; [ Riscv_isa.Of_signal.Op.lui; Riscv_isa.Of_signal.Op.auiPc ], immu
         ]
         ~f:(fun (s, v) -> List.map s ~f:(fun s -> s, v))
  in
  (* Set which instructions read rs1, read rs2, and write to rd *)
  let%hw regmask =
    cases ~default:(zero 3) opcode
    @@ List.concat_map
         [ ( [ Riscv_isa.Of_signal.Op.intR; Riscv_isa.Of_signal.Op.load ]
           , of_bit_string "111" )
         ; ( [ Riscv_isa.Of_signal.Op.intI; Riscv_isa.Of_signal.Op.jalr ]
           , of_bit_string "101" )
         ; ( [ Riscv_isa.Of_signal.Op.store; Riscv_isa.Of_signal.Op.branch ]
           , of_bit_string "110" )
         ; ( [ Riscv_isa.Of_signal.Op.jal
             ; Riscv_isa.Of_signal.Op.lui
             ; Riscv_isa.Of_signal.Op.auiPc
             ]
           , of_bit_string "001" )
         ; ( [ Riscv_isa.Of_signal.Op.env ]
           , mux2
               is_csr
               (mux2 is_csr_imm (of_bit_string "001") (of_bit_string "101"))
               (of_bit_string "000") )
         ]
         ~f:(fun (s, v) -> List.map s ~f:(fun s -> s, v))
  in
  let%hw is_int_r = opcode ==: Riscv_isa.Of_signal.Op.intR in
  let%hw is_int_i = opcode ==: Riscv_isa.Of_signal.Op.intI in
  let%hw is_load = opcode ==: Riscv_isa.Of_signal.Op.load in
  let%hw is_store = opcode ==: Riscv_isa.Of_signal.Op.store in
  let%hw is_branch = opcode ==: Riscv_isa.Of_signal.Op.branch in
  let%hw is_jalr = opcode ==: Riscv_isa.Of_signal.Op.jalr in
  let%hw is_jal = opcode ==: Riscv_isa.Of_signal.Op.jal in
  let%hw is_lui = opcode ==: Riscv_isa.Of_signal.Op.lui in
  let%hw is_auipc = opcode ==: Riscv_isa.Of_signal.Op.auiPc in
  let%hw is_system = is_env &&: ~:(is_csr) in
  let funct7 = insn.:[31, 25] in
  (* Construct result *)
  let decoded =
    ({ opcode
     ; is_int_r
     ; is_int_i
     ; is_load
     ; is_store
     ; is_branch
     ; is_jalr
     ; is_jal
     ; is_lui
     ; is_auipc
     ; is_env
     ; is_system
     ; rs1 = mux2 regmask.:(2) rs1 (zero 5)
     ; rs2 = mux2 regmask.:(1) rs2 (zero 5)
     ; rd = mux2 regmask.:(0) rd (zero 5)
     ; funct3
     ; funct7
     ; imm
     ; optype =
         Decoded.Optype.of_funct3
           ~arithr:is_int_r
           ~arithi:is_int_i
           ~f7second:funct7.:(5)
           funct3
     ; is_csr
     ; csr_addr = insn.:[31, 20]
     ; csr_writes =
         funct3.:[1, 0] ==:. 1 ||: (funct3.:[1, 0] >=:. 2 &&: (insn.:[19, 15] <>:. 0))
     ; is_ecall = insn ==: Riscv_isa.Of_signal.System_insn.ecall
     ; is_ebreak = insn ==: Riscv_isa.Of_signal.System_insn.ebreak
     ; is_mret = insn ==: Riscv_isa.Of_signal.System_insn.mret
     ; is_sret = insn ==: Riscv_isa.Of_signal.System_insn.sret
     ; is_illegal = gnd
     ; result_in_m = is_load ||: is_csr
     ; rs2_not_used_until_m = is_store
     }
      : _ O.t)
  in
  let%hw.Detect_illegal.O.Of_signal illegal =
    Detect_illegal.hierarchical ~scope { decoded; privilege; mstatus }
  in
  { decoded with is_illegal = illegal.illegal }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
