open Core
open Hardcaml
open Signal

module I = Types.Scalar (struct
    let port_name = "regid"
    let port_width = 32
  end)

module O = Decoded

(* Extracts registers read/written by an instruction (based on its opcode) (zero for unused regs),
as well as assembling its immediate and other fields.*)
let create scope (insn : _ I.t) =
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
  (* Extract immediate based on opcode *)
  let%hw imm =
    cases ~default:(zero 32) opcode
    @@ List.concat_map
         [ [ Riscv.Op.intI; Riscv.Op.load; Riscv.Op.jalr; Riscv.Op.env ], immi
         ; [ Riscv.Op.store ], imms
         ; [ Riscv.Op.branch ], immb
         ; [ Riscv.Op.jal ], immj
         ; [ Riscv.Op.lui; Riscv.Op.auiPc ], immu
         ]
         ~f:(fun (s, v) -> List.map s ~f:(fun s -> s, v))
  in
  (* Set which instructions read rs1, read rs2, and write to rd *)
  let regmask =
    cases ~default:(zero 3) opcode
    @@ List.concat_map
         [ [ Riscv.Op.intR; Riscv.Op.load ], of_bit_string "111"
         ; [ Riscv.Op.intI; Riscv.Op.jalr ], of_bit_string "101"
         ; [ Riscv.Op.store; Riscv.Op.branch ], of_bit_string "110"
         ; [ Riscv.Op.jal; Riscv.Op.lui; Riscv.Op.auiPc ], of_bit_string "001"
         ; [ Riscv.Op.env ], of_bit_string "000"
         ]
         ~f:(fun (s, v) -> List.map s ~f:(fun s -> s, v))
  in
  let funct3 = insn.:[14, 12] in
  let funct7 = insn.:[31, 25] in
  (* Construct result *)
  ({ opcode
   ; rs1 = mux2 regmask.:(2) rs1 (zero 5)
   ; rs2 = mux2 regmask.:(1) rs2 (zero 5)
   ; rd = mux2 regmask.:(0) rd (zero 5)
   ; funct3 = insn.:[14, 12]
   ; funct7 = insn.:[31, 25]
   ; imm
   ; optype =
       Decoded.Optype.of_funct3
         ~arithr:(opcode ==: Riscv.Op.intR)
         ~arithi:(opcode ==: Riscv.Op.intI)
         ~f7top:(msb funct7)
         funct3
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
