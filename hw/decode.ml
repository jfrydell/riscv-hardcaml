
open Hardcaml

type decoded = {
  opcode: Signal.t;
  rs1: Signal.t;
  rs2: Signal.t;
  rd: Signal.t;
  imm: Signal.t;
  funct3: Signal.t;
  funct7: Signal.t;
}

(* Extracts registers read/written by an instruction (based on its opcode) (zero for unused regs),
as well as assembling its immediate and other fields.*)
let decode insn = let open Signal in
  (* Extract fields *)
  let opcode = insn.:[6,0] in
  let rs1 = insn.:[19,15] and rs2 = insn.:[24,20] and rd = insn.:[11,7] in
  let immi = sresize insn.:[31,20] 32 and imms = sresize (insn.:[31,25] @: insn.:[11,7]) 32
  and immb = sresize (insn.:(31) @: insn.:(7) @: insn.:[30,25] @: insn.:[11,8] @: gnd) 32
  and immu = sresize (insn.:[31,12] @: zero 12) 32
  and immj = sresize (insn.:(31) @: insn.:[19,12] @: insn.:(20) @: insn.:[30,21] @: gnd) 32 in

  (* Extract immediate based on opcode *)
  let imm = Util.muxmatch ~scrutinee:opcode ~cases:[
    [Riscv.Op.intI; Riscv.Op.load; Riscv.Op.jalr; Riscv.Op.env], immi;
    [Riscv.Op.store], imms;
    [Riscv.Op.branch], immb;
    [Riscv.Op.jal], immj;
    [Riscv.Op.lui; Riscv.Op.auiPc], immu;
  ] in

  (* Set which instructions read rs1, read rs2, and write to rd *)
  let regmask = Util.muxmatch ~scrutinee:opcode ~cases:[
    [Riscv.Op.intR; Riscv.Op.load], of_bit_string "111";
    [Riscv.Op.intI; Riscv.Op.jalr], of_bit_string "101";
    [Riscv.Op.store; Riscv.Op.branch], of_bit_string "110";
    [Riscv.Op.jal; Riscv.Op.lui; Riscv.Op.auiPc], of_bit_string "001";
    [Riscv.Op.env], of_bit_string "000";
  ] in

  (* Construct result *)
  {
    opcode = opcode -- "opcode";
    rs1 = mux2 regmask.:(2) rs1 (zero 5) -- "rs1";
    rs2 = mux2 regmask.:(1) rs2 (zero 5) -- "rs2";
    rd = mux2 regmask.:(0) rd (zero 5) -- "rd";
    funct3 = insn.:[14,12] -- "funct3";
    funct7 = insn.:[31,25] -- "funct7";
    imm = imm -- "imm";
  }
