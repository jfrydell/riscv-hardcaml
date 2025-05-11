
open Base
open Hardcaml

type stage = F | D | X | M | W
let stage_index = function F -> 0 | D -> 1 | X -> 2 | M -> 3 | W -> 4
let index_stage = function 0 -> F | 1 -> D | 2 -> X | 3 -> M | 4 -> W | _ -> failwith "invalid stage"

type pipe_signal = stage -> Signal.t

(* Forwards a signal through pipeline registers. Parameters:
- The signal to forward through the pipeline
- The stage in which this signal is generated
- For each stage, a 1-bit signal indicating that stage stalls, keeping its previous signal instead of the one from the previous stage
- For each stage, a 1-bit signal indicating it bubbles, taking on a given default value instead of the one from the previous stage
- The default value to take on a bubble or on a reset (defaults to 0)
Note that bubbles and stalls do not propagate backwards automatically. The most common case is stage N bubbling and all stages <N stalling, but
this must be implemented in the bubble and stall signals.
If a stage is marked as both bubbling and stalling, it will stall (allows setting bubble N = stall N-1 as default).
Produces a `pipe_signal`, i.e. a function mapping a stage to a signal from these registers.
*)
let forward_pipeline ~signal ~stage ~stall ~bubble ?default:(default=Signal.zero (Signal.width signal)) ~regspec =
  (* Signal in stage N is just input if N was inject stage, otherwise add register with value N-1 as input.
  Easily implemented with simple recursion: *)
  let rec get_signal si =
    if si < (stage_index stage) then failwith "Tried to extract signal from pipeline before it was added"
    else if si = (stage_index stage) then signal
    else let prev = get_memo (si-1) in Signal.(
      reg regspec ~enable:(~: (stall (index_stage si))) (mux2 (bubble (index_stage si)) default prev)
    )

  (* Just using `get_signal` would rebuild pipeline on each access. Memoization avoids (while lazying building only needed latches) *)
  and cache = Array.create ~len:5 None
  and get_memo si =
    match cache.(si) with
    | Some w -> w
    | None -> cache.(si) <- Some (get_signal si); get_memo si

  in (fun s -> get_memo (stage_index s))


(* Integer ALU. Takes in two inputs and an optype, which is funct3 with additional bit differentiating srl from sra and add from sub.
Specifically:
- 000 0 = add
- 000 1 = sub
- 001 x = sll
- 010 x = slt
- 011 x = sltu
- 100 x = xor
- 101 0 = sra
- 101 1 = srl
- 110 x = or
- 111 x = and
Could expand to other operations by requiring some x's to be 0s (wouldn't affect existing instructions).
*)
let alu src1 src2 optype =
  let open Signal in

  let op3 = optype.:[3,1] and op1 = optype.:(0) in

  (* Adder; subtractor if sub (via op1 since sra doesn't use) or slt(u) *)
  let add_in_2 = mux2 (op1 |: (op3 ==: of_string "3'h2") |: (op3 ==: of_string "3'h3")) (negate src1) src2 in
  let rel_add = src1 +: add_in_2 in

  (* Logic functions *)
  let rel_and = src1 &: src2
  and rel_or = src1 |: src2
  and rel_xor = src1 ^: src2
  and rel_sll = log_shift sll src2 src2.:[4,0]
  and rel_srl = log_shift srl src1 src2.:[4,0]
  and rel_sra = log_shift sra src1 src2.:[4,0] in

  (* Set less than based on signs (avoids overflow in adder) *)
  let rel_slt_sltu = mux (src1.:(31) @: src2.:(31)) [
    (* Both positive: just check sign bit for both, no overflow possible *)
    rel_add.:(31) @: rel_add.:(31) ;
    (* First positive, second negative: greater if signed and less if unsigned *)
    of_bit_string "01" ;
    (* First negative, second positive: less if signed and greater if unsigned *)
    of_bit_string "10" ;
    (* Both negative: same as both positive (just added/subtracted cancelling 2^31 to both) *)
    rel_add.:(31) @: rel_add.:(31)
  ] in

  (* Compute result with funct3 *)
  mux op3 [
    rel_add;                      (* 0: add/sub *)
    rel_sll;                      (* 1: sll *)
    uresize rel_slt_sltu.:(1) 32; (* 2: slt *)
    uresize rel_slt_sltu.:(0) 32; (* 3: sltu *)
    rel_xor;                      (* 4: xor *)
    mux2 op1 rel_sra rel_srl;     (* 5: sra/srl *)
    rel_or;                       (* 6: or *)
    rel_and;                      (* 7: and *)
  ]


let cpu ~clock ~reset ~insn_in =
  let open Signal in

  (* Pipeline stuff *)
  (* Pipeline regs are falling edge *)
  let regspec = Reg_spec.create ~clock ~reset () |> Reg_spec.override ~clock_edge:(Edge.Falling) in
  (* Stall signals: only ever stall decode (on hazard) and fetch (propagated from decode) *)
  let stall_decode = wire 1 in
  let stall = function
    | F | D -> stall_decode
    | X | M | W -> gnd in
  (* Send bubble to D,X on branch in execute, or to any on stall in previous stage *)
  let branch_execute = wire 1 in
  let bubble s =
    let withoutstall = function D | X -> branch_execute | _ -> gnd in
    withoutstall s ||: stall (index_stage (stage_index s - 1))
  in
  let forward_pipeline ~signal ~stage ?default = forward_pipeline ~signal ~stage ~stall ~bubble ?default ~regspec in

  (* Make fetch stage *)
  let next_pc = wire 32 in
  let pc_reg = reg regspec ~enable:vdd next_pc in
  let pc = forward_pipeline ~signal:pc_reg ~stage:F in
  let insn = forward_pipeline ~signal:insn_in ~stage:F ~default:(of_hex ~width:32 "00000013") in
  (* Next PC calculation. increment by 1 unless we are branching *)
  let branch_pc = wire 32 in
  next_pc <== mux2 branch_execute branch_pc (pc_reg +:. 1);

  (* Decode: extract immediate and/or register designators (only used in decode; inserted separately into pipeline based on opcode post-decode) *)
  let insnd = insn D in
  let opcode s = (insn s).:[6,0] in
  let rs = [|insnd.:[19,15]; insnd.:[24,20]|] in
  let rd = insnd.:[11,7] in
  let immi = sresize insnd.:[31,20] 32 and imms = sresize (insnd.:[31,25] @: insnd.:[11,7]) 32
  and immb = sresize (insnd.:(31) @: insnd.:(7) @: insnd.:[30,25] @: insnd.:[11,8] @: gnd) 32
  and immu = sresize (insnd.:[31,12] @: zero 12) 32
  and immj = sresize (insnd.:(31) @: insnd.:[19,12] @: insnd.:(20) @: insnd.:[30,21] @: gnd) 32 in

  (* Register file *)
  let reg_write = {With_valid.value = wire 32; valid = wire 1} in
  let write_port =  { Write_port.write_clock = clock
                    ; write_address = rd
                    ; write_data = reg_write.value
                    ; write_enable = reg_write.valid } in
  let read_ports = Array.map rs ~f:(fun rs ->
      { Read_port.read_clock = clock
      ; read_address = rs
      ; read_enable = vdd }
    ) in
  let regfile = Ram.create
        ~collision_mode:Write_before_read
        ~size:32
        ~write_ports:[|write_port|]
        ~read_ports
        () in
  let rsvals = Array.map2_exn rs regfile ~f:(fun rs rsv ->
    mux2 (rs ==:. 0) (of_int ~width:32 0) rsv
  ) in

  (* Place relevant values into pipeline. In particular, need to lookup type of instruction for 2nd input (rs2 or imm of various types) *)
  (* TODO: only set regs that are actually written by insn.
  TODO: same with inputs; those are trickier than anticipated because rs2 can be used in many places (likely don't want just one pipeline slot) *)
  let rs1 = forward_pipeline ~signal:rs.(0) ~stage:D
  and rs2 = forward_pipeline ~signal:rs.(1) ~stage:D
  and rd = forward_pipeline ~signal:rd ~stage:D in
  let src1 = forward_pipeline ~signal:rsvals.(0) ~stage:D
  and src2 =
    let src_ops = [
      rsvals.(1), [of_bit_string "0110011"; of_bit_string "1100011"]; (* For B-type (branch), ALU takes reg and immb is used elsewhere *)
      immi, [of_bit_string "0010011"; of_bit_string "0000011"; of_bit_string "1110011"; of_bit_string "1100111"];
      imms, [of_bit_string "0100011"];
      immj, [of_bit_string "1101111"];
      immu, [of_bit_string "0110111"];
    ] in
    let src = List.map ~f:(fun (src, opcodes) ->
      {
        With_valid.value = src;
        (* Source should be chosen if opcode matches one of its  *)
        valid = opcodes |> List.map ~f:(fun o -> (opcode D) ==: o) |> tree ~arity:2 ~f:(reduce ~f:(|:))
      }
    ) src_ops |> priority_select in
    forward_pipeline ~signal:src.value ~stage:D in
  (* opcode, func bits come directly from instruction (TODO: injecting separately would save some reg bits because rest of instruction unused?) *)
  let opcode s = (insn s).:[6,0] in
  let funct3 s = (insn s).:[14,12] and funct7 s = (insn s).:[31,25] in

  (* Execute stage *)
  (* Sources with bypassing *)
  let src1x = wire 32 and src2x = wire 32 in
  (* ALU optype selection based on opcode (TODO: lift to previous stage for timing probably) *)
  let optype = onehot_select [
    (* R-type: funct3 with add/sub and sra/srl determined by funct7 being 0x20 *)
    { valid = (opcode X) ==: Riscv.opIntR;
      value = (funct3 X) @: (funct7 X ==: of_string "7'h20") };
    (* I-type: funct3 with sra/srl determined by upper bits of imm (but never sub, so must check funct3) *)
    { valid = (opcode X) ==: Riscv.opIntI;
      value = (funct3 X) @: ((funct3 X ==: of_string "3'b101") &: ((src2 X).:[11,5] ==: of_string "7'h20")) };
    (* Loads/stores: add offset *)
    { valid = (opcode X ==: Riscv.opLoad) |: ((opcode X) ==: Riscv.opStore);
      value = of_bit_string "0000" };
    (* Branch eq/ne: subtract to check zero bit *)
    { valid = (opcode X ==: Riscv.opBranch) &: ((funct3 X ==: Riscv.Funct3.beq) |: (funct3 X ==: Riscv.Funct3.bne));
      value = of_bit_string "0001" };
    (* Branch lt/ge: slt *)
    { valid = (opcode X ==: Riscv.opBranch) &: ((funct3 X ==: Riscv.Funct3.blt) |: (funct3 X ==: Riscv.Funct3.bge));
      value = of_bit_string "0100" };
    (* Branch lt/ge unsigned: sltu *)
    { valid = (opcode X ==: Riscv.opBranch) &: ((funct3 X ==: Riscv.Funct3.bltu) |: (funct3 X ==: Riscv.Funct3.bgeu));
      value = of_bit_string "0110" };
  ] in
  (* ALU *)
  let alu_result = alu src1x src2x optype in
  let writeval_d = forward_pipeline ~signal:alu_result ~stage:X in

  (* Branches *)
  (* We must branch (from execute (TODO: not always)) if a branch condition holds or we are doing a jump. *)
  branch_execute <== tree ~arity:2 ~f:(reduce ~f:(|:)) [
    (opcode X) ==: Riscv.opBranch &: (funct3 X ==: Riscv.Funct3.beq) &: (~: (gnd ||: alu_result));
    (opcode X) ==: Riscv.opBranch &: (funct3 X ==: Riscv.Funct3.bne) &: (gnd ||: alu_result);
    (opcode X) ==: Riscv.opBranch &: ((funct3 X ==: Riscv.Funct3.blt) |: (funct3 X ==: Riscv.Funct3.bltu)) &: alu_result.:(0);
    (opcode X) ==: Riscv.opBranch &: ((funct3 X ==: Riscv.Funct3.bge) |: (funct3 X ==: Riscv.Funct3.bgeu)) &: ~: alu_result.:(0);
    (opcode X) ==: Riscv.opJal;
    (opcode X) ==: Riscv.opJalr;
  ];
  (* Address to branch to is PC+imm for branches and jal, src1+imm for jalr. But imm actually isn't `src2`
  for B-type branches as they use rs2 for comparison. So grab that from the pipeline where necessary. *)
  let branch_imm = mux2 (opcode X ==: Riscv.opBranch) (forward_pipeline ~signal:immb ~stage:D X) (src2 X) in
  branch_pc <== (mux2 (opcode X ==: Riscv.opJalr) src1x (pc X)) +: branch_imm;


  (* TODO: Memory stage *)
  let writeval_m = forward_pipeline ~signal:(writeval_d M) ~stage:M in


  (* TODO: Writeback stage *)
  reg_write.value <== (writeval_m W);
  reg_write.valid <== vdd;


  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)? ultimately only two possible things to bypass;
  main thing to abstract over is which values came from rs1 and rs2 and when they were written to) *)
  src1x <== mux2 (rs1 X ==: rd M) (writeval_d M) (
            mux2 (rs1 X ==: rd W) (writeval_m W) (
            src1 X));
  src2x <== mux2 (rs2 X ==: rd M) (writeval_d M) (
            mux2 (rs2 X ==: rd W) (writeval_m W) (
            src2 X));


  (* TODO: Stall logic *)
  stall_decode <== gnd;


  pc_reg
