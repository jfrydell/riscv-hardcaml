
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

(* Creates a new wire for each stage of the pipeline, returning a `pipe_signal` referencing these wires *)
let pipe_wires width =
  let wires = Array.init 5 ~f:(fun _ -> Signal.wire width) in
  (fun s -> wires.(stage_index s))


let cpu ~clock ~reset ~insn_in ~data_in =
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

  (* Fetch stage *)
  let next_pc = wire 32 in
  let pc_reg = reg regspec ~enable:vdd next_pc in
  let pc = forward_pipeline ~signal:pc_reg ~stage:F in
  let insn = forward_pipeline ~signal:insn_in ~stage:F ~default:(of_hex ~width:32 "00000013") in
  (* Next PC calculation. increment by 1 unless we are branching *)
  let branch_pc = wire 32 in
  next_pc <== mux2 branch_execute branch_pc (pc_reg +:. 1);

  (* Decode *)
  let decoded = Decode.decode (insn D) in

  (* Register file *)
  let reg_write = wire 32 in
  let reg_dest = wire 5 in
  let reg_srcs = [|decoded.rs1; decoded.rs2|] in
  let write_port =  { Write_port.write_clock = clock
                    ; write_address = reg_dest
                    ; write_data = reg_write
                    ; write_enable = reg_dest <>: zero 5 } in
  let read_ports = Array.map reg_srcs ~f:(fun rs ->
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
  let rsvals = Array.map2_exn reg_srcs regfile ~f:(fun rs rsv ->
    mux2 (rs ==:. 0) (of_int ~width:32 0) rsv
  ) in

  (* Place decoded values into pipeline *)
  let opcode = forward_pipeline ~signal:decoded.opcode ~stage:D
  and rs1 = forward_pipeline ~signal:decoded.rs1 ~stage:D
  and rs2 = forward_pipeline ~signal:decoded.rs2 ~stage:D
  and rd = forward_pipeline ~signal:decoded.rd ~stage:D
  and imm = forward_pipeline ~signal:decoded.imm ~stage:D
  and funct3 = forward_pipeline ~signal:decoded.funct3 ~stage:D
  and funct7 = forward_pipeline ~signal:decoded.funct7 ~stage:D in
  (* For rs vals, put original array into pipeline and create wires for bypassing (`rsvals` = unbypassed version) *)
  let rsvals = Array.map ~f:(fun v -> forward_pipeline ~signal:v ~stage:D) rsvals in
  let rs1val = pipe_wires 32 and rs2val = pipe_wires 32 in

  (* Execute stage *)
  (* Sources (with bypassing) *)
  let src1x = rs1val X in
  let src2x = mux2 ((opcode X ==: Riscv.Op.intR) |: (opcode X ==: Riscv.Op.branch)) (rs2val X) (imm X) in
  (* ALU optype selection based on opcode (TODO: lift to previous stage for timing probably) *)
  let optype = onehot_select [
    (* R-type: funct3 with add/sub and sra/srl determined by funct7 being 0x20 *)
    { valid = (opcode X) ==: Riscv.Op.intR;
      value = (funct3 X) @: (funct7 X ==: of_string "7'h20") };
    (* I-type: funct3 with sra/srl determined by upper bits of imm (but never sub, so must check funct3) *)
    { valid = (opcode X) ==: Riscv.Op.intI;
      value = (funct3 X) @: ((funct3 X ==: of_string "3'b101") &: ((imm X).:[11,5] ==: of_string "7'h20")) };
    (* Loads/stores: add offset *)
    { valid = (opcode X ==: Riscv.Op.load) |: ((opcode X) ==: Riscv.Op.store);
      value = of_bit_string "0000" };
    (* Branch eq/ne: subtract to check zero bit *)
    { valid = (opcode X ==: Riscv.Op.branch) &: ((funct3 X ==: Riscv.Funct3.beq) |: (funct3 X ==: Riscv.Funct3.bne));
      value = of_bit_string "0001" };
    (* Branch lt/ge: slt *)
    { valid = (opcode X ==: Riscv.Op.branch) &: ((funct3 X ==: Riscv.Funct3.blt) |: (funct3 X ==: Riscv.Funct3.bge));
      value = of_bit_string "0100" };
    (* Branch lt/ge unsigned: sltu *)
    { valid = (opcode X ==: Riscv.Op.branch) &: ((funct3 X ==: Riscv.Funct3.bltu) |: (funct3 X ==: Riscv.Funct3.bgeu));
      value = of_bit_string "0110" };
  ] in
  (* ALU *)
  let alu_result = Alu.alu src1x src2x optype in
  let writeval_x = forward_pipeline ~signal:alu_result ~stage:X in

  (* Branches *)
  (* We must branch (from execute (TODO: not always)) if a branch condition holds or we are doing a jump. *)
  branch_execute <== tree ~arity:2 ~f:(reduce ~f:(|:)) [
    (opcode X) ==: Riscv.Op.branch &: (funct3 X ==: Riscv.Funct3.beq) &: (~: (gnd ||: alu_result));
    (opcode X) ==: Riscv.Op.branch &: (funct3 X ==: Riscv.Funct3.bne) &: (gnd ||: alu_result);
    (opcode X) ==: Riscv.Op.branch &: ((funct3 X ==: Riscv.Funct3.blt) |: (funct3 X ==: Riscv.Funct3.bltu)) &: alu_result.:(0);
    (opcode X) ==: Riscv.Op.branch &: ((funct3 X ==: Riscv.Funct3.bge) |: (funct3 X ==: Riscv.Funct3.bgeu)) &: ~: alu_result.:(0);
    (opcode X) ==: Riscv.Op.jal;
    (opcode X) ==: Riscv.Op.jalr;
  ];
  (* Address to branch to is PC+imm for branches and jal, src1+imm for jalr *)
  branch_pc <== (mux2 (opcode X ==: Riscv.Op.jalr) src1x (pc X)) +: (imm X);


  (* Memory stage *)
  (* Generate fields: type (00 = no access, 10 = load, 11 = store); size (00 = byte, 01 = half, 10 = word); addr; data *)
  let load = (opcode M ==: Riscv.Op.load) in
  let store = (opcode M ==: Riscv.Op.store) in
  let mem_access = (load |: store) @: store in
  let mem_size = (funct3 M).:[1,0] in
  let mem_addr = writeval_x M in
  (* Grab mem data from rs2 for stores (imm used for ALU) *)
  let mem_data = (rs2val M) in

  (* Get data in from memory (TODO: have valid interface with stalling) *)
  let unsigned_extend = (funct3 M).:(2) in
  let loaded_val = mux mem_size [
    mux2 unsigned_extend (uresize data_in.:[7,0] 32) (sresize data_in.:[7,0] 32);
    mux2 unsigned_extend (uresize data_in.:[15,0] 32) (sresize data_in.:[15,0] 32);
    data_in
  ] in
  let writeval_m = forward_pipeline ~signal:(
    mux2 load loaded_val (writeval_x M)
  ) ~stage:M in


  (* Writeback stage *)
  reg_write <== (writeval_m W);
  reg_dest <== (rd W);


  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)? ultimately only two possible things to bypass;
  main thing to abstract over is which values came from rs1 and rs2 and when they were written to) *)
  (rs1val X) <== mux2 (rs1 X ==: rd M) (writeval_x M) (
                  mux2 (rs1 X ==: rd W) (writeval_m W) (
                  rsvals.(0) X));
  (rs2val X) <== mux2 (rs2 X ==: rd M) (writeval_x M) (
                  mux2 (rs2 X ==: rd W) (writeval_m W) (
                  rsvals.(1) X));
  (* for store data *)
  (rs2val M) <== mux2 (rs2 M ==: rd W) (writeval_m W) (rsvals.(1) M);


  (* TODO: Stall logic *)
  stall_decode <== gnd;


  pc_reg, mem_addr, mem_access, mem_size, mem_data
