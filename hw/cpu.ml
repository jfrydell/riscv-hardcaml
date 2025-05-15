
open Base
open Hardcaml

(* Interface *)
module I = struct
  type 'a t =
    { clock: 'a
    ; reset: 'a
    ; insn: 'a [@bits 32]
    ; data: 'a [@bits 32]
    }
  [@@deriving hardcaml]
end
module MemReq = struct
  type 'a t =
    { addr: 'a [@bits 32]
    (* 0 = no request, 1 = load/store *)
    ; valid: 'a
    ; store: 'a
    (* 00 = byte, 01 = half, 10 = word *)
    ; size: 'a [@bits 2]
    ; store_data: 'a [@bits 32]
    }
  [@@deriving hardcaml]
end
module O = struct
  type 'a t =
    { pc: 'a [@bits 32]
    ; access: 'a MemReq.t
    (* 1 if an instruction will be committed on the next cycle. Register file state will be in sync with this. *)
    ; will_commit: 'a
    }
  [@@deriving hardcaml]
end


type stage = F | D | X | M | W
let stage_index = function F -> 0 | D -> 1 | X -> 2 | M -> 3 | W -> 4
let index_stage = function 0 -> F | 1 -> D | 2 -> X | 3 -> M | 4 -> W | _ -> failwith "invalid stage"
let stage_names = [|"F"; "D"; "EX"; "M"; "W"|] (* EF for (approximate) alphabetical order (fetch doesn't matter much) *)

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
      let s = reg regspec ~enable:(~: (stall (index_stage si))) (mux2 (bubble (index_stage si)) default prev) in
      (* Stdio.printf "%d: " si; (* TODO why all the duplicate signals? *)
      Stdio.print_s (sexp_of_list sexp_of_string (names signal)); *)
      set_names s (List.map (names signal) ~f:(fun n -> stage_names.(si) ^ "." ^ n));
      s
    )

  (* Just using `get_signal` would rebuild pipeline on each access. Memoization avoids (while lazying building only needed latches) *)
  and cache = Array.create ~len:5 None
  and get_memo si =
    match cache.(si) with
    | Some w -> w
    | None -> cache.(si) <- Some (get_signal si); get_memo si

  in (fun s -> get_memo (stage_index s))

(* Creates a new wire for each stage of the pipeline, returning a `pipe_signal` referencing these wires *)
let pipe_wires name w =
  let wires = Array.init 5 ~f:(fun i -> Signal.(wire w -- (stage_names.(i) ^ "." ^ name))) in
  (fun s -> wires.(stage_index s))


(* Main CPU pipeline *)
let cpu (inputs: Signal.t I.t) =
  let open Signal in

  (* Pipeline stuff *)
  (* Pipeline regs are rising edge to work with Cyclesim (everything in core must be same edge). Insn/data mem will be opposite. *)
  let regspec = Reg_spec.create ~clock:inputs.clock ~reset:inputs.reset () in
  (* Stall signals: only ever stall decode (on hazard) and fetch (propagated from decode) *)
  let stall_decode = wire 1 in
  let stall = function
    | F | D -> stall_decode
    | X | M | W -> gnd in
  (* Send bubble to D,X on branch in execute, or to any on stall in previous stage *)
  let branch_execute = wire 1 -- "branch from execute" in
  let bubble s =
    let withoutstall = function D | X -> branch_execute | _ -> gnd in
    withoutstall s ||: stall (index_stage (stage_index s - 1))
  in
  let forward_pipeline ~signal ~stage ?default = forward_pipeline ~signal ~stage ~stall ~bubble ?default ~regspec in

  (* Fetch stage *)
  let next_pc = wire 32 -- "next_pc" in
  let pc_reg = reg regspec ~enable:vdd next_pc -- "pcreg" in
  let pc = forward_pipeline ~signal:pc_reg ~stage:F in
  let insn = forward_pipeline ~signal:(inputs.insn -- "insn") ~stage:F ~default:(of_hex ~width:32 "00000013") in
  let is_insn = forward_pipeline ~signal:(vdd -- "is_insn") ~stage:F in (* for tracking commits *)
  (* Next PC calculation. increment by 1 unless we are branching *)
  let branch_pc = wire 32 -- "branch_pc" in
  next_pc <== mux2 branch_execute branch_pc (pc_reg +:. 4);

  (* Decode *)
  let decoded = Decode.decode (insn D) in

  (* Register file *)
  let reg_write = wire 32 in
  let reg_dest = wire 5 in
  let reg_srcs = [|decoded.rs1; decoded.rs2|] in
  let regfile = Regfile.create Regfile.I.{
    rd = reg_dest;
    rdval = reg_write;
    rs = reg_srcs;
    clock = inputs.clock;
    reset = inputs.reset; (* WARN/TODO: unused in current implementation *)
  } in
  (* `rsvals` = unbypassed register values from register file *)
  let rsvals = Array.map ~f:(fun v -> forward_pipeline ~signal:v ~stage:D) regfile.rsval in

  (* Create wires for bypassed sources and written destination *)
  let rs1val = pipe_wires "rs1val" 32 and rs2val = pipe_wires "rs2val" 32 and rdval = pipe_wires "rdval" 32 in

  (* Place decoded values into pipeline *)
  let opcode = forward_pipeline ~signal:decoded.opcode ~stage:D
  and rs1 = forward_pipeline ~signal:decoded.rs1 ~stage:D
  and rs2 = forward_pipeline ~signal:decoded.rs2 ~stage:D
  and rd = forward_pipeline ~signal:decoded.rd ~stage:D
  and imm = forward_pipeline ~signal:decoded.imm ~stage:D
  and funct3 = forward_pipeline ~signal:decoded.funct3 ~stage:D
  and funct7 = forward_pipeline ~signal:decoded.funct7 ~stage:D in

  (* Execute stage *)
  (* First operand is rs1 (bypassed) usually, but PC for branch, jal, and auipc (lui takes rs1=0 for imm pass-through) *)
  let src1x = mux2
                ((opcode X ==: Riscv.Op.branch) |: (opcode X ==: Riscv.Op.jal) |: (opcode X ==: Riscv.Op.auiPc))
                (pc X) (rs1val X) -- "EX ALU src1" in
  (* Second is rs2 for R-type, imm otherwise (including branch, where rs2 is used for comparison while ALU calculates PC) *)
  let src2x = mux2
                (opcode X ==: Riscv.Op.intR)
                (rs2val X) (imm X) -- "EX ALU src2" in
  (* ALU optype selection based on opcode (TODO: lift to previous stage for timing probably) *)
  let optype = priority_select_with_default [
    (* R-type: funct3 with add/sub and sra/srl determined by funct7 being 0x20 *)
    { valid = (opcode X) ==: Riscv.Op.intR;
      value = (funct3 X) @: (funct7 X ==: of_string "7'h20") };
    (* I-type: funct3 with sra/srl determined by upper bits of imm (but never sub, so must check funct3) *)
    { valid = (opcode X) ==: Riscv.Op.intI;
      value = (funct3 X) @: ((funct3 X ==: of_string "3'b101") &: ((imm X).:[11,5] ==: of_string "7'h20")) };
  ] (* Otherwise (load, store, branch, jal(r), lui, luipc): just add *)
    ~default:(of_bit_string "0000") -- "EX optype" in
  (* ALU *)
  let alu_result = Alu.alu src1x src2x optype -- "EX ALU result" in

  (* Branches *)
  (* We must branch (from execute (TODO: not always)) if a branch condition holds or we are doing a jump. *)
  let branch_cond = mux (funct3 X) [
    (rs1val X) ==: (rs2val X); (* beq = 0 *)
    (rs1val X) <>: (rs2val X); (* bne = 1 *)
    gnd; gnd; (* 2,3 unused *)
    (rs1val X) <+ (rs2val X); (* blt = 4 *)
    (rs1val X) >=+ (rs2val X); (* bge = 5 *)
    (rs1val X) <: (rs2val X); (* bltu = 6 *)
    (rs1val X) >=: (rs2val X); (* bgeu = 7 *)
  ] in
  branch_execute <== reduce ~f:(|:) [
    (opcode X) ==: Riscv.Op.branch &: branch_cond;
    (opcode X) ==: Riscv.Op.jal;
    (opcode X) ==: Riscv.Op.jalr;
  ];
  (* Address to branch to is computed by ALU (PC+imm for branch, jal; rs1+imm for jalr) *)
  branch_pc <== alu_result;

  (* Pass written value to M for writeback, bypassing, and address computation (each only where applicable) *)
  rdval M <== forward_pipeline ~signal:(
    (* If jal or jalr, then PC+4; otherwise from ALU (auipc and lui implemented in ALU src
    selection, with lui's rs1=0 ensuring imm passes through ALU unchanged) *)
    mux2 ((opcode X ==: Riscv.Op.jal) |: (opcode X ==: Riscv.Op.jalr)) (pc X +: of_string "32'd4") alu_result
  ) ~stage:X M;


  (* Memory stage *)
  let load = (opcode M ==: Riscv.Op.load) in
  let store = (opcode M ==: Riscv.Op.store) in
  (* Generate fields: type (00 = no access, 10 = load, 11 = store); size (00 = byte, 01 = half, 10 = word); addr; data *)
  let access = MemReq.{
    addr = rdval M;
    size = (funct3 M).:[1,0];
    valid = load |: store;
    store;
    store_data = rs2val M;
  } in

  (* Get data in from memory (TODO: have valid interface with stalling) *)
  let unsigned_extend = (funct3 M).:(2) in
  let loaded_val = mux access.size [
    mux2 unsigned_extend (uresize inputs.data.:[7,0] 32) (sresize inputs.data.:[7,0] 32);
    mux2 unsigned_extend (uresize inputs.data.:[15,0] 32) (sresize inputs.data.:[15,0] 32);
    inputs.data
  ] in

  (* Value in rd at W is either that from X or value from memory *)
  rdval W <== forward_pipeline ~signal:(
    mux2 load loaded_val (rdval M)
  ) ~stage:M W;


  (* Writeback stage *)
  reg_write <== (rdval W);
  reg_dest <== (rd W);


  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)? ultimately only two possible things to bypass;
  main thing to abstract over is which values came from rs1 and rs2 and when they were written to) *)
  (rs1val X) <== mux2 ((rs1 X ==: rd M) &: (rs1 X <>: zero 5)) (rdval M) (
                  mux2 ((rs1 X ==: rd W) &: (rs1 X <>: zero 5)) (rdval W) (
                  rsvals.(0) X));
  (rs2val X) <== mux2 ((rs2 X ==: rd M) &: (rs2 X <>: zero 5)) (rdval M) (
                  mux2 ((rs2 X ==: rd W) &: (rs2 X <>: zero 5)) (rdval W) (
                  rsvals.(1) X));
  (* for store data *)
  (rs2val M) <== mux2 ((rs2 M ==: rd W) &: (rs2 M <>: zero 5)) (rdval W) (rsvals.(1) M);


  (* Stall logic: stall only needed for load-use hazard (load in X when consuming instruction in D) *)
  let reg_is_load_dest rs = (opcode X ==: Riscv.Op.load) &: (rs ==: rd X) &: (rs <>: zero 5) in
  stall_decode <== (reg_is_load_dest (rs1 D) |: reg_is_load_dest (rs2 D));

  O.{
    pc = pc_reg;
    access;
    will_commit = is_insn W;
  }
