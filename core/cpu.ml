open Base
open Hardcaml
open Signal

(* Interface *)
module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; from_l1d : 'a Memory.L1d_cache.To_pipe.t
    ; from_l1i : 'a Memory.L1i_cache.To_pipe.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_l1i : 'a Memory.L1i_cache.From_pipe.t
    ; to_l1d : 'a Memory.L1d_cache.From_pipe.t
    ; commit_pc : 'a With_valid.t [@bits 32]
    (** The PC of the instruction committing this cycle, when valid. *)
    }
  [@@deriving hardcaml]
end

type stage =
  | F
  | D
  | X
  | M
  | W

let stage_index = function
  | F -> 0
  | D -> 1
  | X -> 2
  | M -> 3
  | W -> 4
;;

let index_stage = function
  | 0 -> F
  | 1 -> D
  | 2 -> X
  | 3 -> M
  | 4 -> W
  | _ -> failwith "invalid stage"
;;

let stage_names = [| "F"; "D"; "EX"; "M"; "W" |]
(* EF for (approximate) alphabetical order (fetch doesn't matter much) *)

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
let forward_pipeline
  ~signal
  ~stage
  ~stall
  ~bubble
  ?(default = Signal.zero (Signal.width signal))
  ~clocking
  ()
  =
  (* Signal in stage N is just input if N was inject stage, otherwise add register with value N-1 as input.
  Easily implemented with simple recursion: *)
  let rec get_signal si =
    if si < stage_index stage
    then failwith "Tried to extract signal from pipeline before it was added"
    else if si = stage_index stage
    then signal
    else (
      let prev = get_memo (si - 1) in
      let s =
        Types.Clocking.reg
          clocking
          ~enable:~:(stall (index_stage si))
          (mux2 (bubble (index_stage si)) default prev)
      in
      set_names
        s
        (List.map (names_and_locs signal) ~f:(fun n ->
           { n with name = n.name ^ "." ^ stage_names.(si) }));
      s)
  (* Just using `get_signal` would rebuild pipeline on each access. Memoization avoids (while lazying building only needed latches) *)
  and cache = Array.create ~len:5 None
  and get_memo si =
    match cache.(si) with
    | Some w -> w
    | None ->
      cache.(si) <- Some (get_signal si);
      get_memo si
  in
  fun s -> get_memo (stage_index s)
;;

(* Creates a new wire for each stage of the pipeline, returning a `pipe_signal` referencing these wires *)
let pipe_wires name w =
  let wires =
    Array.init 5 ~f:(fun i -> Signal.(wire w -- (stage_names.(i) ^ "." ^ name)))
  in
  fun s -> wires.(stage_index s)
;;

let create scope (i : _ I.t) =
  let open Signal in
  (* Pipeline stuff *)
  (* Stall signals: stall decode on hazard, fetch when insn not valid, memory
     on a load miss. *)
  let%hw stall_fetch = wire 1 in
  let%hw stall_decode = wire 1 in
  let%hw stall_memory = wire 1 in
  let stall = function
    | F -> stall_fetch |: stall_decode |: stall_memory
    | D -> stall_decode |: stall_memory
    | X | M ->
      stall_memory
      (* If we stall memory while a WX bypass is happening (e.g., R-type writing to $r1 in W, load miss in M, then R-type reading from $r1 in X), we can have issue where instruction in X is meant to bypass value from W, but while it is stalled (propagated back from M), the instruction in W writes back and the value is unavailable.

           TODO: I think best fix is for stalled pipeline latch to take in bypassed value, rather than always holding its current value. Current stall signal uses pipeline latch write-enable, but my understanding is that just muxes in current value when disabled. Muxing in the value from after bypass logic instead shouldn't affect logic depth. *)
    | W -> stall_memory
  in
  (* Send bubble to D,X on branch in execute, or to any on stall in previous stage *)
  let%hw branch_execute = wire 1 in
  let bubble s =
    let withoutstall = function
      | D | X -> branch_execute
      | _ -> gnd
    in
    withoutstall s ||: stall (index_stage (stage_index s - 1))
  in
  let forward_pipeline ~signal ~stage ?default () =
    forward_pipeline ~signal ~stage ~stall ~bubble ?default ~clocking:i.clocking ()
  in
  (* Fetch stage *)
  (* Next PC to fetch is the branch PC if branching, PC+4 if successfully
     fetched, or the previous PC if fetch stalled. Note that a branch occurs
     while fetch is stalled, the PC which missed is now irrelevant, and we need
     to save the branch PC so the branch insn can continue onward (could
     imagine stalling execute, but if M and W proceed then bypassing is
     annoying). *)
  let%hw branch_pc = wire 32 in
  let%hw pc_reg = wire 32 in
  let%hw pc_to_fetch =
    mux2 branch_execute branch_pc (mux2 (stall F) pc_reg (pc_reg +:. 4))
  in
  pc_reg <-- Types.Clocking.reg i.clocking pc_to_fetch;
  let pc = forward_pipeline ~signal:pc_reg ~stage:F () in
  let%hw.Memory.L1i_cache.From_pipe.Of_signal to_l1i = { pc = pc_to_fetch } in
  (* Fetch is stalled if the PC missed in the cache. If a branch occurred
     during miss handling, the I-cache gives [valid] once the miss is handled,
     and we must ignore. *)
  (* TODO: don't love the comparison here, and really feels like this should be simplifiable. *)
  stall_fetch <-- (~:(i.from_l1i.valid) ||: (i.from_l1i.pc <>: pc_reg));
  let insn =
    forward_pipeline
      ~signal:i.from_l1i.insn
      ~stage:F
      ~default:(of_hex ~width:32 "00000013")
      ()
  in
  (* for tracking commits *)
  let is_insn = forward_pipeline ~signal:(vdd -- "is_insn") ~stage:F () in
  (* Decode *)
  let%hw.Decoded.Of_signal decoded = Decode.hierarchical ~scope (insn D) in
  (* Register file *)
  let%hw reg_write = wire 32 in
  let%hw reg_dest = wire 5 in
  let reg_srcs = [| decoded.rs1; decoded.rs2 |] in
  let regfile =
    Regfile.hierarchical
      ~scope
      { rd = reg_dest; rdval = reg_write; rs = reg_srcs; clocking = i.clocking }
  in
  (* Create wires for bypassed sources and written destination *)
  let rs1val = pipe_wires "rs1val" 32
  and rs2val = pipe_wires "rs2val" 32
  and rdval = pipe_wires "rdval" 32 in
  rs1val D <-- regfile.rsval.(0);
  rs2val D <-- regfile.rsval.(1);
  (* Place decoded values into pipeline *)
  let opcode = forward_pipeline ~signal:decoded.opcode ~stage:D ()
  and rs1 = forward_pipeline ~signal:decoded.rs1 ~stage:D ()
  and rs2 = forward_pipeline ~signal:decoded.rs2 ~stage:D ()
  and rd = forward_pipeline ~signal:decoded.rd ~stage:D ()
  and imm = forward_pipeline ~signal:decoded.imm ~stage:D ()
  and funct3 = forward_pipeline ~signal:decoded.funct3 ~stage:D ()
  and optype = forward_pipeline ~signal:decoded.optype ~stage:D () in
  (* Execute stage *)
  (* First operand is rs1 (bypassed) usually, but PC for branch, jal, and auipc (lui takes rs1=0 for imm pass-through) *)
  let%hw src1x =
    mux2
      (opcode X
       ==: Riscv.Op.branch
       |: (opcode X ==: Riscv.Op.jal)
       |: (opcode X ==: Riscv.Op.auiPc))
      (pc X)
      (rs1val X)
  in
  (* Second is rs2 for R-type, imm otherwise (including branch, where rs2 is used for comparison while ALU calculates PC) *)
  let%hw src2x = mux2 (opcode X ==: Riscv.Op.intR) (rs2val X) (imm X) in
  (* ALU *)
  let%hw.Alu.O.Of_signal alu_result =
    Alu.hierarchical ~scope { src1 = src1x; src2 = src2x; optype = optype X }
  in
  (* Branches *)
  (* We must branch (from execute (TODO: not always)) if a branch condition holds or we are doing a jump. *)
  let branch_cond =
    mux
      (funct3 X)
      [ rs1val X ==: rs2val X
      ; (* beq = 0 *)
        rs1val X <>: rs2val X
      ; (* bne = 1 *)
        gnd
      ; gnd
      ; (* 2,3 unused *)
        rs1val X <+ rs2val X
      ; (* blt = 4 *)
        rs1val X >=+ rs2val X
      ; (* bge = 5 *)
        rs1val X <: rs2val X
      ; (* bltu = 6 *)
        rs1val X >=: rs2val X (* bgeu = 7 *)
      ]
  in
  branch_execute
  <-- reduce
        ~f:( |: )
        [ opcode X ==: Riscv.Op.branch &: branch_cond
        ; opcode X ==: Riscv.Op.jal
        ; opcode X ==: Riscv.Op.jalr
        ];
  (* Address to branch to is computed by ALU (PC+imm for branch, jal; rs1+imm for jalr) *)
  branch_pc <-- alu_result.result;
  (* Memory stage. Inputs are latched internally. *)
  let%hw.Memory.L1d_cache.From_pipe.Of_signal to_l1d =
    { addr = alu_result.result
    ; load = opcode X ==: Riscv.Op.load
    ; store = opcode X ==: Riscv.Op.store
    ; size = (funct3 X).:[1, 0]
    ; sign_extend = ~:((funct3 X).:(2))
    ; store_data = rs2val X
    }
  in
  stall_memory <-- i.from_l1d.stall;
  (* Pass written value to M for writeback and bypassing, then that or loaded value to W. *)
  rdval M
  <-- forward_pipeline
        ~signal:
          ((* If jal or jalr, then PC+4; otherwise from ALU (auipc and lui implemented in ALU src
    selection, with lui's rs1=0 ensuring imm passes through ALU unchanged) *)
           mux2
             (opcode X ==: Riscv.Op.jal |: (opcode X ==: Riscv.Op.jalr))
             (pc X +: of_string "32'd4")
             alu_result.result)
        ~stage:X
        ()
        M;
  rdval W
  <-- forward_pipeline
        ~signal:(mux2 (opcode M ==: Riscv.Op.load) i.from_l1d.load_data (rdval M))
        ~stage:M
        ()
        W;
  (* Writeback stage *)
  reg_write <-- rdval W;
  reg_dest <-- rd W;
  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)? ultimately only two possible things to bypass;
  main thing to abstract over is which values came from rs1 and rs2 and when they were written to) *)
  rs1val X
  <-- mux2
        (rs1 X ==: rd M &: (rs1 X <>: zero 5) -- "bypassMX1")
        (rdval M)
        (mux2
           (rs1 X ==: rd W &: (rs1 X <>: zero 5) -- "bypassWX1")
           (rdval W)
           (forward_pipeline ~signal:(rs1val D) ~stage:D () X));
  rs2val X
  <-- mux2
        (rs2 X ==: rd M &: (rs2 X <>: zero 5) -- "bypassMX2")
        (rdval M)
        (mux2
           (rs2 X ==: rd W &: (rs2 X <>: zero 5) -- "bypassWX2")
           (rdval W)
           (forward_pipeline ~signal:(rs2val D) ~stage:D () X));
  (* for store data *)
  rs2val M
  <-- mux2
        (rs2 M ==: rd W &: (rs2 M <>: zero 5) -- "bypassWM2")
        (rdval W)
        (forward_pipeline ~signal:(rs2val X) ~stage:X () M);
  (* Stall logic: stall only needed for load-use hazard (load in X when consuming instruction in D) *)
  let reg_is_load_dest rs =
    opcode X ==: Riscv.Op.load &: (rs ==: rd X) &: (rs <>: zero 5)
  in
  stall_decode <-- (reg_is_load_dest (rs1 D) |: reg_is_load_dest (rs2 D));
  let%hw commit_valid = is_insn W &&: ~:(stall W) in
  O.{ to_l1i; to_l1d; commit_pc = { valid = commit_valid; value = pc W } }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
