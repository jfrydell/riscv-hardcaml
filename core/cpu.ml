open Base
open Hardcaml
open Signal

(* Interface *)
module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; from_l1d : 'a Memory.L1d_cache.To_pipe.t
    ; from_l1i : 'a Memory.L1i_cache.To_pipe.t
    ; request_interrupt : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_l1i : 'a Memory.L1i_cache.From_pipe.t
    ; to_l1d : 'a Memory.L1d_cache.From_pipe.t
    ; csrs : 'a Privileged.Csrs.t (** Current CSR values. *)
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
    | X -> stall_memory
    | M ->
      stall_memory
      (* If we stall memory while a WX bypass is happening (e.g., R-type writing to $r1 in W, load miss in M, then R-type reading from $r1 in X), we can have issue where instruction in X is meant to bypass value from W, but while it is stalled (propagated back from M), the instruction in W writes back and the value is unavailable.

           TODO: I think best fix is for stalled pipeline latch to take in bypassed value, rather than always holding its current value. Current stall signal uses pipeline latch write-enable, but my understanding is that just muxes in current value when disabled. Muxing in the value from after bypass logic instead shouldn't affect logic depth. *)
    | W -> stall_memory
  in
  (* Send bubble to M,D,X on trap (detected at end of memory), to D,X on trap or
     branch in execute, or to any on stall in previous stage. W bubbles if an
     exception means the instruction in M itself should not be committed. *)
  let%hw trap_active = wire 1 in
  let%hw trap_squash_M = wire 1 in
  let%hw branch_execute = wire 1 in
  (* The instruction finishing F is garbage due to a trap or branch, possibly which occurred previously while F was stalled. (Always will be true when [trap_active] or [branch_execute].) *)
  let%hw fetched_insn_invalid = wire 1 in
  let bubble s =
    let withoutstall = function
      | W -> trap_squash_M
      | M -> trap_active
      | X -> branch_execute ||: trap_active
      | D -> fetched_insn_invalid
      | _ -> gnd
    in
    withoutstall s ||: stall (index_stage (stage_index s - 1))
  in
  let forward_pipeline ~signal ~stage ?default () =
    forward_pipeline ~signal ~stage ~stall ~bubble ?default ~clocking:i.clocking ()
  in
  (* Fetch stage *)
  (* [pc_to_fetch] holds the PC that we want to begin fetching once the
     instruction currently being fetched is done. [fetch_pc] stores the PC that is
     currently being fetched---if fetch doesn't stall, this will always be [reg
     pc_to_fetch], but a stall causes them to diverge.

     Traps and branches must update the value of [pc_to_fetch], and are tracked
     in [pc_update]. Notably, a trap or branch only gives its value for a cycle, and
     if fetch is stalled at that point, this update won't make it to [fetch_pc]
     immediately. So, as long as fetch was stalled and missed [pc_to_fetch],
     [pc_to_fetch] holds its value from the previous cycle [pc_to_fetch_latched]
     (unless there is a(nother) [pc_update]). *)
  let%hw.With_valid.Of_signal pc_update = { value = wire 32; valid = wire 1 } in
  let%hw pc_reg = wire 32 in
  let%hw pc_to_fetch_latch = wire 32 in
  let%hw pc_to_fetch =
    mux2 pc_update.valid pc_update.value
    @@ mux2 (Types.Clocking.reg i.clocking (stall F)) pc_to_fetch_latch (pc_reg +:. 4)
  in
  pc_to_fetch_latch <-- Types.Clocking.reg i.clocking pc_to_fetch;
  pc_reg <-- Types.Clocking.reg i.clocking ~enable:~:(stall F) pc_to_fetch;
  let pc = forward_pipeline ~signal:pc_reg ~stage:F () in
  (* To handle propagated stalls correctly, we can't let F take in a new PC
     while it is stalling (technically mux condition could just be propagated
     stalls, but should simplify and this is better for maintainability). One might
     think we could include this in [pc_to_fetch], but that doesn't work---if D is
     stalled while a [pc_update] happens, in principle (in practice shouldn't
     happen?) we'd need to keep track of that PC update until it can be sent to the
     L1 I$, but can't send it now (as we'd lose whatever's currently there waiting
     to go to D). *)
  (* TODO: probably should stop fetching while a trap is active. fine to keep doing it, as we will throw away those instructions at D. *)
  let%hw.Memory.L1i_cache.From_pipe.Of_signal to_l1i =
    { pc = mux2 (stall F) pc_reg pc_to_fetch }
  in
  stall_fetch <-- ~:(i.from_l1i.valid);
  let insn =
    forward_pipeline
      ~signal:i.from_l1i.insn
      ~stage:F
      ~default:(of_hex ~width:32 "00000013")
      ()
  in
  (* PC updates come from branches and traps, prioritizing traps (shouldn't matter in practice, but they logically happened earlier). *)
  let%hw branch_pc = wire 32 in
  let%hw.With_valid.Of_signal trap_pc = { value = wire 32; valid = wire 1 } in
  pc_update.valid <-- (branch_execute ||: trap_pc.valid);
  pc_update.value <-- mux2 trap_pc.valid trap_pc.value branch_pc;
  (* If a PC update occurs, the currently-being-fetched instruction is invalid (and we must remember this until it completes and is replaced by a bubble in D). *)
  fetched_insn_invalid
  <-- Utils.sr ~style:`Mealy_set i.clocking ~set:pc_update.valid ~reset:~:(stall F);
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
  and optype = forward_pipeline ~signal:decoded.optype ~stage:D ()
  and is_csr = forward_pipeline ~signal:decoded.is_csr ~stage:D ()
  and result_in_m = forward_pipeline ~signal:decoded.result_in_m ~stage:D () in
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
  let next_pc =
    forward_pipeline ~signal:(mux2 branch_execute branch_pc (pc X +:. 4)) ~stage:X ()
  in
  let%hw execute_result =
    mux2
      (opcode X ==: Riscv.Op.jal |: (opcode X ==: Riscv.Op.jalr))
      (pc X +: of_string "32'd4")
      alu_result.result
  in
  (* Memory stage. Inputs are latched internally. *)
  let%hw.Memory.L1d_cache.From_pipe.Of_signal to_l1d =
    { addr = alu_result.result
    ; load = opcode X ==: Riscv.Op.load &&: ~:(bubble M)
    ; store = opcode X ==: Riscv.Op.store &&: ~:(bubble M)
    ; size = (funct3 X).:[1, 0]
    ; sign_extend = ~:((funct3 X).:(2))
    ; store_data = rs2val X
    }
  in
  stall_memory <-- i.from_l1d.stall;
  let%hw.Privileged.Csrs.Of_signal csrs = Privileged.Csrs.Of_signal.wires () in
  let csr_read_cases =
    Privileged.Csrs.map2 Privileged.Csrs.addresses csrs ~f:(fun address value ->
      of_unsigned_int ~width:12 address, value)
    |> Privileged.Csrs.to_list
  in
  let%hw csr_read = cases ~default:(zero 32) (insn M).:[31, 20] csr_read_cases in
  let%hw memory_result = mux2 (is_csr M) csr_read i.from_l1d.load_data in
  (* Pass written value to M for writeback and bypassing, then that or loaded value to W. *)
  rdval M <-- forward_pipeline ~signal:execute_result ~stage:X () M;
  rdval W
  <-- forward_pipeline
        ~signal:(mux2 (result_in_m M) memory_result (rdval M))
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
  (* For store data *)
  rs2val M
  <-- mux2
        (rs2 M ==: rd W &: (rs2 M <>: zero 5) -- "bypassWM2")
        (rdval W)
        (forward_pipeline ~signal:(rs2val X) ~stage:X () M);
  (* CSR write data just gets value from X (could bypass load-to-use, but didn't bother). *)
  rs1val M <-- forward_pipeline ~signal:(rs1val X) ~stage:X () M;
  (* Stall logic: stall only needed for load-use hazard (load in X when consuming instruction in D) *)
  let reg_is_load_dest rs = result_in_m X &: (rs ==: rd X) &: (rs <>: zero 5) in
  (* TODO: shouldn't stall if can bypass to store data, right? otherwise what is rs2val M bypass doing? *)
  stall_decode <-- (reg_is_load_dest (rs1 D) |: reg_is_load_dest (rs2 D));
  (* Trap / CSR write handling.
     Traps are always raised as an instruction finishes the M stage. We can't
     raise earlier because page faults are detected in M, but can't wait until
     commit because stores write to the cache in M. Because both of these
     happen in the same stage, but one requires the trap to occur before the
     instruction is executed, and the other requires the instruction to
     complete (since it's already written to memory) before the trap, we
     this outputs a [trap_squash_M] signal stating when the instruction should
     M should not proceed to writeback, and the trap logically occurs before it
     commits (setting EPC to the instruction in M, not the one after). *)
  let%hw.Privileged.Trap.O.Of_signal trap =
    Privileged.Trap.hierarchical
      ~scope
      { clocking = i.clocking
      ; m_stage = { valid = is_insn M; stall = stall_memory }
      ; pc = pc M
      ; next_pc = next_pc M
      ; detect_exception = { insn = insn M; rs1 = rs1val M; csrs }
      ; interrupt_request =
          { valid = i.request_interrupt; value = of_unsigned_int ~width:4 11 }
      }
  in
  trap_active <-- trap.trap_active;
  trap_squash_M <-- trap.squash_M;
  With_valid.iter2 ~f:( <-- ) trap_pc trap.handler_pc;
  Privileged.Csrs.Of_signal.assign csrs trap.csrs;
  let%hw commit_valid = is_insn W &&: ~:(stall W) in
  O.{ to_l1i; to_l1d; csrs; commit_pc = { valid = commit_valid; value = pc W } }
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
