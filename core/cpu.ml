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

(** Pipeline status for debugging. *)
module Pipeline_status = struct
  type 'a t =
    { stall : 'a Pipeline.Pipelined_bit.t
    ; bubble : 'a Pipeline.Pipelined_bit.t
    ; trap_active : 'a
    ; is_insn : 'a Pipeline.Pipelined_bit.t
    ; pc : 'a Pipeline.Pipelined_word.t
    ; last_pc_update : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_l1i : 'a Memory.L1i_cache.From_pipe.t
    ; to_l1d : 'a Memory.L1d_cache.From_pipe.t
    ; commit_pc : 'a With_valid.t [@bits 32]
    (** The PC of the instruction committing this cycle, when valid. *)
    ; csrs : 'a Privileged.Csrs.t (** Current CSR values. *)
    ; pipeline_status : 'a Pipeline_status.t
    }
  [@@deriving hardcaml]
end

let create ?(initial_pc = 0) scope (i : _ I.t) =
  let initial_pc = of_int_trunc ~width:32 initial_pc in
  (* Pipeline stuff *)
  (* Stall signals: stall decode on hazard, fetch when insn not valid, memory
     on a load miss. *)
  let%hw stall_fetch = wire 1 in
  let%hw stall_decode = wire 1 in
  let%hw stall_memory = wire 1 in
  let%hw.Pipeline.Pipelined_bit.Of_signal stall =
    { f = stall_fetch |: stall_decode |: stall_memory
    ; d = stall_decode |: stall_memory
    ; x = stall_memory
    ; m =
        stall_memory
        (* If we stall memory while a WX bypass is happening (e.g., R-type writing to $r1 in W, load miss in M, then R-type reading from $r1 in X), we can have issue where instruction in X is meant to bypass value from W, but while it is stalled (propagated back from M), the instruction in W writes back and the value is unavailable.

           TODO: I think best fix is for stalled pipeline latch to take in bypassed value, rather than always holding its current value. Current stall signal uses pipeline latch write-enable, but my understanding is that just muxes in current value when disabled. Muxing in the value from after bypass logic instead shouldn't affect logic depth. *)
    ; w = stall_memory
    }
  in
  (* Send bubble to M,D,X on trap (detected at end of memory), to D,X on trap or
     branch in execute, or to any on stall in previous stage. W bubbles if an
     exception means the instruction in M itself should not be committed. *)
  let%hw trap_active = wire 1 in
  let%hw trap_squash_M = wire 1 in
  let%hw branch_execute = wire 1 in
  (* The instruction finishing F is garbage due to a trap or branch, possibly which occurred previously while F was stalled. *)
  let%hw fetched_insn_invalid = wire 1 in
  let%hw.Pipeline.Pipelined_bit.Of_signal bubble =
    { w = trap_squash_M ||: stall.m
    ; m = trap_active ||: stall.x
    ; x = branch_execute ||: trap_active ||: stall.d
    ; d = branch_execute ||: trap_active ||: fetched_insn_invalid ||: stall.f
    ; f = gnd
    }
  in
  let%hw.Pipeline.Pipeline_info.Of_signal pipe_info =
    { clocking = i.clocking; stall; bubble }
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
    @@ mux2 (Types.Clocking.reg i.clocking stall.f) pc_to_fetch_latch (pc_reg +:. 4)
  in
  pc_to_fetch_latch <-- Types.Clocking.reg i.clocking ~clear_to:initial_pc pc_to_fetch;
  pc_reg
  <-- Types.Clocking.reg i.clocking ~clear_to:initial_pc ~enable:~:(stall.f) pc_to_fetch;
  let%hw.Pipeline.Pipelined_word.Of_signal pc =
    Pipeline.Pipelined_word.forward ~pipe_info ~from_stage:F pc_reg
  in
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
    { pc = mux2 stall.f pc_reg pc_to_fetch }
  in
  stall_fetch <-- ~:(i.from_l1i.valid ||: i.from_l1i.fault);
  let%hw.Pipeline.Pipelined_word.Of_signal insn =
    Pipeline.Pipelined_word.forward
      ~pipe_info
      ~from_stage:F
      ~default:(of_hex ~width:32 "00000013")
      (* Insert no-op on fault, as we don't raise exception until reaching M,
         where a mistaken store could occur. *)
      (mux2 i.from_l1i.fault (of_hex ~width:32 "00000013") i.from_l1i.insn)
  in
  (* PC updates come from branches and traps, prioritizing traps (shouldn't matter in practice, but they logically happened earlier). *)
  let%hw branch_pc = wire 32 in
  let%hw.With_valid.Of_signal trap_pc = { value = wire 32; valid = wire 1 } in
  pc_update.valid <-- (branch_execute ||: trap_pc.valid);
  pc_update.value <-- mux2 trap_pc.valid trap_pc.value branch_pc;
  (* If a PC update occurs, the currently-being-fetched instruction is invalid (and we must remember this until it completes and is replaced by a bubble in D). *)
  fetched_insn_invalid
  <-- Utils.sr ~style:`Mealy_set i.clocking ~set:pc_update.valid ~reset:~:(stall.f);
  (* For tracking commits. *)
  let%hw.Pipeline.Pipelined_word.Of_signal is_insn =
    Pipeline.Pipelined_word.forward ~pipe_info ~from_stage:F ~default:gnd vdd
  in
  (* Decode *)
  let module Pipelined_decoded = Pipeline.Pipelined (Decoded) in
  let%hw.Pipelined_decoded.Of_signal decoded =
    Pipelined_decoded.forward ~pipe_info ~from_stage:D (Decode.hierarchical ~scope insn.d)
  in
  (* Register file *)
  let%hw reg_write = wire 32 in
  let%hw reg_dest = wire 5 in
  let reg_srcs = [| decoded.d.rs1; decoded.d.rs2 |] in
  let regfile =
    Regfile.hierarchical
      ~scope
      { rd = reg_dest; rdval = reg_write; rs = reg_srcs; clocking = i.clocking }
  in
  (* Create wires for bypassed sources and written destination *)
  let%hw.Pipeline.Pipelined_word.Of_signal rs1val =
    Pipeline.Pipelined_word.Of_signal.wires ()
  and rs2val = Pipeline.Pipelined_word.Of_signal.wires ()
  and rdval = Pipeline.Pipelined_word.Of_signal.wires () in
  rs1val.d <-- regfile.rsval.(0);
  rs2val.d <-- regfile.rsval.(1);
  (* Execute stage *)
  (* First operand is rs1 (bypassed) usually, but PC for branch, jal, and auipc (lui takes rs1=0 for imm pass-through) *)
  let%hw src1x =
    mux2
      (decoded.x.opcode
       ==: Riscv_isa.Of_signal.Op.branch
       |: (decoded.x.opcode ==: Riscv_isa.Of_signal.Op.jal)
       |: (decoded.x.opcode ==: Riscv_isa.Of_signal.Op.auiPc))
      pc.x
      rs1val.x
  in
  (* Second is rs2 for R-type, imm otherwise (including branch, where rs2 is used for comparison while ALU calculates PC) *)
  let%hw src2x =
    mux2 (decoded.x.opcode ==: Riscv_isa.Of_signal.Op.intR) rs2val.x decoded.x.imm
  in
  (* ALU *)
  let%hw.Alu.O.Of_signal alu_result =
    Alu.hierarchical ~scope { src1 = src1x; src2 = src2x; optype = decoded.x.optype }
  in
  (* Branches *)
  (* We must branch (from execute (TODO: not always)) if a branch condition holds or we are doing a jump. *)
  let branch_cond =
    mux
      decoded.x.funct3
      [ rs1val.x ==: rs2val.x
      ; (* beq = 0 *)
        rs1val.x <>: rs2val.x
      ; (* bne = 1 *)
        gnd
      ; gnd
      ; (* 2,3 unused *)
        rs1val.x <+ rs2val.x
      ; (* blt = 4 *)
        rs1val.x >=+ rs2val.x
      ; (* bge = 5 *)
        rs1val.x <: rs2val.x
      ; (* bltu = 6 *)
        rs1val.x >=: rs2val.x (* bgeu = 7 *)
      ]
  in
  branch_execute
  <-- reduce
        ~f:( |: )
        [ decoded.x.opcode ==: Riscv_isa.Of_signal.Op.branch &: branch_cond
        ; decoded.x.opcode ==: Riscv_isa.Of_signal.Op.jal
        ; decoded.x.opcode ==: Riscv_isa.Of_signal.Op.jalr
        ];
  (* Address to branch to is computed by ALU (PC+imm for branch, jal; rs1+imm for jalr) *)
  branch_pc <-- alu_result.result;
  let%hw.Pipeline.Pipelined_word.Of_signal next_pc =
    mux2 branch_execute branch_pc (pc.x +:. 4)
    |> Pipeline.Pipelined_word.forward ~pipe_info ~from_stage:X
  in
  let%hw execute_result =
    mux2
      (decoded.x.opcode
       ==: Riscv_isa.Of_signal.Op.jal
       |: (decoded.x.opcode ==: Riscv_isa.Of_signal.Op.jalr))
      (pc.x +: of_string "32'd4")
      alu_result.result
  in
  (* Memory stage. Inputs are latched internally. *)
  let%hw store_data = wire 32 in
  let%hw.Memory.L1d_cache.From_pipe.Of_signal to_l1d =
    { addr = alu_result.result
    ; load = decoded.x.opcode ==: Riscv_isa.Of_signal.Op.load &&: ~:(bubble.m)
    ; store = decoded.x.opcode ==: Riscv_isa.Of_signal.Op.store &&: ~:(bubble.m)
    ; size = decoded.x.funct3.:[1, 0]
    ; sign_extend = ~:(decoded.x.funct3.:(2))
    ; store_data
    }
  in
  stall_memory <-- i.from_l1d.stall;
  let%hw.Privileged.Csrs.Of_signal csrs = Privileged.Csrs.Of_signal.wires () in
  let csr_read_cases =
    Privileged.Csrs.map2 Privileged.Csrs.addresses csrs ~f:(fun address value ->
      of_unsigned_int ~width:12 address, value)
    |> Privileged.Csrs.to_list
  in
  let%hw csr_read = cases ~default:(zero 32) insn.m.:[31, 20] csr_read_cases in
  let%hw memory_result = mux2 decoded.m.is_csr csr_read i.from_l1d.load_data in
  (* Pass written value to M for writeback and bypassing, then that or loaded value to W. *)
  let rdval_from_execute =
    Pipeline.Pipelined_word.forward ~pipe_info ~from_stage:X execute_result
  in
  rdval.m <-- rdval_from_execute.m;
  let rdval_from_memory =
    mux2 decoded.m.result_in_m memory_result rdval.m
    |> Pipeline.Pipelined_word.forward ~pipe_info ~from_stage:M
  in
  rdval.w <-- rdval_from_memory.w;
  (* Writeback stage *)
  reg_write <-- rdval.w;
  reg_dest <-- decoded.w.rd;
  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)? ultimately only two possible things to bypass;
  main thing to abstract over is which values came from rs1 and rs2 and when they were written to) *)
  rs1val.x
  <-- mux2
        (decoded.x.rs1 ==: decoded.m.rd &: (decoded.x.rs1 <>: zero 5) -- "bypassMX1")
        rdval.m
      @@ mux2
           (decoded.x.rs1 ==: decoded.w.rd &: (decoded.x.rs1 <>: zero 5) -- "bypassWX1")
           rdval.w
      @@ Pipeline.forward ~pipe_info ~from:D ~to_:X rs1val.d;
  rs2val.x
  <-- mux2
        (decoded.x.rs2 ==: decoded.m.rd &: (decoded.x.rs2 <>: zero 5) -- "bypassMX2")
        rdval.m
      @@ mux2
           (decoded.x.rs2 ==: decoded.w.rd &: (decoded.x.rs2 <>: zero 5) -- "bypassWX2")
           rdval.w
      @@ Pipeline.forward ~pipe_info ~from:D ~to_:X rs2val.d;
  (* Store data needs to be bypassed before M pipeline register, as it goes
     into L1 D-cache which does its own latching. TODO: do all bypassing pre-latch like this? *)
  store_data
  <-- mux2
        ((decoded.x.rs2 ==: decoded.m.rd &: (decoded.x.rs2 <>: zero 5))
         -- "bypass_nextcycle_WM2")
        rdval_from_memory.m
        rs2val.x;
  (* CSR write data just gets value from X (could bypass load-to-use, but didn't bother). *)
  rs1val.m <-- Pipeline.forward ~pipe_info ~from:X ~to_:M rs1val.x;
  (* Stall logic: stall only needed for load-use hazard (load in X when consuming instruction in D) *)
  let reg_is_load_dest rs =
    decoded.x.result_in_m &: (rs ==: decoded.x.rd) &: (rs <>: zero 5)
  in
  stall_decode
  <-- (reg_is_load_dest decoded.d.rs1
       |: (reg_is_load_dest decoded.d.rs2 &: ~:(decoded.d.rs2_not_used_until_m)));
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
  let%hw.Privileged.Detect_exception.Triggers.Of_signal exception_triggers =
    { fetch_fault =
        Pipeline.forward ~pipe_info ~from:F ~to_:M ~default:gnd i.from_l1i.fault
    ; memory_fault = i.from_l1d.fault
    ; branch_unaligned = sel_bottom ~width:2 next_pc.m <>:. 0
    ; access_unaligned = gnd (* TODO *)
    }
  in
  let%hw.Privileged.Trap.O.Of_signal trap =
    Privileged.Trap.hierarchical
      ~scope
      { clocking = i.clocking
      ; m_stage = { valid = is_insn.m; stall = stall.m }
      ; pc = pc.m
      ; next_pc = next_pc.m
      ; detect_exception =
          { insn = insn.m
          ; decoded = decoded.m
          ; rs1 = rs1val.m
          ; csrs
          ; triggers = exception_triggers
          }
      ; interrupt_request =
          { valid = i.request_interrupt; value = of_unsigned_int ~width:4 11 }
      }
  in
  trap_active <-- trap.trap_active;
  trap_squash_M <-- trap.squash_M;
  With_valid.iter2 ~f:( <-- ) trap_pc trap.handler_pc;
  Privileged.Csrs.Of_signal.assign csrs trap.csrs;
  let%hw commit_valid = is_insn.w &&: ~:(stall.w) in
  O.
    { to_l1i
    ; to_l1d
    ; csrs
    ; commit_pc = { value = pc.w; valid = commit_valid }
    ; pipeline_status =
        { stall
        ; bubble
        ; pc
        ; trap_active
        ; is_insn = Pipeline.Pipelined_word.to_untyped is_insn
        ; last_pc_update =
            Types.Clocking.reg i.clocking ~enable:pc_update.valid pc_update.value
        }
    }
;;

let hierarchical ?initial_pc =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical (create ?initial_pc)
;;
