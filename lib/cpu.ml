
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


(* Implements integer ALU instructions. Takes in values for opcode, sources, funct7, and funct3. *)
let alu opcode alu_in_1 alu_in_2 funct7 funct3 =
  let open Signal in

  (* Adder *)
  let add_in_2 = mux2 (funct7 ==: of_string "7'h20" ||: funct3 ==: of_string "2'h2" ||: funct3 ==: of_string "2'h3")
    (negate alu_in_2) alu_in_2 in
  let rel_add = alu_in_1 +: add_in_2 in

  (* Logic functions *)
  let rel_and = alu_in_1 &: alu_in_2
  and rel_or = alu_in_1 |: alu_in_2
  and rel_xor = alu_in_1 ^: alu_in_2
  and rel_sll = log_shift sll alu_in_1 alu_in_2.:[4,0]
  and rel_srl = log_shift srl alu_in_1 alu_in_2.:[4,0]
  and rel_sra = log_shift sra alu_in_1 alu_in_2.:[4,0] in

  (* Choice of right shift depends on funct7 for reg and imm[11,5] for imm; could use pipeline to decide,
  but since it's all in the same stage it's simpler to just use a mux *)
  let sra_over_srl = mux2 (opcode.:(5)) (funct7 ==: of_string "7'h20") (alu_in_2.:[11,5] ==: of_string "7'h20") in

  (* Set less than based on signs (avoids overflow in adder) *)
  let rel_slt_sltu = mux (alu_in_1.:(31) @: alu_in_2.:(31)) [
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
  mux funct3 [
    rel_add;                      (* 0: add/sub *)
    rel_sll;                      (* 1: sll *)
    uresize rel_slt_sltu.:(1) 32; (* 2: slt *)
    uresize rel_slt_sltu.:(0) 32; (* 3: sltu *)
    rel_xor;                      (* 4: xor *)
    mux2 sra_over_srl             (* 5: sr *)
      rel_sra rel_srl;
    rel_or;                       (* 6: or *)
    rel_and;                      (* 7: and *)
  ]


let cpu ~clock ~reset ~imem_size =
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
  let read_port = { Read_port.read_clock = clock
                  ; read_address = pc_reg
                  ; read_enable = vdd } in
  let imem = Ram.create
        ~collision_mode:Read_before_write
        ~size:imem_size
        ~write_ports:[||]
        ~read_ports:[|read_port|]
        () in
  let insn = forward_pipeline ~signal:imem.(0) ~stage:F ~default:(of_hex ~width:32 "00000013") in
  (* Next PC calculation (TODO branch) *)
  let _ = next_pc <== next_pc +:. 1 in

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
  let reg_write_val = wire 32 in
  let reg_write_en = wire 1 in (* TODO: implement this; something with `WriteReg` maybe? *)
  let write_port =  { Write_port.write_clock = clock
                    ; write_address = rd
                    ; write_data = reg_write_val
                    ; write_enable = reg_write_en } in
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
  (* TODO: only set regs that are actually written by insn *)
  let rs1 = forward_pipeline ~signal:rs.(0) ~stage:D
  and rs2 = forward_pipeline ~signal:(mux2 (opcode D) rs.(1) (zero 3)) ~stage:D
  and rd = forward_pipeline ~signal:rd ~stage:D in
  let src1 = forward_pipeline ~signal:rsvals.(0) ~stage:D
  and src2 =
    let src_ops = [
      rsvals.(1), [of_bit_string "0110011"];
      immi, [of_bit_string "0010011"; of_bit_string "0000011"; of_bit_string "1110011"; of_bit_string "1100111"];
      imms, [of_bit_string "0100011"];
      immb, [of_bit_string "1100011"];
      immj, [of_bit_string "1101111"];
      immu, [of_bit_string "0110111"];
    ] in
    let src = List.map ~f:(fun (src, opcodes) ->
      {
        With_valid.value = src;
        (* Source should be chosen if opcode matches one of its  *)
        valid = opcodes |> List.map ~f:(fun o -> (opcode D) ==: o) |> tree ~arity:2 ~f:(function [a; b] -> a ||: b | _ -> failwith "invalid tree")
      }
    ) src_ops |> priority_select in
    forward_pipeline ~signal:src.value ~stage:D in
  (* opcode, func bits come directly from instruction (TODO: injective separately would save some reg bits because rest of instruction unused?) *)
  let opcode s = (insn s).:[6,0] in
  let funct3 s = (insn s).:[14,12] and funct7 s = (insn s).:[31,25] in

  (* Execute stage *)
  (* Sources with bypassing *)
  let src1x = wire 32 and src2x = wire 32 in
  (* ALU *)
  let alu_result = alu (opcode X) src1x src2x (funct7 X) (funct3 X) in
  let writeval_d = forward_pipeline ~signal:alu_result ~stage:X in

  (* Memory stage *)
  let writeval_m = forward_pipeline ~signal:(writeval_d M) ~stage:M in


  (* Bypassing (TODO: make nice abstraction for this (but not too general like original attempt)?) *)
  let _ = src1x <== mux2 (rs1 X ==: rd M) (writeval_d M) (
                    mux2 (rs1 X ==: rd W) (writeval_m W) (
                    src1 X)) in
  let _ = src2x <== mux2 (rs2 X ==: rd M) (writeval_d M) (
                    mux2 (rs2 X ==: rd W) (writeval_m W) (
                    src2 X)) in




  ()
