
open Base
open Hardcaml


(* Implements integer ALU instructions. Takes in hooks for opcode, funct7, funct3, and result to write to rd. *)
let int_alu_insns opcode funct7 funct3 immi rdval =
  let open Signal in let open Pipeline in

  (* Pipeline for all int ALU instructions, with subpipelines for immediate and register versions *)
  let p = new pipeline (fun _ _ -> vdd) in (* TODO: don't need guard because we'll use subpipelines for all writes. Should have a way to specify this? *)
  let pr = new pipeline (fun p s -> p#take opcode s ==: (of_bit_string "0110011"))
  and pi = new pipeline (fun p s -> p#take opcode s ==: (of_bit_string "0010011")) in

  (* ALU inputs *)
  let alu_in_1 = p#take (ReadReg Rs1) sX in
  (* Pull ALU in 2 from common ALU pipeline, writing from guarded imm and reg pipelines *)
  let alu_in_2 = wire 32 in
  let alu_in_2_hook = p#pull_pipe alu_in_2 sX in
  let _ = pr#inject (pr#take (ReadReg Rs2) sX) sX alu_in_2_hook
  and _ = pi#inject (pi#take immi sX) sX alu_in_2_hook in

  (* Adder *)
  let add_in_2 = mux2 (p#take funct7 sX ==: of_string "7'h20" ||: p#take funct3 sX ==: of_string "2'h2" ||: p#take funct3 sX ==: of_string "2'h3")
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
  let sra_over_srl = mux2 ((p#take opcode sX).:(5)) ((p#take funct7 sX) ==: of_string "7'h20") (alu_in_2.:[11,5] ==: of_string "7'h20") in

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
  let result = mux (p#take funct3 sX) [
    rel_add;                      (* 0: add/sub *)
    rel_sll;                      (* 1: sll *)
    uresize rel_slt_sltu.:(1) 32; (* 2: slt *)
    uresize rel_slt_sltu.:(0) 32; (* 3: sltu *)
    rel_xor;                      (* 4: xor *)
    mux2 sra_over_srl             (* 5: sr *)
      rel_sra rel_srl;
    rel_or;                       (* 6: or *)
    rel_and;                      (* 7: and *)
  ] in

  (* Write back to $rd and note that value can be bypassed from M (or later; TODO make this +1 automatic?) *)
  p#inject result sX (Write rdval);
  (* TODO: this is a bit janky (giving value written by rdval as our bypass result; feels like it should be our own value somehow);
  may want to rework this anyway, especially if Rd is done with constructor (why not just have `WriteReg` no args at that point?).
  Probably want to specify register IDs with wires for simplicity of implementation? Although I think they'll always be the same pipeline slots anyway? *)
  p#inject (p#take (Read (rdval, 32)) sM) sM (WriteReg Rd);

  (* Integrate sub-pipelines *)
  p#integrate [pr; pi];

  (* Return the final pipeline *)
  p


let cpu ~clock ~reset ~imem_size =
  let open Signal in let open Pipeline in

  let p = new pipeline (fun _ _ -> vdd) in

  (* Make fetch stage *)
  let next_pc = wire 32 in
  let pc_reg = reg (Reg_spec.create ~clock ~reset ()) ~enable:vdd next_pc in
  let read_port = { Read_port.read_clock = clock
                  ; read_address = pc_reg
                  ; read_enable = vdd } in
  let imem = Ram.create
        ~collision_mode:Read_before_write
        ~size:imem_size
        ~write_ports:[||]
        ~read_ports:[|read_port|]
        () in
  let insn = p#put_pipe imem.(0) sF in
  (* Next PC calculation (TODO branch) *)
  let _ = next_pc <== next_pc +:. 1 in

  (* Decode: extract immediate, opcode, func bits, register designators *)
  let insnd = p#take insn sD in
  (* TODO: get rid of "wire" variants of registers and use `take` (should forward directly since same pipeline stage) *)
  let rs_wires = [|insnd.:[19,15]; insnd.:[24,20]|] in
  let rs1 = p#put_pipe rs_wires.(0) sD and rs2 = p#put_pipe rs_wires.(1) sD in
  let rd_wire = insnd.:[11,7] in
  let rd = p#put_pipe rd_wire sD in
  let funct3 = p#put_pipe insnd.:[14,12] sD and funct7 = p#put_pipe insnd.:[31,25] sD in
  let immi = p#put_pipe (sresize insnd.:[31,20] 32) sD and imms = p#put_pipe (sresize (insnd.:[31,25] @: insnd.:[11,7]) 32) sD
  and immb = p#put_pipe (sresize (insnd.:(31) @: insnd.:(7) @: insnd.:[30,25] @: insnd.:[11,8] @: gnd) 32) sD
  and immu = p#put_pipe (sresize (insnd.:[31,12] @: zero 12) 32) sD
  and immj = p#put_pipe (sresize (insnd.:(31) @: insnd.:[19,12] @: insnd.:(20) @: insnd.:[30,21] @: gnd) 32) sD in
  let opcode = p#put_pipe insnd.:[6,0] sD in

  (* Register file *)
  let rdval = wire 32 in
  let rdval_id = Id.new_id () in
  let _ = p#inject rdval sW (Read (rdval_id, 32)) in (* TODO: how will this backwards path work. Should be good I think? *)
  let reg_we = wire 1 in (* TODO: something more sophisticated with `WriteReg` maybe? *)
  let write_port =  { Write_port.write_clock = clock
                    ; write_address = rd_wire
                    ; write_data = rdval
                    ; write_enable = reg_we } in
  let read_ports = Array.map rs_wires ~f:(fun rs ->
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
  let rsval_wires = Array.map2_exn rs_wires regfile ~f:(fun rs rsv ->
    mux2 (rs ==:. 0) (of_int ~width:32 0) rsv
  ) in
  (* TODO: how to make register values available? *)
  (* let rs1val = put_global rsval_wires.(0) sD and rs2val = put_global rsval_wires.(1) sD in *)

  let alu_pipeline = int_alu_insns opcode funct7 funct3 immi rdval_id


  in

  ()
