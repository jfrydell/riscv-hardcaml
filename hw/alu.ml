
open Hardcaml

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
