open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { src1 : 'a [@bits 32]
    ; src2 : 'a [@bits 32]
    ; optype : 'a Decoded.Optype.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { result : 'a [@bits 32] } [@@deriving hardcaml]
end

let create scope ({ src1; src2; optype } : _ I.t) =
  (* Adder; subtractor if sub (via disambig since sra doesn't use) or slt(u). *)
  let%hw add_in_2 =
    mux2
      Decoded.Optype.(optype ==: sub |: (optype ==: slt) |: (optype ==: sltu))
      (negate src2)
      src2
  in
  let%hw rel_add = src1 +: add_in_2 in
  (* Logic functions *)
  let%hw rel_and = src1 &: src2 in
  let%hw rel_or = src1 |: src2 in
  let%hw rel_xor = src1 ^: src2 in
  let%hw rel_sll = log_shift ~f:sll src1 ~by:src2.:[4, 0] in
  let%hw rel_srl = log_shift ~f:srl src1 ~by:src2.:[4, 0] in
  let%hw rel_sra = log_shift ~f:sra src1 ~by:src2.:[4, 0] in
  (* Set less than based on signs (avoids overflow in adder) *)
  let rel_slt_sltu =
    mux
      (src1.:(31) @: src2.:(31))
      [ (* Both positive: just check sign bit for both, no overflow possible *)
        rel_add.:(31) @: rel_add.:(31)
      ; (* First positive, second negative: greater if signed and less if unsigned *)
        of_bit_string "01"
      ; (* First negative, second positive: less if signed and greater if unsigned *)
        of_bit_string "10"
      ; (* Both negative: same as both positive (just added/subtracted cancelling 2^31 to both) *)
        rel_add.:(31) @: rel_add.:(31)
      ]
  in
  (* Compute result with funct3 *)
  let result =
    mux
      (drop_bottom ~width:1 optype)
      [ rel_add (* 0: add/sub *)
      ; rel_sll (* 1: sll *)
      ; uresize rel_slt_sltu.:(1) ~width:32 (* 2: slt *)
      ; uresize rel_slt_sltu.:(0) ~width:32 (* 3: sltu *)
      ; rel_xor (* 4: xor *)
      ; mux2 (lsb optype) rel_sra rel_srl (* 5: srl/sra *)
      ; rel_or (* 6: or *)
      ; rel_and (* 7: and *)
      ]
  in
  ({ result } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
