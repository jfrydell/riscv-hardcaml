open! Core
open! Hardcaml
open Signal

let addr_width = 32

module Access_type = struct
  module Cases = struct
    type t =
      | Instruction
      | Load
      | Store
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  include Enum.Make_binary (Cases)
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; state : 'a State.t
    ; va : 'a With_valid.t [@bits addr_width]
    (** Request a new address to be translated. Once a request is received, this valid bit
        is ignored until [pa.valid] goes high (but a request arriving the cycle [pa.valid]
        is high will be processed). *)
    ; access_type : 'a Access_type.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { pa : 'a With_valid.t [@bits addr_width]
    (** A translated address. Held until the cycle after a new request comes in on
        [va.valid]. *)
    }
  [@@deriving hardcaml]
end

(** No-translation mode, simply buffering the input VA into the output PA. For debugging,
    can insert a varying stall before output. *)
let create scope ({ clocking; va; state; _ } : _ I.t) =
  let%hw stall_cycles = wire 2 in
  (* Don't capture new request while stalling previous (or if it's not valid). *)
  let%hw not_stalling = stall_cycles ==:. 0 in
  let%hw accept = not_stalling &&: va.valid in
  let%hw.With_valid.Of_signal pa =
    With_valid.map va ~f:(Types.Clocking.reg ~enable:accept clocking)
  in
  (* Cycle through 4 values of stall amount if we're in none_debug mode. *)
  let%hw phase =
    Types.Clocking.reg_fb ~width:2 ~enable:accept clocking ~f:(fun p -> p +:. 1)
  in
  let%hw stall_amount =
    mux phase @@ List.map ~f:(of_int_trunc ~width:2) [ 1; 2; 0; 0 ]
    |> mux2
         (State.Translation_mode.Binary.Of_signal.is state.translation_mode None)
         (zero 2)
  in
  (* Set cycle count when we accept request, then decrement each cycle. *)
  stall_cycles
  <-- Types.Clocking.reg
        clocking
        (mux2 accept stall_amount @@ mux2 not_stalling (zero 2) (stall_cycles -:. 1));
  (* Output zeros when stalling. *)
  ({ pa =
       { value = mux2 not_stalling pa.value (zero 32); valid = not_stalling &&: pa.valid }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
