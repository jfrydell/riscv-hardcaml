open! Core
open Hardcaml
open Signal

(** A one-hot shift register, rotating left when [advance] is high. *)
let rotating_onehot ~clocking ~advance ~width =
  Types.Clocking.reg_fb clocking ~enable:advance ~width ~f:(fun v ->
    rotl (v ^:. 1) ~by:1 ^:. 1)
  ^:. 1
;;

(** An SR latch. *)
let sr ?(priority = `Reset) ?(style = `Moore) ?enable clocking ~set ~reset =
  let prev_val = wire 1 in
  let new_val =
    match priority with
    | `Set -> prev_val &: ~:reset |: set
    | `Reset -> prev_val |: set &: ~:reset
  in
  prev_val <-- Types.Clocking.reg ?enable clocking new_val;
  match style with
  | `Moore -> prev_val
  | `Mealy -> new_val
  | `Mealy_set -> prev_val |: set
  | `Mealy_reset -> prev_val &: ~:reset
;;

(** Mux a signal with 0. *)
let zero_if cond s = mux2 cond (zero (width s)) s

let zero_unless cond s = mux2 cond s (zero (width s))

(** Accumulator. *)
let accumulator ~clocking ?enable val_ =
  Types.Clocking.reg_fb ?enable clocking ~width:(width val_) ~f:(fun s -> s +: val_)
;;

(** Shift register thing, outputting [len] signals starting with input and delaying each
    by a cycle. *)
let rec shreg ?enable clocking ~len s =
  if len = 0
  then []
  else s :: shreg ?enable clocking ~len:(len - 1) (Types.Clocking.reg clocking ?enable s)
;;

(** A wrapper around [Always.Variable] with both the (assignable) value for the next cycle
    and its value from the previous, combining a register and wire. *)
module Var = struct
  type t =
    { prev : Signal.t
    ; next : Always.Variable.t
    }

  let make clocking ~width =
    let prev = wire width in
    let next = Always.Variable.wire ~default:prev () in
    prev <-- Types.Clocking.reg clocking next.value;
    { prev; next }
  ;;

  let apply_names ?(prefix = "") ?(suffix = "") ?(naming_op = ( -- )) { prev; next } =
    ignore @@ naming_op prev (prefix ^ "prev" ^ suffix);
    ignore @@ naming_op next.value (prefix ^ "next" ^ suffix)
  ;;
end
