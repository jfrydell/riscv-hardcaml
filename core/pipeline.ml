(** Utilities for passing values through the 5-stage pipeline. *)

open! Core
open! Hardcaml

module Stage = struct
  type t =
    | F
    | D
    | X
    | M
    | W
  [@@deriving equal]
end

(* Represents a value as seen by each stage of the pipeline. The [@@deriving
   hardcaml] assumes a bit width of 1, but we internally use this for other
   pipeline values (for map2 and things). *)
module Pipelined_bit = struct
  type 'a t =
    { f : 'a
    ; d : 'a
    ; m : 'a
    ; x : 'a
    ; w : 'a
    }
  [@@deriving hardcaml]

  let at_stage ~(stage : Stage.t) { f; d; x; m; w } =
    match stage with
    | F -> f
    | D -> d
    | X -> x
    | M -> m
    | W -> w
  ;;

  (** [Stage.t] for each stage (useful with map2). *)
  let stage_ids : Stage.t t = { f = F; d = D; x = X; m = M; w = W }
end

(** Information needed to instantiate pipeline registers, managing propagation of
    instructions through the pipeline. *)
module Pipeline_info = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; stall : 'a Pipelined_bit.t
    (** Indicates that a stage should keep its current value instead of taking a new one. *)
    ; bubble : 'a Pipelined_bit.t
    (** Indicates that a stage should take on a default value instead of the previous one
        (unless stalled). *)
    }
  [@@deriving hardcaml]
end

(** Represents a value as seen by each stage of the pipeline, with a helper for
    constructing automatically from the value in one stage. *)
module Pipelined (Value : Interface.S) = struct
  type 'a t =
    { f : 'a Value.t
    ; d : 'a Value.t
    ; m : 'a Value.t
    ; x : 'a Value.t
    ; w : 'a Value.t
    }
  [@@deriving hardcaml]

  (** For mapping only one level deep (i.e. applying functions to values in each stage,
      possibly with heterogeneous types). *)
  type 'a untyped = 'a Value.t Pipelined_bit.t

  let to_untyped { f; d; x; m; w } : 'a Value.t Pipelined_bit.t = { f; d; x; m; w }
  let of_untyped ({ f; d; x; m; w } : 'a Value.t Pipelined_bit.t) = { f; d; x; m; w }

  (** Unassigned wire represents a stage which isn't populated for this value (for
      example, because it was produced later).

      TODO: is there any potential issue here if we passed a wire into [forward]? *)
  let invalid_stage = Value.const (Signal.wire 1)

  let is_invalid v = Value.equal Signal.equal v invalid_stage

  let forward
    ~(from_stage : Stage.t)
    ~(pipe_info : _ Pipeline_info.t)
    ?(default = Value.Of_signal.zero ())
    signal
    =
    let open Signal in
    (* Populate any stages which are currently empty while the previous is not. *)
    let forward_once (current : t untyped) : t untyped =
      let prev : t untyped =
        { f = invalid_stage; d = current.f; x = current.d; m = current.x; w = current.m }
      in
      Pipelined_bit.map4
        current
        prev
        pipe_info.stall
        pipe_info.bubble
        ~f:(fun current prev stall bubble ->
          if is_invalid current && not (is_invalid prev)
          then
            Value.Of_signal.reg
              (Types.Clocking.to_spec pipe_info.clocking)
              ~enable:~:stall
              (Value.Of_signal.mux2 bubble default prev)
          else current)
    in
    (* Start with just the inserted stage populated, then propagate until we reach W. If we are passed an invalid (empty) value to begin with, will never complete, but we can just return all invalid. *)
    let initial =
      Pipelined_bit.(
        map stage_ids ~f:(fun id ->
          if Stage.equal id from_stage then signal else invalid_stage))
    in
    let rec loop (v : t untyped) =
      if is_invalid signal || not (is_invalid v.w) then v else loop (forward_once v)
    in
    loop initial |> of_untyped
  ;;
end

(** Pipeline a 32-bit value. *)
module Pipelined_word = Pipelined (Types.Scalar (struct
    let port_name = ""
    let port_width = 32
  end))
