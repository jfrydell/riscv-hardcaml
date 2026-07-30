(** Routes memory requests to one of two destinations depending on a predicate.

    The main complicated part is the ready signal. When switching from one destination to
    another, we cannot raise [ready] and accept a new request until the previous
    transaction is done. Otherwise, we could get two responses at once from the two
    destinations. This also means we can't actually route the request to the correct
    destination until the previous one is done, either.

    To support a destination that does allow for pipelining (despite none being
    implemented yet), we only do this waiting when switching destinations. (It's possible
    this half-supported pipelining is not worth the effort, though, as we may eventually
    want response flow control or even reordering.) *)

open! Core
open! Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; in_req : 'a Bus.To_mem.t
    ; in_resp_t : 'a Bus.From_mem.t
    ; in_resp_f : 'a Bus.From_mem.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { out_resp : 'a Bus.From_mem.t
    ; out_req_t : 'a Bus.To_mem.t
    ; out_req_f : 'a Bus.To_mem.t
    }
  [@@deriving hardcaml]
end

let create ~addr_pred scope ({ clocking; in_req; in_resp_t; in_resp_f } : _ I.t) : _ O.t =
  let%hw awaiting_response = wire 1 in
  let%hw req_route = addr_pred in_req.addr in
  let%hw current_resp_route = wire 1 in
  let%hw route_allowed = req_route ==: current_resp_route ||: ~:awaiting_response in
  current_resp_route <-- Clocking.reg clocking ~enable:route_allowed req_route;
  let%hw ready = mux2 req_route in_resp_t.ready in_resp_f.ready &&: route_allowed in
  let%hw last =
    in_resp_t.valid
    &&: in_resp_t.last
    ||: (in_resp_f.valid &&: in_resp_f.last)
    ||: Clocking.reg
          clocking
          (ready &&: in_req.valid &&: Bus.Access_type.is_write in_req.access_type)
  in
  awaiting_response
  <-- Utils.sr
        ~style:`Mealy_reset
        ~priority:`Set
        clocking
        ~set:(in_req.valid &&: route_allowed)
        ~reset:last;
  { out_resp =
      { (Bus.From_mem.Of_signal.mux2 current_resp_route in_resp_t in_resp_f) with ready }
  ; out_req_t = { in_req with valid = in_req.valid &&: req_route &&: route_allowed }
  ; out_req_f = { in_req with valid = in_req.valid &&: ~:req_route &&: route_allowed }
  }
;;

let hierarchical ~addr_pred =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical (create ~addr_pred)
;;
