open! Core
open! Hardcaml
open Signal

let ensure_fairness scope ~clocking ~req_bits ~rotate =
  let%hw_list rotating_priority =
    let bits = List.map req_bits ~f:(fun _ -> wire 1) in
    let reg = Types.Clocking.reg clocking ~enable:rotate in
    List.iter2_exn (List.tl_exn bits) (List.drop_last_exn bits) ~f:(fun next prev ->
      next <-- reg prev);
    List.hd_exn bits <-- reg ~clear_to:vdd (List.last_exn bits);
    bits
  in
  let%hw_list req_prioritized = List.map2_exn req_bits rotating_priority ~f:( &: ) in
  let%hw someone_prioritized = tree ~arity:3 ~f:(reduce ~f:( |: )) req_prioritized in
  List.map2_exn req_bits req_prioritized ~f:(fun request prioritized ->
    prioritized ||: (request &&: ~:someone_prioritized))
;;

(** Arbitrate all unified memory-bus access types. Selection is held until [ready]
    completes the transaction, and both response data and [ready] are returned only to the
    requester that owns the transaction. *)
let arb scope ~clocking ~reqs ~(resp : _ Memory_bus.Bus.From_mem.t) =
  let num_reqs = List.length reqs in
  let%hw_list valid_reqs =
    List.map reqs ~f:(fun (request : _ Memory_bus.Bus.To_mem.t) -> request.valid)
  in
  let%hw owner_mask = wire num_reqs in
  let%hw owner_active = reduce ~f:( |: ) (bits_lsb owner_mask) in
  let%hw completing = owner_active &&: resp.ready in
  (* A requester keeps [valid] high through its completion cycle, so it must not be
     mistaken for a new request. Other requesters may be handed directly to the receiver
     on that cycle. *)
  let%hw_list eligible_reqs =
    List.map2_exn valid_reqs (bits_lsb owner_mask) ~f:(fun valid owns_transaction ->
      valid &&: (~:completing ||: ~:owns_transaction))
  in
  let%hw_list fair_valid_reqs =
    ensure_fairness scope ~clocking ~req_bits:eligible_reqs ~rotate:completing
  in
  let masks_with_valid =
    List.mapi fair_valid_reqs ~f:(fun index valid ->
      { With_valid.value = of_int_trunc ~width:num_reqs (1 lsl index); valid })
  in
  let grant = priority_select masks_with_valid in
  let%hw grant_mask = mux2 grant.valid grant.value (zero num_reqs) in
  owner_mask
  <-- Types.Clocking.reg clocking ~enable:(~:owner_active ||: completing) grant_mask;
  let%hw choose_new_request = ~:owner_active ||: completing in
  let%hw selected_mask = mux2 choose_new_request grant_mask owner_mask in
  let reqs_with_valid =
    List.map2_exn reqs (bits_lsb selected_mask) ~f:(fun value valid ->
      { With_valid.value; valid })
  in
  let selected_req = Memory_bus.Bus.To_mem.Of_signal.priority_select reqs_with_valid in
  let%hw.Memory_bus.Bus.To_mem.Of_signal req =
    { selected_req.value with valid = selected_req.valid }
  in
  (* The completing response still belongs to the old owner even though [req] may already
     contain the next request. Responses cannot arrive until the request owner has been
     latched. *)
  let%hw response_mask = owner_mask in
  let resps =
    List.map (bits_lsb response_mask) ~f:(fun owns_response ->
      { resp with
        valid = resp.valid &&: owns_response
      ; ready = resp.ready &&: owns_response
      })
  in
  req, resps
;;

let hierarchical ~scope ~clocking ~reqs ~resp =
  arb (Scope.sub_scope scope "arb") ~clocking ~reqs ~resp
;;
