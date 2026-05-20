open! Core
open! Hardcaml
open Signal

module type Config = sig
  module Req : Interface.S
  module Resp : Interface.S

  val req_valid : t Req.t -> t
  val resp_done : t Resp.t -> t
  val mask_resp_valid : resp:t Resp.t -> t -> t Resp.t
end

module Make (Config : Config) = struct
  (** Ensure fairness for requests with a round-robin priority bit. If the prioritized
      request isn't asserted, this is ignored, but it ensures fairness in that a requestor
      will always be chosen one in N times where N is the number of requestors (just not
      for N the number of active requestors). *)
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
    List.map2_exn req_bits req_prioritized ~f:(fun r p ->
      p ||: (r &&: ~:someone_prioritized))
  ;;

  (** Creates an arbiter combining several [reqs] into one, and returning several [resp]s,
      with only the most recent requestor set to valid. Earlier [reqs] are prioritized in
      general but a priority bit rotating between all [reqs] ensures fairness if all are
      requesting constantly. *)
  let arb scope ~clocking ~reqs ~resp =
    let%hw_list valid_reqs = List.map reqs ~f:Config.req_valid in
    let%hw_list fair_valid_reqs =
      ensure_fairness scope ~clocking ~req_bits:valid_reqs ~rotate:(Config.resp_done resp)
    in
    (* Select a new requestor when a response arrives, or nobody is requesting. *)
    let%hw select_new =
      Config.resp_done resp ||: ~:(tree ~arity:3 ~f:(reduce ~f:( |: )) valid_reqs)
    in
    let%hw_list selected_valid =
      List.map
        fair_valid_reqs
        ~f:(Types.Clocking.cut_through_reg clocking ~enable:select_new)
    in
    (* Request is priority-selected from valid ones. *)
    let reqs_with_valid =
      List.map2_exn reqs selected_valid ~f:(fun value valid ->
        { With_valid.value; valid })
    in
    let req = (Config.Req.Of_signal.priority_select reqs_with_valid).value in
    (* Response is forwarded to whoever had the last accepted request. This is
       just whoever was selected on the previous cycle, as selection is held
       until we get a response. *)
    let masks_with_valid =
      List.mapi selected_valid ~f:(fun i valid ->
        { With_valid.value = zero (List.length reqs - 1 - i) @: vdd @: zero i; valid })
    in
    let%hw resp_mask =
      Types.Clocking.reg clocking (priority_select masks_with_valid).value
    in
    let resps = List.map (bits_lsb resp_mask) ~f:(Config.mask_resp_valid ~resp) in
    req, resps
  ;;

  let hierarchical ~scope ~clocking ~reqs ~resp =
    arb (Scope.sub_scope scope "arb") ~clocking ~reqs ~resp
  ;;
end

module Arb_read = Make (struct
    module Req = Iface.Read_block.To_mem
    module Resp = Iface.Read_block.From_mem

    let req_valid ({ load; _ } : _ Req.t) = load
    let resp_done ({ valid; last; _ } : _ Resp.t) = valid &&: last

    let mask_resp_valid ~(resp : _ Resp.t) valid =
      { resp with valid = resp.valid &&: valid }
    ;;
  end)

let arb_rd = Arb_read.hierarchical
