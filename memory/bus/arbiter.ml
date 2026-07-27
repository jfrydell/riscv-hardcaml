open! Core
open! Hardcaml
open Signal

module type Config = sig
  val port_count : int
end

module Make (Config : Config) = struct
  let port_count = Config.port_count

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; up_req : 'a Bus.To_mem.t list [@length port_count]
      ; dn_resp : 'a Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { up_resp : 'a Bus.From_mem.t list [@length port_count]
      ; dn_req : 'a Bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; up_req; dn_resp } : _ I.t) =
    (* For fairness, we keep a rotating register ensuring an always-requesting register is selected 1/port_count of the time.
       If the currently-priorized register is not selected, though, we prioritize in order of [up_req]. *)
    let%hw rotate_fairness = dn_resp.ready in
    let%hw rotating_fairness =
      Types.Clocking.reg_fb
        clocking
        ~enable:rotate_fairness
        ~width:port_count
        ~f:(fun v ->
          (* TODO: clear_to 1 and wrap at end *)
          mux2 (v ==:. 0) (one port_count) (sll v ~by:1))
    in
    let%hw req_onehot =
      List.fold_map up_req ~init:gnd ~f:(fun already_chose { valid; _ } ->
        already_chose ||: valid, mux2 already_chose gnd valid)
      |> snd
      |> concat_lsb
    in
    let%hw chosen_req =
      mux2
        (rotating_fairness
         &: concat_lsb (List.map ~f:(fun { valid; _ } -> valid) up_req)
         <>:. 0)
        rotating_fairness
        req_onehot
    in
    (* Select req to mem based on [chosen_req]. We do everything
       combinationally; if timing is bad, registers / skid buffers can be
       placed on either side. *)
    let%hw.Bus.To_mem.Of_signal dn_req =
      List.map2_exn up_req (split_lsb ~part_width:1 chosen_req) ~f:(fun req valid ->
        { With_valid.value = req; valid })
      |> Bus.To_mem.Of_signal.onehot_select
    in
    (* Once memory accepts the currently-chosen req, latch it in to route
       responses correctly. The response is guaranteed to be done ([last]) by
       the time [ready] rises, so the entire thing will be routed correctly. *)
    let%hw route_resp = Types.Clocking.reg ~enable:dn_resp.ready clocking chosen_req in
    (* We are ready for a request only from the one we are currently accepting
       (gives combinational valid->ready dependence; can break with register on
       input if necessary). *)
    let%hw_list.Bus.From_mem.Of_signal up_resp =
      List.map2_exn
        (split_lsb ~part_width:1 chosen_req)
        (split_lsb ~part_width:1 route_resp)
        ~f:(fun chosen routing ->
          { dn_resp with
            ready = dn_resp.ready &&: chosen
          ; valid = dn_resp.valid &&: routing
          })
    in
    ({ up_resp; dn_req } : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical create
  ;;
end

module Two = Make (struct
    let port_count = 2
  end)

module Three = Make (struct
    let port_count = 3
  end)

module Four = Make (struct
    let port_count = 4
  end)
