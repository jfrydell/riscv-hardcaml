open! Core
open! Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; state : 'a State.t
    ; from_tlb : 'a Iface.Tlb_request.t
    ; read_from_mem : 'a Memory_bus.From_mem.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_tlb : 'a Iface.Tlb_response.t
    ; read_to_mem : 'a Memory_bus.To_mem.t
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; state; from_tlb; read_from_mem } : _ I.t) =
  let%hw busy = wire 1 in
  let%hw accept = ~:busy &&: from_tlb.valid in
  let%hw vpn = Types.Clocking.reg clocking ~enable:accept from_tlb.vpn in
  let%hw response = read_from_mem.valid in
  (* 1 means translating high order bits via satp, 0 means lower bits based on returned PPN. *)
  let%hw req_level = wire 1 in
  let%hw resp_level = Types.Clocking.reg clocking req_level in
  req_level
  <-- (resp_level
       (* The cycle we get a response, increment level by 1. *)
       |> mux2 response (resp_level -:. 1)
       (* Once we're processing a new request, restart at 0. *)
       |> mux2 (Types.Clocking.reg clocking accept) (one 1));
  let%hw.Iface.Pte.Of_signal pte = Iface.Pte.of_bitvector read_from_mem.data in
  let%hw incoming_base_ppn = pte.ppn @: zero Iface.page_offset_width in
  let%hw base_ppn =
    Types.Clocking.cut_through_reg
      clocking
      ~enable:(response ||: accept)
      (mux2 accept state.page_table_root incoming_base_ppn)
  in
  let%hw offset =
    mux req_level
    @@ List.init 2 ~f:(fun l ->
      vpn
      |> drop_bottom ~width:(Iface.vpn_part_width * l)
      |> sel_bottom ~width:Iface.vpn_part_width
      |> uresize ~width:Iface.addr_width
      |> sll ~by:2)
  in
  let%hw read_addr = base_ppn +: offset in
  (* Process response, determining when we have translated successfully or encountered an error. *)
  let%hw error = ~:(pte.valid) ||: (pte.perm.write &&: ~:(pte.perm.read)) in
  let%hw invalid_superpage =
    mux resp_level
    @@ List.init 2 ~f:(fun l ->
      if l = 0
      then gnd
      else sel_bottom ~width:(Iface.vpn_part_width * l) pte.ppn |> no_bits_set |> ( ~: ))
  in
  let%hw.Iface.Tlb_entry.Of_signal entry =
    Iface.Tlb_entry.of_pte
      ~level:resp_level
      ~vpn
      ~asid:state.asid
      { pte with valid = pte.valid &&: ~:error &&: ~:invalid_superpage }
  in
  (* Finish translation if we've received page with R or X (superpage), on
     an error, or this was the last level (if this PTE incorrectly has R=X=0,
     pointing to another level, we'll page fault anyway). *)
  let%hw finish =
    response &&: (resp_level ==:. 0 ||: error ||: pte.perm.read ||: pte.perm.execute)
  in
  busy <-- Utils.sr ~set:accept ~reset:finish clocking ~style:`Mealy_reset ~priority:`Set;
  ({ to_tlb = { entry; valid = finish }
   ; read_to_mem =
       { valid = busy
       ; uncacheable = vdd
       ; access_type = Memory_bus.Access_type.read_word
       ; addr = read_addr
       ; data = zero Memory_bus.data_width
       ; size = zero 2
       ; last = gnd
       }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
