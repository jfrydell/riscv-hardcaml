open! Core
open! Hardcaml
open Signal

let vpn_part_width = 10

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
  (* 0 means translating high order bits via satp, 1 means lower bits based on returned PPN. *)
  let%hw level = wire 1 in
  let%hw prev_level = Types.Clocking.reg clocking level in
  level
  <-- (prev_level
       (* The cycle we get a response, increment level by 1. *)
       |> mux2 response (prev_level +:. 1)
       (* Once we're processing a new request, restart at 0. *)
       |> mux2 (Types.Clocking.reg clocking accept) (zero 1));
  let%hw.Iface.Pte.Of_signal pte = Iface.Pte.of_bitvector read_from_mem.data in
  let%hw incoming_base_ppn = pte.ppn @: zero Iface.page_offset_width in
  let%hw base_ppn =
    Types.Clocking.cut_through_reg
      clocking
      ~enable:(response ||: accept)
      (mux2 accept state.page_table_root incoming_base_ppn)
  in
  let%hw offset =
    mux level
    @@ List.init 2 ~f:(fun l ->
      vpn
      |> drop_top ~width:(vpn_part_width * l)
      |> sel_top ~width:vpn_part_width
      |> uresize ~width:Iface.addr_width
      |> sll ~by:2)
  in
  let%hw read_addr = base_ppn +: offset in
  let%hw.Iface.Tlb_entry.Of_signal leaf_entry =
    Iface.Tlb_entry.of_pte ~vpn ~asid:state.asid pte
  in
  let%hw last_response = prev_level ==:. 1 &&: response in
  busy
  <-- Utils.sr
        ~set:accept
        ~reset:last_response
        clocking
        ~style:`Mealy_reset
        ~priority:`Set;
  ({ to_tlb = { entry = leaf_entry; valid = last_response }
   ; read_to_mem =
       { valid = busy
       ; access_type = Memory_bus.Access_type.read_word
       ; addr = read_addr
       ; data = zero Memory_bus.cpu_bus_width
       ; store_size = zero 2
       ; last = gnd
       }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
