open! Core
open! Hardcaml
open Signal

let bits_index = 4
let num_entries = 1 lsl bits_index

module Tlb_entry = Iface.Tlb_entry

let vpn_of_va va = drop_bottom ~width:Iface.page_offset_width va
let index_of_vpn vpn = sel_bottom ~width:bits_index vpn

let physical_address ~va ~(entry : Signal.t Iface.Tlb_entry.t) =
  concat_lsb [ sel_bottom ~width:Iface.page_offset_width va; entry.ppn ]
;;

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; state : 'a State.t
    ; va : 'a With_valid.t [@bits Iface.addr_width]
    ; from_walker : 'a Iface.Tlb_response.t
    ; trigger_flush : 'a
    ; required_permission : 'a Iface.Permission.t
    (** Bits of permission which must be set to avoid a fault; latched with [va]. *)
    ; require_superuser : 'a
    (** Requires the U bit to be clear to access (set for supervisor with SUM=0), latched
        with [va]. *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a Iface.Translation.t
    (** Result to send to CPU. Doesn't maintain mutual exclusivity of [valid]/[fault] and
        [stall] (the latter takes priority). *)
    ; to_walker : 'a Iface.Tlb_request.t
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) =
  (* [busy] remains asserted from accepting a VA until an entry is loaded from
     the PT. At the moment, all entries from the PT are written to the TLB,
     even invalid ones, so all accesses eventually "hit" (just possibly on an
     entry with valid=0). To make this work, we use Tlb_entry.entry_valid to
     mean "this entry corresponds to one from the PT", even if that PTE had
     V=0, in which case all TLB pemissions are 0. This maybe isn't ideal, as it
     pollutes the TLB with faulting accesses, but it simplifies this logic (and
     if that's happening often, arguably we do want to cache those faults?). *)
  let%hw busy = wire 1 in
  let%hw accept = ~:busy &&: i.va.valid in
  let%hw.I.Of_signal active = I.map ~f:(Types.Clocking.reg i.clocking ~enable:accept) i in
  let%hw active_vpn = vpn_of_va active.va.value in
  (* Flush one entry per cycle. The extra read port lets us preserve the global
     bit while invalidating all non-global entries. *)
  let%hw flushing = wire 1 in
  let%hw flush_write_valid = Types.Clocking.reg i.clocking flushing in
  let%hw flush_write_enable = flushing &&: flush_write_valid in
  let%hw flushing_addr =
    Types.Clocking.reg_fb
      i.clocking
      ~clear_to:(zero bits_index)
      ~enable:(flushing ||: i.trigger_flush)
      ~width:bits_index
      ~f:(fun addr -> mux2 i.trigger_flush (zero bits_index) (addr +:. 1))
  in
  let%hw flush_write_address = flushing_addr -:. 1 in
  let%hw flush_done = flush_write_enable &&: all_bits_set flush_write_address in
  flushing <-- Utils.sr ~set:i.trigger_flush ~reset:flush_done i.clocking;
  let%hw.Tlb_entry.Of_signal flushing_entry = Tlb_entry.Of_signal.wires () in
  let loaded_entry_from_mem =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_entries
        ~write_ports:
          [| { write_clock = i.clocking.clock
             ; write_enable = flush_write_enable ||: (i.from_walker.valid &&: ~:flushing)
             ; write_address =
                 mux2
                   flush_write_enable
                   flush_write_address
                   (index_of_vpn i.from_walker.entry.vpn)
             ; write_data =
                 Tlb_entry.Of_signal.pack
                   (Tlb_entry.Of_signal.mux2
                      flush_write_enable
                      { flushing_entry with
                        entry_valid = flushing_entry.entry_valid &&: flushing_entry.global
                      }
                      i.from_walker.entry)
             }
          |]
        ~read_ports:
          [| { read_clock = i.clocking.clock
             ; read_enable = accept
             ; read_address = index_of_vpn (vpn_of_va i.va.value)
             }
           ; { read_clock = i.clocking.clock
             ; read_enable = flushing
             ; read_address = flushing_addr
             }
          |]
        ~name:"tlb_entries"
        ()
    in
    Tlb_entry.Of_signal.assign flushing_entry (Tlb_entry.Of_signal.unpack mem.(1));
    Tlb_entry.Of_signal.unpack mem.(0)
  in
  let%hw.Tlb_entry.Of_signal loaded_entry =
    { loaded_entry_from_mem with
      entry_valid = loaded_entry_from_mem.entry_valid &&: ~:flushing
    }
  in
  let%hw vpn_match = loaded_entry.vpn ==: active_vpn in
  let%hw asid_match = loaded_entry.asid ==: active.state.asid in
  let%hw hit =
    active.va.valid
    &&: loaded_entry.entry_valid
    &&: vpn_match
    &&: (asid_match ||: loaded_entry.global)
  in
  let%hw result_pa = physical_address ~va:active.va.value ~entry:loaded_entry in
  (* We fault if entry doesn't have all requested permissions, or if U is set
     for supervisor-only accesses. Other access faults in the PT are reflected
     in walker filling the TLB with a permission-less entry. *)
  let%hw missing_permission =
    Iface.Permission.map2 active.required_permission loaded_entry.perm ~f:(fun r p ->
      r &: ~:p)
    |> Iface.Permission.to_list
    |> reduce ~f:( ||: )
  in
  let%hw result_fault =
    missing_permission ||: (loaded_entry.perm.user &&: active.require_superuser)
  in
  (* Stall until response from walker causes us to hit, and latch result once that happens. *)
  busy <-- Utils.sr ~set:accept ~reset:hit i.clocking ~style:`Mealy_reset ~priority:`Set;
  let hold_after_hit = Types.Clocking.cut_through_reg i.clocking ~enable:hit in
  ({ result =
       { pa = hold_after_hit result_pa
       ; io = hold_after_hit result_pa.:(Iface.addr_width - 1)
       ; valid = hold_after_hit ~:result_fault
       ; fault = hold_after_hit result_fault
       ; stall = busy
       }
   ; to_walker = { vpn = active_vpn; valid = busy }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
