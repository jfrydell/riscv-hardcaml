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
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a Iface.Translation.t
    ; to_walker : 'a Iface.Tlb_request.t
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; state; va; from_walker } : _ I.t) =
  (* [busy] remains asserted from accepting a VA until either a hit is found or
     a walker response arrives. *)
  let%hw busy = wire 1 in
  let%hw.With_valid.Of_signal active =
    With_valid.map ~f:(Types.Clocking.reg clocking ~enable:~:busy) va
  in
  let%hw active_vpn = vpn_of_va active.value in
  let%hw accept = ~:busy &&: va.valid in
  let%hw.State.Of_signal state_latched =
    State.map ~f:(Types.Clocking.reg clocking ~enable:accept) state
  in
  let%hw fill = from_walker.valid in
  let%hw.Tlb_entry.Of_signal loaded_entry =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_entries
        ~write_ports:
          [| { write_clock = clocking.clock
             ; write_enable = fill
             ; write_address = index_of_vpn from_walker.entry.vpn
             ; write_data = Tlb_entry.Of_signal.pack from_walker.entry
             }
          |]
        ~read_ports:
          [| { read_clock = clocking.clock
             ; read_enable = accept
             ; read_address = index_of_vpn (vpn_of_va va.value)
             }
          |]
        ~name:"tlb_entries"
        ()
    in
    Tlb_entry.Of_signal.unpack mem.(0)
  in
  let%hw vpn_match = loaded_entry.vpn ==: active_vpn in
  let%hw asid_match = loaded_entry.asid ==: state_latched.asid in
  let%hw hit =
    active.valid
    &&: loaded_entry.valid
    &&: vpn_match
    &&: (asid_match ||: loaded_entry.global)
  in
  let%hw miss = busy &&: ~:hit in
  let%hw.Tlb_entry.Of_signal result_entry =
    Tlb_entry.Of_signal.mux2 fill from_walker.entry loaded_entry
  in
  let%hw result_valid = hit ||: fill in
  let%hw result_value = physical_address ~va:active.value ~entry:result_entry in
  (* Stall until we get response back, and latch result once that happens. *)
  busy
  <-- Utils.sr ~set:accept ~reset:result_valid clocking ~style:`Mealy_reset ~priority:`Set;
  let hold_when_valid = Types.Clocking.cut_through_reg clocking ~enable:result_valid in
  ({ result = { pa = hold_when_valid result_value; valid = vdd; stall = busy }
   ; to_walker = { vpn = active_vpn; valid = miss }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
