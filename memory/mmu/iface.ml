open! Core
open! Hardcaml
open Signal

(* TODO: change physical addr width to 34? *)
(* TODO: some of these in Memory_bus? *)
let addr_width = 32
let page_offset_width = 12
let vpn_width = addr_width - page_offset_width
let ppn_width = addr_width - page_offset_width
let vpn_part_width = 10
let asid_width = 9

(** Permission-setting bits in a PTE/TLB. *)
module Permission = struct
  type 'a t =
    { read : 'a
    ; write : 'a
    ; execute : 'a
    ; user : 'a
    }
  [@@deriving hardcaml]
end

(** The bits of a 32-bit Sv32 page-table entry used by this simplified MMU. *)
module Pte = struct
  type 'a t =
    { ppn : 'a [@bits ppn_width]
    ; valid : 'a
    ; perm : 'a Permission.t
    ; global : 'a
    }
  [@@deriving hardcaml]

  (** Decode a PTE bitvector. Sv32 stores PPN in bits [31:10]; the two bits above the
      project's 32-bit physical-address range are discarded. *)
  let of_bitvector pte : Signal.t t =
    { ppn = pte |> drop_bottom ~width:10 |> sel_bottom ~width:ppn_width
    ; valid = pte.:(0)
    ; perm = { read = pte.:(1); write = pte.:(2); execute = pte.:(3); user = pte.:(4) }
    ; global = pte.:(5)
    }
  ;;

  let of_signal = of_bitvector
end

(** A cached translation. *)
module Tlb_entry = struct
  type 'a t =
    { vpn : 'a [@bits vpn_width]
    ; ppn : 'a [@bits ppn_width]
    ; asid : 'a [@bits asid_width]
    ; perm : 'a Permission.t
    ; global : 'a
    ; entry_valid : 'a
    (** This refers to whether this entry in the TLB BRAM is a valid entry from the PT.
        This is not PTE.valid, which is instead reflected in the [perm] bits (all zero if
        PTE.valid is false). *)
    }
  [@@deriving hardcaml]

  (** Build a cached translation from a VPN and decoded PTE. The ASID is optional for
      address-only conversions; the page-table walker supplies it when filling the TLB.

      If [level > 0], lower PPN bits are filled in from the VPN for superpage translation. *)
  let of_pte ?asid ~level ~vpn (pte : Signal.t Pte.t) : Signal.t t =
    let ppn =
      mux level
      @@ List.init 2 ~f:(fun l ->
        if l = 0
        then pte.ppn
        else drop_bottom ~width:(10 * l) pte.ppn @: sel_bottom ~width:(10 * l) vpn)
    in
    { vpn
    ; ppn
    ; asid = Option.value asid ~default:(zero asid_width)
    ; perm = Permission.map ~f:(( &: ) pte.valid) pte.perm
    ; global = pte.global
    ; entry_valid = vdd
    }
  ;;
end

(** A request from the TLB to the page-table walker. *)
module Tlb_request = struct
  type 'a t =
    { vpn : 'a [@bits vpn_width]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

(** A response from the page-table walker to the TLB. *)
module Tlb_response = struct
  type 'a t =
    { entry : 'a Tlb_entry.t
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

(** The result of address translation, sent to the CPU. Unless noted otherwise, guaranteed
    to be held stable until the next translation request arrives or memory state changes. *)
module Translation = struct
  type 'a t =
    { pa : 'a [@bits addr_width] (** The translated address. *)
    ; valid : 'a
    (** The address was translated successfully and can be accessed at [pa]. *)
    ; fault : 'a
    (** There was an error in the access translation, so a page fault should be raised. *)
    ; io : 'a (** The address is in the uncachable I/O region. *)
    ; stall : 'a
    (** The last requested translation is in progress, so the values output here are
        invalid. TODO: always lower valid/fault during stall so we can use individually? *)
    }
  [@@deriving hardcaml]
end
