open! Core
open! Hardcaml
open Signal

let addr_width = 32
let page_offset_width = 12
let vpn_width = addr_width - page_offset_width
let ppn_width = addr_width - page_offset_width
let asid_width = 16

(** The bits of a 32-bit Sv32 page-table entry used by this simplified MMU. *)
module Pte = struct
  type 'a t =
    { ppn : 'a [@bits ppn_width]
    ; valid : 'a
    ; read : 'a
    ; write : 'a
    ; execute : 'a
    ; user : 'a
    ; global : 'a
    }
  [@@deriving hardcaml]

  (** Decode a PTE bitvector. Sv32 stores PPN in bits [31:10]; the two bits above the
      project's 32-bit physical-address range are discarded. *)
  let of_bitvector pte : Signal.t t =
    { ppn = pte |> drop_bottom ~width:10 |> sel_bottom ~width:ppn_width
    ; valid = pte.:(0)
    ; read = pte.:(1)
    ; write = pte.:(2)
    ; execute = pte.:(3)
    ; user = pte.:(4)
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
    ; valid : 'a
    ; read : 'a
    ; write : 'a
    ; execute : 'a
    ; user : 'a
    ; global : 'a
    }
  [@@deriving hardcaml]

  (** Build a cached translation from a VPN and decoded PTE. The ASID is optional for
      address-only conversions; the page-table walker supplies it when filling the TLB. *)
  let of_pte ?asid ~vpn (pte : Signal.t Pte.t) : Signal.t t =
    { vpn
    ; ppn = pte.ppn
    ; asid = Option.value asid ~default:(zero asid_width)
    ; valid = pte.valid
    ; read = pte.read
    ; write = pte.write
    ; execute = pte.execute
    ; user = pte.user
    ; global = pte.global
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
    to be held stable until the next translation request arrives. TODO: extend to support
    page faults *)
module Translation = struct
  type 'a t =
    { pa : 'a [@bits addr_width] (** The translated address. *)
    ; valid : 'a (** The address was translated successfully. *)
    ; stall : 'a
    (** The last requested translation is in progress, so the values output here are
        invalid. *)
    }
  [@@deriving hardcaml]

  (** Implement correct timing behavior, latching output when a response arrives and
      raising stall between request and response. Ignores input [stall] value, and all
      inputs when [response_valid] is low. *)
  let with_latches ~accept_request ~response_valid ~clocking t =
    { (map t ~f:(Types.Clocking.cut_through_reg clocking ~enable:response_valid)) with
      stall =
        Utils.sr
          ~set:accept_request
          ~reset:response_valid
          clocking
          ~style:`Mealy_reset
          ~priority:`Set
    }
  ;;
end
