open! Core
open Hardcaml

type 'a t =
  { mstatus : 'a [@bits 32]
  ; mstatush : 'a [@bits 32] (** Read-only zero on this implementation. *)
  ; mepc : 'a [@bits 32]
  ; mcause : 'a [@bits 32]
  ; mtval : 'a [@bits 32]
  ; custom0 : 'a [@bits 32]
  ; custom1 : 'a [@bits 32]
  ; custom2 : 'a [@bits 32]
  ; custom3 : 'a [@bits 32]
  }
[@@deriving hardcaml]

let addresses =
  { mstatus = 0x300
  ; mstatush = 0x301
  ; mepc = 0x341
  ; mcause = 0x342
  ; mtval = 0x343
  ; custom0 = 0x7c0
  ; custom1 = 0x7c1
  ; custom2 = 0x7c2
  ; custom3 = 0x7c3
  }
;;

(** Endow a set of CSR fields containing another Hardcaml interface with the combined
    Hardcaml interface functions. *)
module Wrap (Data : Interface.S) = struct
  module T = struct
    type nonrec 'a t = 'a Data.t t
    [@@deriving equal ~localize, compare ~localize, sexp_of]

    let port_names_and_widths =
      map port_names_and_widths ~f:(fun (csr_name, _) ->
        Data.map Data.port_names_and_widths ~f:(fun (field_name, width) ->
          csr_name ^ "$" ^ field_name, width))
    ;;

    let map t ~f = map t ~f:(Data.map ~f) [@nontail]
    let iter t ~f = iter t ~f:(Data.iter ~f) [@nontail]
    let map2 a b ~f = map2 a b ~f:(Data.map2 ~f) [@nontail]
    let iter2 a b ~f = iter2 a b ~f:(Data.iter2 ~f) [@nontail]
    let to_list t = List.concat_map (to_list t) ~f:Data.to_list
  end

  include T
  include Interface.Make (T)
end

(** One bit per CSR. *)
module Mask = Wrap (Types.Value (struct
    let port_name = "valid"
    let port_width = 1
  end))

(** Utilities for working with mstatus. *)
module Mstatus = struct
  let field_mask ~lsb ~width =
    Signal.sll (Signal.of_int_trunc ~width:32 ((1 lsl width) - 1)) ~by:lsb
  ;;

  (** Bitmask for SIE (supervisor interrupt enable) field. *)
  let sie = field_mask ~lsb:1 ~width:1

  (** Bitmask for MIE (machine interrupt enable) field. *)
  let mie = field_mask ~lsb:3 ~width:1

  (** Bitmask for SPIE (supervisor previous interrupt enable) field. *)
  let spie = field_mask ~lsb:5 ~width:1

  (** Bitmask for MPIE (machine previous interrupt enable) field. *)
  let mpie = field_mask ~lsb:7 ~width:1

  (** Bitmask for SPP (supervisor previous privilege) field. *)
  let spp = field_mask ~lsb:8 ~width:1

  (** Bitmask for MPP (machine previous privilege) field. *)
  let mpp = field_mask ~lsb:11 ~width:2

  (** Bitmask for MPRV (modify privilege) field. *)
  let mprv = field_mask ~lsb:17 ~width:1

  (** Bitmask for SUM (permit supervisor user memory access) field. *)
  let sum = field_mask ~lsb:18 ~width:1

  (** Bitmask for MXR (make executable readable) field. *)
  let mxr = field_mask ~lsb:19 ~width:1

  (** Bitmask for TVM (trap virtual memory) field. *)
  let tvm = field_mask ~lsb:20 ~width:1

  (** Bitmask for TW (timeout wait) field. *)
  let tw = field_mask ~lsb:21 ~width:1

  (** Bitmask for TSR (trap SRET) field. *)
  let tsr = field_mask ~lsb:22 ~width:1

  (** Bitmask for read only zero fields. Includes unimplemented WPRI fields, as well as
      endianness control, vector/float/additional state,. *)
  let wpri = Signal.of_bit_string "11111111100000011110011001010101"
end
