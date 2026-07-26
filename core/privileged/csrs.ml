open! Core
open Hardcaml

type 'a t =
  { mstatus : 'a [@bits 32]
  ; mstatush : 'a [@bits 32] (** Read-only zero on this implementation. *)
  ; sepc : 'a [@bits 32]
  ; scause : 'a [@bits 32]
  ; stval : 'a [@bits 32]
  ; mepc : 'a [@bits 32]
  ; mcause : 'a [@bits 32]
  ; mtval : 'a [@bits 32]
  ; custom0 : 'a [@bits 32]
  ; custom1 : 'a [@bits 32]
  ; custom2 : 'a [@bits 32]
  ; custom3 : 'a [@bits 32]
  ; privilege : 'a [@bits 32]
  (** The current privilege mode. This is an implementation-internal CSR. *)
  }
[@@deriving hardcaml]

let addresses =
  { mstatus = 0x300
  ; mstatush = 0x301
  ; sepc = 0x141
  ; scause = 0x142
  ; stval = 0x143
  ; mepc = 0x341
  ; mcause = 0x342
  ; mtval = 0x343
  ; custom0 = 0x7c0
  ; custom1 = 0x7c1
  ; custom2 = 0x7c2
  ; custom3 = 0x7c3
  ; privilege = 0xfff
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
  module Fields = struct
    type 'a t =
      { sie : 'a
      ; mie : 'a
      ; spie : 'a
      ; mpie : 'a
      ; spp : 'a
      ; mpp : 'a [@bits 2]
      ; mprv : 'a
      ; sum : 'a
      ; mxr : 'a
      ; tvm : 'a
      ; tw : 'a
      ; tsr : 'a
      }
    [@@deriving hardcaml]

    let of_register register =
      { sie = Signal.select register ~high:1 ~low:1
      ; mie = Signal.select register ~high:3 ~low:3
      ; spie = Signal.select register ~high:5 ~low:5
      ; mpie = Signal.select register ~high:7 ~low:7
      ; spp = Signal.select register ~high:8 ~low:8
      ; mpp = Signal.select register ~high:12 ~low:11
      ; mprv = Signal.select register ~high:17 ~low:17
      ; sum = Signal.select register ~high:18 ~low:18
      ; mxr = Signal.select register ~high:19 ~low:19
      ; tvm = Signal.select register ~high:20 ~low:20
      ; tw = Signal.select register ~high:21 ~low:21
      ; tsr = Signal.select register ~high:22 ~low:22
      }
    ;;

    let to_register { sie; mie; spie; mpie; spp; mpp; mprv; sum; mxr; tvm; tw; tsr } =
      Signal.concat_msb
        [ Signal.zero 9
        ; tsr
        ; tw
        ; tvm
        ; mxr
        ; sum
        ; mprv
        ; Signal.zero 4
        ; mpp
        ; Signal.zero 2
        ; spp
        ; mpie
        ; Signal.gnd
        ; spie
        ; Signal.gnd
        ; mie
        ; Signal.gnd
        ; sie
        ; Signal.gnd
        ]
    ;;
  end
end
