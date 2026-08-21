open! Core
open Hardcaml

type 'a t =
  { sstatus : 'a [@bits 32] (** Restricted view of [mstatus]. *)
  ; sie : 'a [@bits 32] (** Restricted view of [mie]. *)
  ; stvec : 'a [@bits 32]
  ; sscratch : 'a [@bits 32]
  ; sepc : 'a [@bits 32]
  ; scause : 'a [@bits 32]
  ; stval : 'a [@bits 32]
  ; sip : 'a [@bits 32] (** Restricted view of [mip]. *)
  ; satp : 'a [@bits 32]
  ; misa : 'a [@bits 32]
  ; mvendorid : 'a [@bits 32]
  ; marchid : 'a [@bits 32]
  ; mimpid : 'a [@bits 32]
  ; mhartid : 'a [@bits 32]
  ; mstatus : 'a [@bits 32]
  ; mstatush : 'a [@bits 32] (** Read-only zero on this implementation. *)
  ; medeleg : 'a [@bits 32]
  ; mideleg : 'a [@bits 32]
  ; mie : 'a [@bits 32]
  ; mtvec : 'a [@bits 32]
  ; mscratch : 'a [@bits 32]
  ; mepc : 'a [@bits 32]
  ; mcause : 'a [@bits 32]
  ; mtval : 'a [@bits 32]
  ; mip : 'a [@bits 32]
  ; custom0 : 'a [@bits 32]
  ; custom1 : 'a [@bits 32]
  ; custom2 : 'a [@bits 32]
  ; custom3 : 'a [@bits 32]
  ; privilege : 'a [@bits 32]
  (** The current privilege mode. This is an implementation-internal CSR. *)
  }
[@@deriving hardcaml]

let addresses =
  { sstatus = 0x100
  ; sie = 0x104
  ; stvec = 0x105
  ; sscratch = 0x140
  ; sepc = 0x141
  ; scause = 0x142
  ; stval = 0x143
  ; sip = 0x144
  ; satp = 0x180
  ; misa = 0x301
  ; mstatus = 0x300
  ; mstatush = 0x310
  ; medeleg = 0x302
  ; mideleg = 0x303
  ; mie = 0x304
  ; mtvec = 0x305
  ; mscratch = 0x340
  ; mepc = 0x341
  ; mcause = 0x342
  ; mtval = 0x343
  ; mip = 0x344
  ; mvendorid = 0xf11
  ; marchid = 0xf12
  ; mimpid = 0xf13
  ; mhartid = 0xf14
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

(* This core currently implements RV32I with S and U privilege modes.  MISA is
   hardwired until extension enable/disable semantics are implemented. *)
let initial_misa = Signal.of_unsigned_int ~width:32 0x40140100

(** One bit per CSR. *)
module Mask = Wrap (Types.Value (struct
    let port_name = "valid"
    let port_width = 1
  end))

(** Utilities for working with mstatus. *)
module Mstatus = struct
  let sstatus_mask =
    Signal.of_unsigned_int
      ~width:32
      ((1 lsl 1) lor (1 lsl 5) lor (1 lsl 8) lor (1 lsl 18) lor (1 lsl 19))
  ;;

  let sstatus_view register = Signal.(register &: sstatus_mask)

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

(** Utilities for the implemented interrupt CSRs.

    Machine external interrupt is currently the only interrupt input. [sie] is a
    restricted view of [mie], exposing only interrupts delegated to S mode. *)
module Interrupt = struct
  let implemented_mask = Signal.of_unsigned_int ~width:32 (1 lsl 11)
  let sie_view ~mie ~mideleg = Signal.(mie &: mideleg &: implemented_mask)
  let sip_view ~mip ~mideleg = Signal.(mip &: mideleg &: implemented_mask)
end
