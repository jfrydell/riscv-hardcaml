open! Core
open Hardcaml

type 'a t =
  { custom0 : 'a
  ; custom1 : 'a
  ; custom2 : 'a
  ; custom3 : 'a
  }
[@@deriving hardcaml]

let addresses = { custom0 = 0x7c0; custom1 = 0x7c1; custom2 = 0x7c2; custom3 = 0x7c3 }

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

module Value = Types.Scalar (struct
    let port_name = "value"
    let port_width = 32
  end)

module Values = Wrap (Value)
