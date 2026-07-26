open! Core
open Hardcaml

(** Latency configuration for CSR updates. *)
module Latencies = struct
  type t =
    { explicit_decode : int
    (** Number of cycles to add for decoding CSR instruction (address decode and routing
        to CSRs). *)
    }
end

let latencies : Latencies.t = { explicit_decode = 2 }

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; explicit_write : 'a Explicit_csr.Decode.I.t
    (** Writes from CSR update instructions. *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { csrs : 'a Csrs.t
    ; write_done : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; explicit_write } : _ I.t) =
  let spec = Types.Clocking.to_spec clocking in
  (* Process explicit CSR writes. *)
  let%hw.Explicit_csr.Decode.O.Of_signal decoded_explicit =
    Explicit_csr.Decode.hierarchical ~scope explicit_write
  in
  let%hw.Explicit_csr.Decode.O.Of_signal delayed_decoded_explicit =
    Explicit_csr.Decode.O.Of_signal.pipeline
      ~n:latencies.explicit_decode
      spec
      decoded_explicit
  in
  (* Apply writes. *)
  let%hw.Csrs.Of_signal csrs = Csrs.Of_signal.wires () in
  let%hw.Csrs.Of_signal after_explicit =
    Explicit_csr.update ~update:delayed_decoded_explicit ~old_values:csrs
  in
  Csrs.Of_signal.assign csrs (Csrs.map ~f:(Types.Clocking.reg clocking) after_explicit);
  ({ csrs
   ; write_done =
       Types.Clocking.pipeline ~n:latencies.explicit_decode clocking explicit_write.valid
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
