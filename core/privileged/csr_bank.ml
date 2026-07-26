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
    ; trap_write : 'a Trap_csr.Decode.I.t (** Writes caused by traps and trap returns. *)
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

let create scope ({ clocking; explicit_write; trap_write } : _ I.t) =
  let spec = Types.Clocking.to_spec clocking in
  let%hw.Csrs.Of_signal csrs = Csrs.Of_signal.wires () in
  (* Process explicit CSR writes. *)
  let%hw.Explicit_csr.Update.Of_signal decoded_explicit =
    Explicit_csr.Decode.hierarchical ~scope explicit_write
  in
  let%hw.Explicit_csr.Update.Of_signal delayed_decoded_explicit =
    Explicit_csr.Decode.O.Of_signal.pipeline
      ~n:latencies.explicit_decode
      spec
      decoded_explicit
  in
  (* Process trap CSR writes. *)
  let%hw.Trap_csr.Update.Of_signal decoded_trap =
    Trap_csr.Decode.hierarchical ~scope { trap_write with csrs }
  in
  (* Apply writes. *)
  let%hw.Csrs.Of_signal after_explicit =
    Explicit_csr.update ~update:delayed_decoded_explicit ~old_values:csrs
  in
  let%hw.Csrs.Of_signal after_trap =
    Trap_csr.update ~update:decoded_trap ~old_values:after_explicit
  in
  let clear_values =
    { (Csrs.map Csrs.addresses ~f:(fun _ -> Signal.zero 32)) with
      privilege = Signal.of_int_trunc ~width:32 3
    }
  in
  Csrs.Of_signal.assign
    csrs
    (Csrs.map2 after_trap clear_values ~f:(fun value clear_to ->
       Types.Clocking.reg clocking ~clear_to value));
  let%hw explicit_write_done =
    Types.Clocking.pipeline ~n:latencies.explicit_decode clocking explicit_write.valid
  in
  ({ csrs
   ; write_done = Signal.(explicit_write_done |: decoded_trap.trap |: decoded_trap.ret)
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
