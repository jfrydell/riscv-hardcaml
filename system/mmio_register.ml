open! Core
open! Hardcaml
open Signal

(** Input wires and output signals exposed as a single struct to use in some external
    port. *)
module Port = struct
  type 'a t =
    { read_value : 'a [@bits 32]
    (** Unassigned wire to be set to value to return to reads. *)
    ; write : 'a With_valid.t [@bits 32]
    (** Written value, valid for one cycle when a write occurs. *)
    ; read : 'a
    (** High for a cycle when a read occurs ([read_value] is sampled on this cycle). *)
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; from_bus : 'a Memory.Bus.To_mem.t
    ; read_value : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_bus : 'a Memory.Bus.From_mem.t
    ; write : 'a With_valid.t [@bits 32]
    ; read : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; from_bus; read_value } : _ I.t) =
  let write_request = from_bus.valid &&: from_bus.access_type.write_through in
  let write =
    { With_valid.valid = write_request; value = sel_bottom ~width:32 from_bus.data }
  in
  let read_request = from_bus.valid &&: from_bus.access_type.read_word in
  let read_response = Types.Clocking.reg clocking read_request in
  let read_addr = Types.Clocking.reg clocking ~enable:read_request from_bus.addr in
  let read_data = Types.Clocking.reg clocking ~enable:read_request read_value in
  let%hw.Memory.Bus.From_mem.Of_signal to_bus =
    { valid = read_response
    ; addr = read_addr
    ; data = uresize ~width:Memory.Bus.cpu_bus_width read_data
    ; last = read_response
    ; ready = vdd
    }
  in
  ({ to_bus; write; read = read_request } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~name:"mmio_register" create
;;
