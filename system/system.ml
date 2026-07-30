open! Core
open! Hardcaml

module type Config = sig
  module Cpu : Cpu.Config
end

(** Wires to emit accesses to the memory/IO bus. *)
module Access_emitting_port = struct
  type 'a t =
    { from_bus : 'a Memory.Bus.From_mem.t
    ; to_bus : 'a Memory.Bus.To_mem.t
    }
  [@@deriving hardcaml]
end

(** Wires to handle accesses from the memory/IO bus. *)
module Access_handling_port = struct
  type 'a t =
    { from_bus : 'a Memory.Bus.To_mem.t
    ; to_bus : 'a Memory.Bus.From_mem.t
    }
  [@@deriving hardcaml]
end

module Make (Config : Config) = struct
  module Cpu = Cpu.Make (Config.Cpu)

  type 'a t =
    { scope : Scope.t
    ; clocking : 'a Types.Clocking.t (** Clock and reset, assigned at creation. *)
    ; cpu : 'a Cpu.O.t (** CPU outputs, assigned at creation. *)
    ; mutable open_emitter : 'a Access_emitting_port.t
    (** A port to connect devices which emit accesses onto the bus. From creation until
        completion, [to_bus] is unassigned wires going into an arbiter, and [from_bus] is
        the already-assigned response. *)
    ; mutable open_handler : 'a Access_handling_port.t
    (** A port to connect devices which receive accesses from the bus. From creation until
        completion, [to_bus] is unassigned wires going into a request-address-based mux,
        and [from_bus] contains any request which hasn't been handled already. *)
    }

  (** Get a new port to emit accesses on, arbiterating with all others. [to_bus] is the
      unassigned wires issuing writes, and [from_bus] is the response. *)
  let add_access_emitter ({ scope; clocking; open_emitter = prev_open_emitter; _ } as t) =
    let%hw.Access_emitting_port.Of_signal open_emitter =
      Access_emitting_port.Of_signal.wires ()
    in
    let%hw.Access_emitting_port.Of_signal connected_emitter =
      Access_emitting_port.Of_signal.wires ()
    in
    (* Connect new open emitter and new connected one to previously open one via arbiter. *)
    let%hw.Memory.Bus.Arbiter.Two.O.Of_signal emitter_arb =
      Memory.Bus.Arbiter.Two.hierarchical
        ~scope
        { clocking
        ; up_req = [ open_emitter.to_bus; connected_emitter.to_bus ]
        ; dn_resp = prev_open_emitter.from_bus
        }
    in
    Memory.Bus.To_mem.Of_signal.assign prev_open_emitter.to_bus emitter_arb.dn_req;
    Memory.Bus.From_mem.Of_signal.assign
      open_emitter.from_bus
      (List.nth_exn emitter_arb.up_resp 0);
    Memory.Bus.From_mem.Of_signal.assign
      connected_emitter.from_bus
      (List.nth_exn emitter_arb.up_resp 1);
    t.open_emitter <- open_emitter;
    connected_emitter
  ;;

  (** Get a new port to handle accesses satisfying the given predicate (excluding those
      handled by previous calls to [add_access_handler]. [to_bus] is the unassigned wires
      issuing writes, and [from_bus] is the response. *)
  let add_access_handler
    ~addr_pred
    ({ scope; clocking; open_handler = prev_open_handler; _ } as t)
    =
    let%hw.Access_handling_port.Of_signal open_handler =
      Access_handling_port.Of_signal.wires ()
    in
    let%hw.Access_handling_port.Of_signal connected_handler =
      Access_handling_port.Of_signal.wires ()
    in
    (* New connected handler only sees addresses in predicate, and new open port gets the remainder. *)
    let%hw.Memory.Bus.Router.O.Of_signal router =
      Memory.Bus.Router.hierarchical
        ~addr_pred
        ~scope
        { clocking
        ; in_req = prev_open_handler.from_bus
        ; in_resp_t = connected_handler.to_bus
        ; in_resp_f = open_handler.to_bus
        }
    in
    Memory.Bus.From_mem.Of_signal.assign prev_open_handler.to_bus router.out_resp;
    Memory.Bus.To_mem.Of_signal.assign connected_handler.from_bus router.out_req_t;
    Memory.Bus.To_mem.Of_signal.assign open_handler.from_bus router.out_req_f;
    t.open_handler <- open_handler;
    connected_handler
  ;;

  (** Get clocking info. *)
  let clocking { clocking; _ } = clocking

  (** Get CPU outputs. *)
  let cpu { cpu; _ } = cpu

  let create ~scope ~clocking =
    (* Start with an I/O bus unattached to anything. Each of these wires acts as the already-connected [from_bus] from one side's open port, and the unassigned wire [to_bus] for the other. *)
    let%hw.Memory.Bus.To_mem.Of_signal emitter_to_handler =
      Memory.Bus.To_mem.Of_signal.wires ()
    in
    let%hw.Memory.Bus.From_mem.Of_signal handler_to_emitter =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let t =
      { clocking
      ; scope
      ; cpu = Cpu.O.Of_signal.wires ()
      ; open_handler = { to_bus = handler_to_emitter; from_bus = emitter_to_handler }
      ; open_emitter = { to_bus = emitter_to_handler; from_bus = handler_to_emitter }
      }
    in
    (* Attach CPU to an emitter port. *)
    let cpu_port = add_access_emitter t in
    let cpu =
      Cpu.hierarchical
        ~scope
        { clocking; request_interrupt = Signal.gnd; from_mem = cpu_port.from_bus }
    in
    Memory.Bus.To_mem.Of_signal.assign cpu_port.to_bus cpu.to_mem;
    Cpu.O.Of_signal.assign t.cpu cpu;
    t
  ;;

  (** Attach a BRAM-backed main memory handling addresses in the internal [0, size_bytes). *)
  let attach_bram_memory ~size_bytes t =
    let module M =
      Memory.Main_memory_bram.Make (struct
        let bytes_per_word = Memory.Bus.cpu_bus_width / 8
        let size = Int.round_up ~to_multiple_of:bytes_per_word size_bytes / bytes_per_word
      end)
    in
    let port = add_access_handler ~addr_pred:(fun a -> Signal.(a <:. size_bytes)) t in
    let mem =
      M.hierarchical ~scope:t.scope { clocking = t.clocking; from_cpu = port.from_bus }
    in
    Memory.Bus.From_mem.Of_signal.assign port.to_bus mem.to_cpu
  ;;

  (** Complete the system, preventing new emitter/handler connections and closing open
      wires. *)
  let complete { open_emitter; open_handler; _ } =
    Memory.Bus.To_mem.Of_signal.(assign open_emitter.to_bus (zero ()));
    Memory.Bus.From_mem.Of_signal.(assign open_handler.to_bus (zero ()))
  ;;
end
