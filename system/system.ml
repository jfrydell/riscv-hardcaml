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

(** Represents a bi-directional transformation of addresses, allowing memories to see
    different addresses to those they are mapped to by the CPU. *)
module Address_transform = struct
  type t =
    { cpu_to_mem : Signal.t -> Signal.t
    ; mem_to_cpu : Signal.t -> Signal.t
    }

  let identity = { cpu_to_mem = Fn.id; mem_to_cpu = Fn.id }

  (** Transform one contiguous range of a given size into another. If alignment permits,
      just masks bits (assuming all transformed addresses are within [size_bytes] of their
      respective start). *)
  let move_range ~cpu_start ~mem_start ~size_bytes : t =
    let open Signal in
    let common_bits =
      Int.min Memory.Bus.addr_width (Int.ctz (cpu_start lxor mem_start))
    in
    let window = Int.pow 2 common_bits in
    let use_upper_bits =
      size_bytes <= window && (cpu_start land (window - 1)) + size_bytes <= window
    in
    if Int.equal cpu_start mem_start
    then { cpu_to_mem = Fn.id; mem_to_cpu = Fn.id }
    else if use_upper_bits
    then (
      let replace upper address =
        let upper = of_unsigned_int ~width:(Memory.Bus.addr_width - common_bits) upper in
        if common_bits = 0
        then upper
        else concat_msb [ upper; sel_bottom ~width:common_bits address ]
      in
      { cpu_to_mem = replace (mem_start lsr common_bits)
      ; mem_to_cpu = replace (cpu_start lsr common_bits)
      })
    else (
      let offset value address =
        let value = of_unsigned_int ~width:Memory.Bus.addr_width value in
        address +: value
      in
      { cpu_to_mem = offset (mem_start - cpu_start)
      ; mem_to_cpu = offset (cpu_start - mem_start)
      })
  ;;
end

module Make (Config : Config) = struct
  module Cpu = Cpu.Make (Config.Cpu)

  type 'a t =
    { scope : Scope.t
    ; clocking : 'a Types.Clocking.t (** Clock and reset, assigned at creation. *)
    ; cpu : 'a Cpu.O.t (** CPU outputs, assigned at creation. *)
    ; request_interrupt : 'a
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
    ?(address_transform = Address_transform.identity)
    ~addr_pred
    ({ scope; clocking; open_handler = prev_open_handler; _ } as t)
    =
    let%hw.Access_handling_port.Of_signal open_handler =
      Access_handling_port.Of_signal.wires ()
    in
    let%hw.Access_handling_port.Of_signal connected_handler =
      Access_handling_port.Of_signal.wires ()
    in
    let%hw.Memory.Bus.From_mem.Of_signal connected_to_bus_transformed =
      { connected_handler.to_bus with
        addr = address_transform.mem_to_cpu connected_handler.to_bus.addr
      }
    in
    (* New connected handler only sees addresses in predicate, and new open port gets the remainder. *)
    let%hw.Memory.Bus.Router.O.Of_signal router =
      Memory.Bus.Router.hierarchical
        ~addr_pred
        ~scope
        { clocking
        ; in_req = prev_open_handler.from_bus
        ; in_resp_t = connected_to_bus_transformed
        ; in_resp_f = open_handler.to_bus
        }
    in
    Memory.Bus.From_mem.Of_signal.assign prev_open_handler.to_bus router.out_resp;
    Memory.Bus.To_mem.Of_signal.assign
      connected_handler.from_bus
      { router.out_req_t with addr = address_transform.cpu_to_mem router.out_req_t.addr };
    Memory.Bus.To_mem.Of_signal.assign open_handler.from_bus router.out_req_f;
    t.open_handler <- open_handler;
    connected_handler
  ;;

  (** Get clocking info. *)
  let clocking { clocking; _ } = clocking

  (** Get CPU outputs. *)
  let cpu { cpu; _ } = cpu

  (** Get interrupt input wire, initially unassigned. *)
  let interrupt { request_interrupt; _ } = request_interrupt

  let create ?initial_pc ~scope ~clocking () =
    (* Start with an I/O bus unattached to anything. Each of these wires acts as the already-connected [from_bus] from one side's open port, and the unassigned wire [to_bus] for the other. *)
    let%hw.Memory.Bus.To_mem.Of_signal emitter_to_handler =
      Memory.Bus.To_mem.Of_signal.wires ()
    in
    let%hw.Memory.Bus.From_mem.Of_signal handler_to_emitter =
      Memory.Bus.From_mem.Of_signal.wires ()
    in
    let%hw request_interrupt = Signal.wire 1 in
    let t =
      { clocking
      ; scope
      ; cpu = Cpu.O.Of_signal.wires ()
      ; request_interrupt
      ; open_handler = { to_bus = handler_to_emitter; from_bus = emitter_to_handler }
      ; open_emitter = { to_bus = emitter_to_handler; from_bus = handler_to_emitter }
      }
    in
    (* Attach CPU to an emitter port. *)
    let cpu_port = add_access_emitter t in
    let cpu =
      Cpu.hierarchical
        ~scope
        ?initial_pc
        { clocking; request_interrupt; from_mem = cpu_port.from_bus }
    in
    Memory.Bus.To_mem.Of_signal.assign cpu_port.to_bus cpu.to_mem;
    Cpu.O.Of_signal.assign t.cpu cpu;
    t
  ;;

  (** Attach a BRAM-backed main memory handling addresses in the interval [start_addr, start_addr + size_bytes). *)
  let attach_bram_memory ?(start_addr = 0) ~size_bytes t =
    let module M =
      Memory.Main_memory_bram.Make (struct
        let capacity = size_bytes
      end)
    in
    let port =
      add_access_handler
        ~address_transform:
          (Address_transform.move_range ~cpu_start:start_addr ~mem_start:0 ~size_bytes)
        ~addr_pred:(fun a ->
          Signal.(a >=:. start_addr &&: (a <:. start_addr + size_bytes)))
        t
    in
    let mem =
      M.hierarchical ~scope:t.scope { clocking = t.clocking; from_cpu = port.from_bus }
    in
    Memory.Bus.From_mem.Of_signal.assign port.to_bus mem.to_cpu
  ;;

  (** Attach a 32-bit read/write MMIO register at [addr]. *)
  let attach_mmio_register ~addr ({ scope; clocking; _ } as t) =
    let handler_port = add_access_handler ~addr_pred:(fun a -> Signal.(a ==:. addr)) t in
    let%hw read_value = Signal.wire 32 in
    let register =
      Mmio_register.hierarchical
        ~scope
        { clocking; from_bus = handler_port.from_bus; read_value }
    in
    Memory.Bus.From_mem.Of_signal.assign handler_port.to_bus register.to_bus;
    let%hw.Mmio_register.Port.Of_signal port =
      { read_value; write = register.write; read = register.read }
    in
    port
  ;;

  (** Complete the system, preventing new emitter/handler connections and closing open
      wires. *)
  let complete { open_emitter; open_handler; _ } =
    Memory.Bus.To_mem.Of_signal.(assign open_emitter.to_bus (zero ()));
    Memory.Bus.From_mem.Of_signal.(assign open_handler.to_bus (zero ()))
  ;;
end
