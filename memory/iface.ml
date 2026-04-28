open! Core
open! Hardcaml

let addr_width = 32
let cpu_bus_width = 64
let block_size_bits = 256

(** Writes performed at a cache/memory. *)
module Write_through = struct
  module To_mem = struct
    type 'a t =
      { addr : 'a [@bits addr_width]
      ; store : 'a
      ; store_data : 'a [@bits addr_width]
      ; store_size : 'a [@bits 2]
      }
    [@@deriving hardcaml]
  end

  module From_mem = struct
    type 'a t = { store_ready : 'a } [@@deriving hardcaml]
  end
end

(** Read a block from a cache/memory. *)
module Read_block = struct
  module To_mem = struct
    type 'a t =
      { addr : 'a [@bits addr_width] (** An address within the block to read. *)
      ; load : 'a
      }
    [@@deriving hardcaml]
  end

  module From_mem = struct
    type 'a t =
      { data : 'a [@bits cpu_bus_width]
      ; addr : 'a [@bits addr_width]
      ; valid : 'a
      ; last : 'a
      }
    [@@deriving hardcaml]
  end
end
