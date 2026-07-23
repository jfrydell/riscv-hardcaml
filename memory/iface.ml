open! Core
open! Hardcaml
open Signal

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

  let to_bytes ({ To_mem.addr; store_data; store_size; _ } : _ To_mem.t) =
    let word_offset = sel_bottom ~width:(address_bits_for (cpu_bus_width / 8)) addr in
    let rep_data ~width =
      List.init (cpu_bus_width / width) ~f:(fun _ -> sel_bottom ~width store_data)
      |> concat_lsb
    in
    let data =
      mux store_size [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
      |> split_lsb ~part_width:8
    in
    let valids =
      [ "0001"; "0011"; "1111" ]
      |> List.map ~f:of_bit_string
      |> mux store_size
      |> uresize ~width:(cpu_bus_width / 8)
      |> log_shift ~f:sll ~by:word_offset
      |> split_lsb ~part_width:1
    in
    List.map2_exn data valids ~f:(fun value valid -> { With_valid.value; valid })
  ;;

  module From_mem = struct
    type 'a t = { store_ready : 'a } [@@deriving hardcaml]
  end
end

(** Write a block back from a cache to memory. *)
module Write_back = struct
  module To_mem = struct
    type 'a t =
      { data : 'a [@bits cpu_bus_width]
      ; addr : 'a [@bits addr_width]
      ; write : 'a
      ; last : 'a
      }
    [@@deriving hardcaml]
  end

  module From_mem = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
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

(** A single 32-bit word read, used by the MMU page-table walker. The definition lives in
    [Mmu.Iface] so the MMU library does not depend back on this library; this alias
    exposes the same interface at the memory boundary. *)
module Read_word = Mmu.Iface.Read_word
(* TODO: put all interfaces in own library probably *)
