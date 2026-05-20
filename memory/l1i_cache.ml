open! Core
open! Hardcaml
open Signal

let addr_width = Iface.addr_width
let bus_width = Iface.cpu_bus_width
let block_size_bits = Iface.block_size_bits

(* Match the current L1 D-cache geometry. *)
let num_sets = 512
let bits_block_offset = address_bits_for (block_size_bits / 8)
let bits_index = address_bits_for num_sets
let bits_tag = addr_width - bits_block_offset - bits_index
let num_words = num_sets * (block_size_bits / bus_width)
let bits_word_index = address_bits_for num_words
let bits_word_offset = address_bits_for (bus_width / 8)
let extract_tag addr = sel_top ~width:bits_tag addr

let extract_index addr =
  drop_bottom ~width:bits_block_offset addr |> sel_bottom ~width:bits_index
;;

let extract_word addr =
  drop_bottom ~width:bits_word_offset addr |> sel_bottom ~width:bits_word_index
;;

let insn_from_word ~word ~word_offset =
  log_shift ~f:srl ~by:(word_offset @: of_bit_string "000") word |> sel_bottom ~width:32
;;

module From_pipe = struct
  type 'a t = { pc : 'a [@bits addr_width] } [@@deriving hardcaml]
end

module To_pipe = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; pc : 'a [@bits addr_width] (** Address of the instruction that was just fetched. *)
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; read_from_mem : 'a Iface.Read_block.From_mem.t
    ; from_pipeline : 'a From_pipe.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { read_to_mem : 'a Iface.Read_block.To_mem.t
    ; to_pipeline : 'a To_pipe.t
    }
  [@@deriving hardcaml]
end

module Metadata = struct
  type 'a t =
    { valid : 'a
    ; tag : 'a [@bits bits_tag]
    }
  [@@deriving hardcaml]
end

let create scope ({ clocking; from_pipeline = { pc }; read_from_mem } : _ I.t) =
  let%hw miss = wire 1 in
  let%hw active_pc = wire addr_width in
  let%hw next_pc = mux2 miss active_pc pc in
  active_pc <-- Types.Clocking.reg clocking next_pc;
  let%hw active_tag = extract_tag active_pc in
  let%hw update_tag = wire 1 in
  let%hw.Metadata.Of_signal read_metadata =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_sets
        ~write_ports:
          [| { write_clock = clocking.clock
             ; write_enable = update_tag
             ; write_address = extract_index active_pc
             ; write_data = Metadata.Of_signal.pack { tag = active_tag; valid = vdd }
             }
          |]
        ~read_ports:
          [| { read_clock = clocking.clock
             ; read_enable = vdd
             ; read_address = extract_index next_pc
             }
          |]
        ~name:"tags"
        ()
    in
    Metadata.Of_signal.unpack mem.(0)
  in
  let%hw tag_match = read_metadata.valid &&: (active_tag ==: read_metadata.tag) in
  miss <-- ~:tag_match;
  let data_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = read_from_mem.valid
           ; write_address = extract_word read_from_mem.addr
           ; write_data = read_from_mem.data
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word next_pc
           }
        |]
      ~name:"data"
      ()
  in
  let%hw loaded_word = data_mem.(0) in
  let%hw word_offset = sel_bottom ~width:bits_word_offset active_pc in
  let%hw insn_value = insn_from_word ~word:loaded_word ~word_offset in
  update_tag <-- (miss &&: read_from_mem.valid &&: read_from_mem.last);
  ({ read_to_mem = { addr = active_pc; load = miss }
   ; to_pipeline = { insn = insn_value; pc = active_pc; valid = tag_match }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
