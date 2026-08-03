open! Core
open! Hardcaml
open Signal

module type Config = sig
  val num_sets : int
end

module Make (Config : Config) = struct
  let num_sets = Config.num_sets
  let addr_width = Memory_bus.addr_width
  let data_width = Memory_bus.data_width

  let block_size_bits = Memory_bus.block_size_bits
  let bits_byte_in_block = address_bits_for (block_size_bits / 8)
  let words_per_block = block_size_bits / data_width
  let bits_set_index = address_bits_for num_sets
  let bits_tag = addr_width - bits_byte_in_block - bits_set_index
  let num_words = num_sets * words_per_block
  let bits_word_index = address_bits_for num_words
  let bits_byte_in_word = address_bits_for (data_width / 8)
  let bits_word_in_block = address_bits_for words_per_block
  let extract_tag addr = sel_top ~width:bits_tag addr

  let extract_set_index addr =
    drop_bottom ~width:bits_byte_in_block addr |> sel_bottom ~width:bits_set_index
  ;;

  let extract_word_index addr =
    drop_bottom ~width:bits_byte_in_word addr |> sel_bottom ~width:bits_word_index
  ;;

  let block_base_addr addr =
    drop_bottom ~width:bits_byte_in_block addr @: zero bits_byte_in_block
  ;;

  let word_offset_addr word =
    uresize ~width:(addr_width - bits_byte_in_word) word @: zero bits_byte_in_word
  ;;

  module Active_access = struct
    type 'a t =
      { valid : 'a
      ; addr : 'a [@bits addr_width]
      ; uncacheable : 'a
      ; read_word : 'a
      ; read_block : 'a
      ; write_through : 'a
      ; write_back : 'a
      ; store_data : 'a [@bits data_width]
      ; store_size : 'a [@bits 2]
      ; tag : 'a [@bits bits_tag]
      ; index : 'a [@bits bits_set_index]
      }
    [@@deriving hardcaml]
  end

  (** Info for a store to write to memory, tracked separately to occur one cycle after the
      [Active_access]. *)
  module Writing_store = struct
    module Byte_valid = With_valid.Vector (struct
        let width = 8
      end)

    type 'a t =
      { addr : 'a [@bits addr_width]
      ; bytes : 'a Byte_valid.t list [@length data_width / 8]
      ; valid : 'a (** Signals that the write should be performed to memory. *)
      }
    [@@deriving hardcaml]

    let of_active ~valid (active : _ Active_access.t) =
      let word_offset =
        sel_bottom ~width:(address_bits_for (data_width / 8)) active.addr
      in
      let rep_data ~width =
        List.init (data_width / width) ~f:(fun _ -> sel_bottom ~width active.store_data)
        |> concat_lsb
      in
      let data =
        mux
          active.store_size
          [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
        |> split_lsb ~part_width:8
      in
      let valids =
        [ "0001"; "0011"; "1111" ]
        |> List.map ~f:of_bit_string
        |> mux active.store_size
        |> uresize ~width:(data_width / 8)
        |> log_shift ~f:sll ~by:word_offset
        |> split_lsb ~part_width:1
      in
      { addr = active.addr
      ; bytes =
          List.map2_exn data valids ~f:(fun value valid -> { With_valid.value; valid })
      ; valid
      }
    ;;
  end

  (** Controls reading data from BRAM and streaming it back to L1 for block-granularity
      reads. TODO: word-granularity reads (take in addr, not base_addr) *)
  module Read_stream = struct
    module I = struct
      type 'a t =
        { clocking : 'a Types.Clocking.t
        ; start : 'a (** Start streaming data out from [base_addr]. *)
        ; base_addr : 'a [@bits addr_width]
        ; data_in : 'a [@bits data_width]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { read_addr : 'a [@bits addr_width]
        ; to_l1 : 'a Memory_bus.From_mem.t
        }
      [@@deriving hardcaml]
    end

    let create scope ({ clocking; start; base_addr; data_in } : _ I.t) =
      let%hw base_addr =
        Types.Clocking.cut_through_reg clocking ~enable:start base_addr
      in
      (* Increment the word number from when [start] is set through the last word in the
         block. *)
      let%hw reading_last = wire 1 in
      let%hw do_read =
        Utils.sr ~set:start ~reset:reading_last ~style:`Mealy_set clocking
      in
      let%hw was_reading = Types.Clocking.reg clocking do_read in
      let%hw start_stream = start &&: ~:was_reading in
      let%hw read_word_number_reg =
        Types.Clocking.reg_fb
          clocking
          ~width:bits_word_in_block
          ~f:(fun w -> mux2 start_stream (zero bits_word_in_block +:. 1) (w +:. 1))
          ~enable:do_read
      in
      let%hw read_word_number =
        mux2 start_stream (zero bits_word_in_block) read_word_number_reg
      in
      reading_last <-- (read_word_number ==: ones bits_word_in_block);
      let%hw read_addr =
        base_addr
        +: (uresize ~width:(addr_width - bits_byte_in_word) read_word_number
            @: zero bits_byte_in_word)
      in
      let%hw valid = Types.Clocking.reg clocking do_read in
      let%hw last = Types.Clocking.reg clocking reading_last in
      ({ read_addr
       ; to_l1 =
           { valid
           ; addr = Types.Clocking.reg clocking read_addr
           ; data = data_in
           ; last
           ; ready = last
           }
       }
       : _ O.t)
    ;;

    let hierarchical =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~name:"read_stream" create
    ;;
  end
end
