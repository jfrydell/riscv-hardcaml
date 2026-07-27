open! Core
open! Hardcaml
open Signal

let addr_width = Memory_bus.addr_width
let bus_width = Memory_bus.cpu_bus_width
let block_size_bits = Memory_bus.block_size_bits

(* Match the current L1 geometry until a separate L2 geometry is specified. *)
let num_sets = 512
let words_per_block = block_size_bits / bus_width
let bits_block_offset = address_bits_for (block_size_bits / 8)
let bits_index = address_bits_for num_sets
let bits_tag = addr_width - bits_block_offset - bits_index
let num_words = num_sets * words_per_block
let bits_word_index = address_bits_for num_words
let bits_word_offset = address_bits_for (bus_width / 8)
let bits_word_in_block = address_bits_for words_per_block
let extract_tag addr = sel_top ~width:bits_tag addr

let extract_index addr =
  drop_bottom ~width:bits_block_offset addr |> sel_bottom ~width:bits_index
;;

let extract_word addr =
  drop_bottom ~width:bits_word_offset addr |> sel_bottom ~width:bits_word_index
;;

let block_base_addr addr =
  drop_bottom ~width:bits_block_offset addr @: zero bits_block_offset
;;

let word_offset_addr word =
  uresize ~width:(addr_width - bits_word_offset) word @: zero bits_word_offset
;;

module Active_access = struct
  type 'a t =
    { valid : 'a
    ; addr : 'a [@bits addr_width]
    ; load : 'a
    ; store : 'a
    ; store_data : 'a [@bits addr_width]
    ; store_size : 'a [@bits 2]
    ; tag : 'a [@bits bits_tag]
    ; index : 'a [@bits bits_index]
    ; base_addr : 'a [@bits addr_width]
    }
  [@@deriving hardcaml]
end

(** Info for a store to write to the cache, tracked separately to occur one cycle after it
    is the [Active_access]. *)
module Writing_store = struct
  module Byte_valid = With_valid.Vector (struct
      let width = 8
    end)

  type 'a t =
    { addr : 'a [@bits addr_width]
    ; bytes : 'a Byte_valid.t list [@length bus_width / 8]
    ; valid : 'a (** Signals that the write should be performed to memory. *)
    }
  [@@deriving hardcaml]

  let of_active ~valid (active : _ Active_access.t) =
    let word_offset = sel_bottom ~width:(address_bits_for (bus_width / 8)) active.addr in
    let rep_data ~width =
      List.init (bus_width / width) ~f:(fun _ -> sel_bottom ~width active.store_data)
      |> concat_lsb
    in
    let data =
      mux active.store_size [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
      |> split_lsb ~part_width:8
    in
    let valids =
      [ "0001"; "0011"; "1111" ]
      |> List.map ~f:of_bit_string
      |> mux active.store_size
      |> uresize ~width:(bus_width / 8)
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

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; from_l1 : 'a Memory_bus.To_mem.t
    ; from_mem : 'a Memory_bus.From_mem.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { to_l1 : 'a Memory_bus.From_mem.t
    ; to_mem : 'a Memory_bus.To_mem.t
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

module Writeback = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; start : 'a
      (** Start streaming data out from [base_addr]. This signal is ignored until the last
          beat is output, when [to_mem.last] is asserted. On that cycle, [start] must be
          lowered, unless another access is ready for streaming (impossible without
          pipelining tag lookups). *)
      ; base_addr : 'a [@bits addr_width]
      ; data_in : 'a [@bits bus_width]
      ; dirty_in : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { read_addr : 'a [@bits addr_width]
      ; to_mem : 'a Memory_bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; start; base_addr; data_in; dirty_in; ready } : _ I.t) =
    let%hw outputting_last = wire 1 in
    let%hw word_done = ready ||: ~:dirty_in in
    let%hw active = Utils.sr ~set:start ~reset:(outputting_last &&: word_done) clocking in
    (* Increment on the cycle that we are [ready] to account for one-cycle delay to get data. *)
    let%hw read_word_number = wire bits_word_in_block in
    let%hw prev_read_word_number = Types.Clocking.reg clocking read_word_number in
    read_word_number
    <-- mux2 (active &&: word_done) (prev_read_word_number +:. 1) prev_read_word_number;
    outputting_last <-- (prev_read_word_number ==: ones bits_word_in_block);
    let%hw read_addr = base_addr +: word_offset_addr read_word_number in
    ({ read_addr = base_addr +: word_offset_addr read_word_number
     ; to_mem =
         (* All these outputs go one cycle after read. *)
         { valid = dirty_in &: active
         ; access_type = Memory_bus.Access_type.write_back
         ; addr = Types.Clocking.reg clocking read_addr
         ; data = data_in
         ; store_size = zero 2
         ; last = active &&: outputting_last
         }
     }
     : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"writeback" create
  ;;
end

module Read_stream = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; start : 'a
      (** Start streaming data out from [base_addr]. This signal is ignored until the last
          beat is output, when [done_] is asserted. On that cycle, [start] must be
          lowered, unless another access is ready for streaming (impossible without
          pipelining tag lookups). *)
      ; base_addr : 'a [@bits addr_width]
      ; data_in : 'a [@bits bus_width]
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
    let%hw base_addr = Types.Clocking.cut_through_reg clocking ~enable:start base_addr in
    (* Increment word in block from when [start] is set through last word in
       block. The cycle after [reading_last] will be when we are [done_], and
       [start] asserted then will keep [do_read] high constantly. *)
    let%hw reading_last = wire 1 in
    let%hw do_read = Utils.sr ~set:start ~reset:reading_last ~style:`Mealy_set clocking in
    let%hw read_word_number =
      Types.Clocking.reg_fb
        clocking
        ~width:bits_word_in_block
        ~f:(fun w -> w +:. 1)
        ~enable:do_read
    in
    reading_last <-- (read_word_number ==: ones bits_word_in_block);
    let%hw read_addr = base_addr +: word_offset_addr read_word_number in
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

let create scope ({ clocking; from_l1; from_mem } : _ I.t) =
  let%hw access_done = wire 1 in
  let read_req = from_l1.valid &&: from_l1.access_type.read_block in
  let write_req = from_l1.valid &&: from_l1.access_type.write_through in
  let addr = from_l1.addr in
  let incoming_access =
    { Active_access.valid = from_l1.valid
    ; addr
    ; load = read_req
    ; store = ~:read_req &: write_req
    ; store_data = sel_bottom ~width:addr_width from_l1.data
    ; store_size = from_l1.store_size
    ; tag = extract_tag addr
    ; index = extract_index addr
    ; base_addr = block_base_addr addr
    }
  in
  let%hw.Active_access.Of_signal active_access = Active_access.Of_signal.wires () in
  let active_valid = active_access.valid in
  (* We raise [ready] on [access_done], so we should take new data then. *)
  let%hw take_incoming = ~:active_valid ||: access_done in
  let%hw.Active_access.Of_signal next_access =
    Active_access.Of_signal.mux2 take_incoming incoming_access active_access
  in
  Active_access.Of_signal.assign
    active_access
    (Active_access.map next_access ~f:(Types.Clocking.reg clocking));
  let%hw.Metadata.Of_signal read_metadata = Metadata.Of_signal.wires () in
  let%hw tag_match = read_metadata.valid &&: (read_metadata.tag ==: active_access.tag) in
  let%hw request_valid = active_valid &&: (active_access.load ||: active_access.store) in
  let%hw unsupported_access =
    active_valid &&: ~:(active_access.load) &&: ~:(active_access.store)
  in
  (* If another block is there, do writeback; if/when empty, fill from mem; when done filling, stream block out or perform store. *)
  let%hw writing_back = request_valid &&: read_metadata.valid &&: ~:tag_match in
  let%hw filling = request_valid &&: ~:(read_metadata.valid) in
  let%hw load_hit = request_valid &&: active_access.load &&: tag_match in
  let%hw store_hit = request_valid &&: active_access.store &&: tag_match in
  (* Loaded word and dirty bit for the current access. Data memory is accessed
     beginning the cycle after the tag memory, with the accessed address always
     coming from [active_acess] (not [next_access]). So this data isn't valid
     until a cycle after [load_hit], [store_hit], or [writing_back] triggers
     the data read. *)
  let%hw loaded_word = wire bus_width in
  let%hw loaded_dirty = wire 1 in
  (* Write back any dirty words of valid but not matching block. *)
  let%hw eviction_base_addr =
    read_metadata.tag @: active_access.index @: zero bits_block_offset
  in
  let%hw writeback_done = wire 1 in
  let writeback =
    Writeback.hierarchical
      ~scope
      { clocking
      ; start = writing_back &&: ~:writeback_done
      ; base_addr = eviction_base_addr
      ; data_in = loaded_word
      ; dirty_in = loaded_dirty
      ; ready = from_mem.ready
      }
  in
  writeback_done <-- (writeback.to_mem.last &&: (from_mem.ready ||: ~:loaded_dirty));
  (* Fill just forwards data from [from_mem] to data mem. *)
  let%hw fill_done = from_mem.valid &&: from_mem.last in
  (* Stream block to L1 on load hit. *)
  let read_stream =
    Read_stream.hierarchical
      ~scope
      { clocking
      ; start = load_hit &&: ~:access_done
      ; base_addr = active_access.base_addr
      ; data_in = loaded_word
      }
  in
  (* For stores, once the block is in the cache, we first read the word, then overwrite with updated data/dirty. We raise [access_done] as we read the word, though, so that we can perform stores at line rate if they all hit. Effectively, we have a 3-stage pipeline: read tag (based on incoming access), read data (or stall for miss), then write updated data. Note that we can't have a write conflict, as we never write data (including to fill on a miss) the first cycle we have an [active_access].

       Because we accept a new access during the read portion of the update, we must register the store data update. *)
  let%hw.Writing_store.Of_signal writing_store =
    Writing_store.(
      of_active active_access ~valid:store_hit |> map ~f:(Types.Clocking.reg clocking))
  in
  let%hw updated_word =
    split_lsb ~part_width:8 ~exact:true loaded_word
    |> List.map2_exn writing_store.bytes ~f:(fun { valid; value } byte ->
      mux2 valid value byte)
    |> concat_lsb
  in
  (* Read data based on status of active access. *)
  let%hw data_read_addr =
    mux2 writing_back writeback.read_addr
    @@ mux2 load_hit read_stream.read_addr
    @@ (* Store hit *)
    active_access.addr
  in
  access_done <-- (store_hit ||: read_stream.to_l1.last ||: unsupported_access);
  (* Write data for cache line fill or store. *)
  let%hw data_write_enable = from_mem.valid ||: writing_store.valid in
  let%hw data_write_addr = mux2 from_mem.valid from_mem.addr writing_store.addr in
  let%hw data_write_value = mux2 from_mem.valid from_mem.data updated_word in
  (* Update tag/valid when we finish bringing in new block or evicting old. *)
  (* TODO: since data mem is Write_before_read (for store followed immediately by load), we're wasting a cycle here not reading data until the cycle after [fill_done], when tag is updated. *)
  (* TODO: i suppose same is true for writeback_done. once mem is accepting last beat, can move on to something else. *)
  let%hw tag_write_enable = writeback_done ||: fill_done in
  let%hw tag_write_valid = ~:writeback_done in
  let tag_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_sets
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = tag_write_enable
           ; write_address = active_access.index
           ; write_data =
               Metadata.Of_signal.pack
                 { valid = tag_write_valid; tag = active_access.tag }
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_index next_access.addr
           }
        |]
      ~name:"l2_tags"
      ()
  in
  Metadata.Of_signal.(assign read_metadata (unpack tag_mem.(0)));
  let data_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = data_write_enable
           ; write_address = extract_word data_write_addr
           ; write_data = data_write_value
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word data_read_addr
           }
        |]
      ~name:"l2_data"
      ()
  in
  let dirty_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = data_write_enable
           ; write_address = extract_word data_write_addr
           ; write_data = writing_store.valid
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word data_read_addr
           }
        |]
      ~name:"l2_dirty"
      ()
  in
  loaded_word <-- data_mem.(0);
  loaded_dirty <-- dirty_mem.(0);
  let%hw.Memory_bus.To_mem.Of_signal read_to_mem =
    { valid =
        filling &&: ~:fill_done
        (* TODO: probably urn off as soon as first byte gets back? or separate ready? *)
    ; access_type = Memory_bus.Access_type.read_block
    ; addr = active_access.addr
    ; data = zero bus_width
    ; store_size = zero 2
    ; last = gnd
    }
  in
  let%hw.Memory_bus.To_mem.Of_signal to_mem =
    Memory_bus.To_mem.Of_signal.mux2 writing_back writeback.to_mem read_to_mem
  in
  let%hw.Memory_bus.From_mem.Of_signal to_l1 =
    { read_stream.to_l1 with ready = take_incoming }
  in
  ({ to_l1; to_mem } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
