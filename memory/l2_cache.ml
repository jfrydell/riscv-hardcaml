open! Core
open! Hardcaml
open Signal

include Cache_common.Make (struct
    (* TODO: bigger / configurable L2 *)
    let num_sets = 512
  end)

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
      ; data_in : 'a [@bits data_width]
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
    ({ read_addr
     ; to_mem =
         (* All these outputs go one cycle after read. *)
         { valid = dirty_in &: active
         ; uncacheable = gnd
         ; access_type = Memory_bus.Access_type.write_back
         ; addr = Types.Clocking.reg clocking read_addr
         ; data = data_in
         ; size = zero 2
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

let create scope ({ clocking; from_l1; from_mem } : _ I.t) =
  let%hw access_done = wire 1 in
  let incoming_access =
    { Active_access.valid = from_l1.valid
    ; addr = from_l1.addr
    ; uncacheable = from_l1.valid &&: from_l1.uncacheable
    ; read_word = from_l1.valid &&: from_l1.access_type.read_word
    ; read_block = from_l1.valid &&: from_l1.access_type.read_block
    ; write_through = from_l1.valid &&: from_l1.access_type.write_through
    ; write_back = gnd
    ; store_data = sel_bottom ~width:data_width from_l1.data
    ; store_size = from_l1.size
    ; tag = extract_tag from_l1.addr
    ; index = extract_set_index from_l1.addr
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
  let%hw cache_request =
    active_valid
    &&: ~:(active_access.uncacheable)
    &&: (active_access.read_block ||: active_access.write_through)
  in
  let%hw uncacheable_read =
    active_valid &&: active_access.uncacheable &&: active_access.read_word
  in
  let%hw uncacheable_write =
    active_valid &&: active_access.uncacheable &&: active_access.write_through
  in
  let%hw request_valid = cache_request ||: uncacheable_read ||: uncacheable_write in
  let%hw unsupported_access = active_valid &&: ~:request_valid in
  (* If another block is there, do writeback; if/when empty, fill from mem; when done filling, stream block out or perform store. *)
  let%hw writing_back = cache_request &&: read_metadata.valid &&: ~:tag_match in
  let%hw filling = cache_request &&: ~:(read_metadata.valid) in
  let%hw load_hit = cache_request &&: active_access.read_block &&: tag_match in
  let%hw store_hit = cache_request &&: active_access.write_through &&: tag_match in
  let%hw read_word_done = uncacheable_read &&: from_mem.valid &&: from_mem.last in
  (* Loaded word and dirty bit for the current access. Data memory is accessed
     beginning the cycle after the tag memory, with the accessed address always
     coming from [active_acess] (not [next_access]). So this data isn't valid
     until a cycle after [load_hit], [store_hit], or [writing_back] triggers
     the data read. *)
  let%hw loaded_word = wire data_width in
  let%hw loaded_dirty = wire 1 in
  (* Write back any dirty words of valid but not matching block. *)
  let%hw eviction_base_addr =
    read_metadata.tag @: active_access.index @: zero bits_byte_in_block
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
  let%hw fill_done = filling &&: from_mem.valid &&: from_mem.last in
  (* Stream block to L1 on load hit. *)
  let read_stream =
    Read_stream.hierarchical
      ~scope
      { clocking
      ; start = load_hit &&: ~:access_done
      ; base_addr = block_base_addr active_access.addr
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
  let%hw uncacheable_write_done = uncacheable_write &&: from_mem.ready in
  access_done
  <-- (store_hit
       ||: read_stream.to_l1.last
       ||: unsupported_access
       ||: read_word_done
       ||: uncacheable_write_done);
  (* Write data for cache line fill or store. *)
  let%hw filling_valid = filling &&: from_mem.valid in
  let%hw data_write_enable = filling_valid ||: writing_store.valid in
  let%hw data_write_addr = mux2 filling_valid from_mem.addr writing_store.addr in
  let%hw data_write_value = mux2 filling_valid from_mem.data updated_word in
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
           ; read_address = extract_set_index next_access.addr
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
           ; write_address = extract_word_index data_write_addr
           ; write_data = data_write_value
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word_index data_read_addr
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
           ; write_address = extract_word_index data_write_addr
           ; write_data = writing_store.valid
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word_index data_read_addr
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
    ; uncacheable = gnd
    ; access_type = Memory_bus.Access_type.read_block
    ; addr = active_access.addr
    ; data = zero data_width
    ; size = zero 2
    ; last = gnd
    }
  in
  let%hw.Memory_bus.To_mem.Of_signal uncacheable_to_mem =
    { valid = uncacheable_read &&: ~:read_word_done ||: uncacheable_write
    ; uncacheable = vdd
    ; access_type =
        Memory_bus.Access_type.Of_signal.mux2
          active_access.read_word
          Memory_bus.Access_type.read_word
          Memory_bus.Access_type.write_through
    ; addr = active_access.addr
    ; data = active_access.store_data
    ; size = active_access.store_size
    ; last = vdd
    }
  in
  let%hw.Memory_bus.To_mem.Of_signal to_mem =
    Memory_bus.To_mem.Of_signal.mux2
      writing_back
      writeback.to_mem
      (Memory_bus.To_mem.Of_signal.mux2
         (uncacheable_read ||: uncacheable_write)
         uncacheable_to_mem
         read_to_mem)
  in
  let%hw.Memory_bus.From_mem.Of_signal to_l1 =
    { valid = read_stream.to_l1.valid ||: read_word_done
    ; addr = mux2 read_word_done from_mem.addr read_stream.to_l1.addr
    ; data = mux2 read_word_done from_mem.data read_stream.to_l1.data
    ; last = mux2 read_word_done from_mem.last read_stream.to_l1.last
    ; ready = take_incoming
    }
  in
  ({ to_l1; to_mem } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
