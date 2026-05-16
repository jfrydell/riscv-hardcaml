open! Core
open! Hardcaml
open Signal

let addr_width = Iface.addr_width
let bus_width = Iface.cpu_bus_width
let block_size_bits = Iface.block_size_bits

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

let block_base_word index = index @: zero bits_word_in_block

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
    ; word : 'a [@bits bits_word_index]
    ; base_addr : 'a [@bits addr_width]
    ; base_word : 'a [@bits bits_word_index]
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; write_from_l1 : 'a Iface.Write_through.To_mem.t
    ; read_from_l1 : 'a Iface.Read_block.To_mem.t
    ; write_from_mem : 'a Iface.Write_back.From_mem.t
    ; read_from_mem : 'a Iface.Read_block.From_mem.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { write_to_l1 : 'a Iface.Write_through.From_mem.t
    ; read_to_l1 : 'a Iface.Read_block.From_mem.t
    ; write_to_mem : 'a Iface.Write_back.To_mem.t
    ; read_to_mem : 'a Iface.Read_block.To_mem.t
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
  module O = struct
    type 'a t =
      { read_addr : 'a [@bits bits_word_index]
      ; to_mem : 'a Iface.Write_back.To_mem.t
      ; done_ : 'a
      }
    [@@deriving hardcaml]
  end

  let create
    scope
    ~(clocking : _ Types.Clocking.t)
    ~active
    ~base_addr
    ~base_word
    ~data_in
    ~dirty_in
    ~ready
    =
    let%hw active_d = Types.Clocking.reg clocking active in
    let%hw first_cycle = active &: ~:active_d in
    let%hw awaiting = wire 1 in
    let%hw pending_valid = wire 1 in
    let%hw pending_dirty = wire 1 in
    let%hw pending_data = wire bus_width in
    let%hw pending_addr = wire addr_width in
    let%hw word = wire bits_word_in_block in
    let%hw issue_read = active &: ~:awaiting &: ~:pending_valid in
    let%hw got_response = awaiting in
    let%hw write_beat = pending_valid &: pending_dirty in
    let%hw consume_pending = pending_valid &: (~:pending_dirty ||: ready) in
    let%hw last_word = word ==:. words_per_block - 1 in
    let%hw done_ = active &: consume_pending &: last_word in
    awaiting
    <-- Types.Clocking.reg
          clocking
          (active &: (issue_read ||: first_cycle &: ~:got_response &: ~:done_));
    pending_valid
    <-- Types.Clocking.reg
          clocking
          (active &: (got_response ||: (pending_valid &: ~:consume_pending) &: ~:done_));
    pending_dirty
    <-- Types.Clocking.reg
          clocking
          (mux2 active (mux2 got_response dirty_in pending_dirty) gnd);
    pending_data
    <-- Types.Clocking.reg
          clocking
          (mux2 active (mux2 got_response data_in pending_data) (zero bus_width));
    pending_addr
    <-- Types.Clocking.reg
          clocking
          (mux2
             active
             (mux2 got_response (base_addr +: word_offset_addr word) pending_addr)
             (zero addr_width));
    word
    <-- Types.Clocking.reg
          clocking
          (mux2
             active
             (mux2
                first_cycle
                (zero bits_word_in_block)
                (mux2 consume_pending (mux2 last_word word (word +:. 1)) word))
             (zero bits_word_in_block));
    ({ read_addr = base_word +: uresize ~width:bits_word_index word
     ; to_mem =
         { data = pending_data
         ; addr = pending_addr
         ; write = write_beat
         ; last = pending_valid &: last_word
         }
     ; done_
     }
     : _ O.t)
  ;;
end

module Read_stream = struct
  module O = struct
    type 'a t =
      { read_addr : 'a [@bits bits_word_index]
      ; to_l1 : 'a Iface.Read_block.From_mem.t
      ; done_ : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope ~(clocking : _ Types.Clocking.t) ~active ~base_addr ~base_word ~data_in
    =
    let%hw active_d = Types.Clocking.reg clocking active in
    let%hw first_cycle = active &: ~:active_d in
    let%hw awaiting = wire 1 in
    let%hw pending_valid = wire 1 in
    let%hw pending_data = wire bus_width in
    let%hw pending_addr = wire addr_width in
    let%hw word = wire bits_word_in_block in
    let%hw issue_read = active &: ~:awaiting &: ~:pending_valid in
    let%hw got_response = awaiting in
    let%hw last_word = word ==:. words_per_block - 1 in
    let%hw done_ = active &: pending_valid &: last_word in
    awaiting
    <-- Types.Clocking.reg
          clocking
          (active &: (issue_read ||: first_cycle &: ~:got_response &: ~:done_));
    pending_valid <-- Types.Clocking.reg clocking (active &: got_response &: ~:done_);
    pending_data
    <-- Types.Clocking.reg
          clocking
          (mux2 active (mux2 got_response data_in pending_data) (zero bus_width));
    pending_addr
    <-- Types.Clocking.reg
          clocking
          (mux2
             active
             (mux2 got_response (base_addr +: word_offset_addr word) pending_addr)
             (zero addr_width));
    word
    <-- Types.Clocking.reg
          clocking
          (mux2
             active
             (mux2
                first_cycle
                (zero bits_word_in_block)
                (mux2 pending_valid (mux2 last_word word (word +:. 1)) word))
             (zero bits_word_in_block));
    ({ read_addr = base_word +: uresize ~width:bits_word_index word
     ; to_l1 =
         { data = pending_data
         ; addr = pending_addr
         ; valid = pending_valid
         ; last = pending_valid &: last_word
         }
     ; done_
     }
     : _ O.t)
  ;;
end

let create
  scope
  ({ clocking; write_from_l1; read_from_l1; write_from_mem; read_from_mem } : _ I.t)
  =
  let%hw access_done = wire 1 in
  let read_req = read_from_l1.load in
  let write_req = write_from_l1.store in
  let addr = mux2 read_req read_from_l1.addr write_from_l1.addr in
  let valid = read_req ||: write_req in
  let incoming_access =
    { Active_access.valid
    ; addr
    ; load = read_req
    ; store = ~:read_req &: write_req
    ; store_data = write_from_l1.store_data
    ; store_size = write_from_l1.store_size
    ; tag = extract_tag addr
    ; index = extract_index addr
    ; word = extract_word addr
    ; base_addr = block_base_addr addr
    ; base_word = block_base_word (extract_index addr)
    }
  in
  let%hw.Active_access.Of_signal active_access = Active_access.Of_signal.wires () in
  let active_valid = active_access.valid in
  let%hw take_incoming = ~:active_valid ||: access_done in
  let%hw.Active_access.Of_signal next_access =
    Active_access.Of_signal.mux2 take_incoming incoming_access active_access
  in
  Active_access.Of_signal.assign
    active_access
    (Active_access.map next_access ~f:(Types.Clocking.reg clocking));
  let%hw tag_write_enable = wire 1 in
  let%hw tag_write_valid = wire 1 in
  let tag_write_data =
    Metadata.Of_signal.pack { valid = tag_write_valid; tag = active_access.tag }
  in
  let%hw.Metadata.Of_signal read_metadata =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_sets
        ~write_ports:
          [| { write_clock = clocking.clock
             ; write_enable = tag_write_enable
             ; write_address = active_access.index
             ; write_data = tag_write_data
             }
          |]
        ~read_ports:
          [| { read_clock = clocking.clock
             ; read_enable = vdd
             ; read_address = extract_index next_access.addr
             }
          |]
        ~name:"tags"
        ()
    in
    Metadata.Of_signal.unpack mem.(0)
  in
  let%hw tag_match = read_metadata.valid &&: (read_metadata.tag ==: active_access.tag) in
  let metadata_valid = read_metadata.valid in
  let%hw request_valid = active_valid &&: (active_access.load ||: active_access.store) in
  let%hw writing_back = request_valid &&: metadata_valid &&: ~:tag_match in
  let%hw filling = request_valid &&: ~:metadata_valid in
  let%hw stream_active = request_valid &&: active_access.load &&: tag_match in
  let%hw store_hit = request_valid &&: active_access.store &&: tag_match in
  let%hw eviction_base_addr =
    read_metadata.tag @: active_access.index @: zero bits_block_offset
  in
  let%hw eviction_base_word = block_base_word active_access.index in
  let%hw loaded_word = wire bus_width in
  let%hw loaded_dirty = wire 1 in
  let writeback =
    Writeback.create
      scope
      ~clocking
      ~active:writing_back
      ~base_addr:eviction_base_addr
      ~base_word:eviction_base_word
      ~data_in:loaded_word
      ~dirty_in:loaded_dirty
      ~ready:write_from_mem.ready
  in
  let read_stream =
    Read_stream.create
      scope
      ~clocking
      ~active:stream_active
      ~base_addr:active_access.base_addr
      ~base_word:active_access.base_word
      ~data_in:loaded_word
  in
  let writeback_done = writeback.done_ in
  let%hw fill_done = read_from_mem.valid &&: read_from_mem.last in
  access_done <-- (store_hit ||: read_stream.done_);
  let%hw data_read_addr =
    mux2
      writing_back
      writeback.read_addr
      (mux2 stream_active read_stream.read_addr next_access.word)
  in
  let%hw dirty_read_addr = mux2 writing_back writeback.read_addr next_access.word in
  let store_word =
    let original = split_lsb ~part_width:8 ~exact:true loaded_word in
    let overwrite_bytes =
      Iface.Write_through.to_bytes
        { addr = active_access.addr
        ; store = active_access.store
        ; store_data = active_access.store_data
        ; store_size = active_access.store_size
        }
    in
    List.map2_exn overwrite_bytes original ~f:(fun { valid; value } byte ->
      mux2 valid value byte)
    |> concat_lsb
  in
  let%hw data_write_enable = read_from_mem.valid ||: store_hit in
  let%hw data_write_addr =
    mux2 read_from_mem.valid (extract_word read_from_mem.addr) active_access.word
  in
  let%hw data_write_value = mux2 read_from_mem.valid read_from_mem.data store_word in
  let dirty_write_enable = data_write_enable in
  let dirty_write_addr = data_write_addr in
  let dirty_write_value = mux2 read_from_mem.valid gnd vdd in
  tag_write_enable <-- (writeback_done ||: fill_done);
  tag_write_valid <-- ~:writeback_done;
  let data_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = data_write_enable
           ; write_address = data_write_addr
           ; write_data = data_write_value
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = data_read_addr
           }
        |]
      ~name:"data"
      ()
  in
  let dirty_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = dirty_write_enable
           ; write_address = dirty_write_addr
           ; write_data = dirty_write_value
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = dirty_read_addr
           }
        |]
      ~name:"dirty"
      ()
  in
  loaded_word <-- data_mem.(0);
  loaded_dirty <-- dirty_mem.(0);
  ({ write_to_l1 = { store_ready = store_hit }
   ; read_to_l1 = read_stream.to_l1
   ; write_to_mem = writeback.to_mem
   ; read_to_mem = { addr = active_access.addr; load = filling }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
