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
      ; to_mem : 'a Iface.Write_back.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; start; base_addr; data_in; dirty_in; ready } : _ I.t) =
    (* Increment word in block from when [start] is set through last word in
       block. The cycle after [reading_last] will be when we are [done_], and
       [start] asserted then will keep [do_read] high constantly. *)
    let%hw reading_last = wire 1 in
    let%hw word_done = ready ||: ~:dirty_in in
    let%hw active =
      Utils.sr ~set:start ~reset:(reading_last &&: word_done) ~style:`Mealy_set clocking
    in
    let%hw read_word_number =
      Types.Clocking.reg_fb
        clocking
        ~width:bits_word_in_block
        ~f:(fun w -> w +:. 1)
        ~enable:(active &&: word_done)
    in
    reading_last <-- (read_word_number ==: ones bits_word_in_block);
    let%hw read_addr = base_addr +: word_offset_addr read_word_number in
    ({ read_addr = base_addr +: word_offset_addr read_word_number
     ; to_mem =
         (* All these outputs go one cycle after read. *)
         { data = data_in
         ; addr = Types.Clocking.reg clocking read_addr
         ; write = dirty_in &: Types.Clocking.reg clocking active
         ; last = Types.Clocking.reg clocking (active &: reading_last)
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
      ; to_l1 : 'a Iface.Read_block.From_mem.t
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
    ({ read_addr
     ; to_l1 =
         { data = data_in
         ; addr = Types.Clocking.reg clocking read_addr
         ; valid = Types.Clocking.reg clocking do_read
         ; last = Types.Clocking.reg clocking reading_last
         }
     }
     : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"read_stream" create
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
    ; base_addr = block_base_addr addr
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
  let%hw.Metadata.Of_signal read_metadata = Metadata.Of_signal.wires () in
  let%hw tag_match = read_metadata.valid &&: (read_metadata.tag ==: active_access.tag) in
  let%hw request_valid = active_valid &&: (active_access.load ||: active_access.store) in
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
      ; ready = write_from_mem.ready
      }
  in
  writeback_done <-- (writeback.to_mem.last &&: write_from_mem.ready);
  (* Fill just forwards data from [read_from_mem] to data mem. *)
  let%hw fill_done = read_from_mem.valid &&: read_from_mem.last in
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
  (* Store hit first reads word, then overwrites with updated data/dirty. We
     can have new [active_access] while doing update, though, as updates never
     happen on the first cycle of access (haven't read data from memory yet).
     So we can accept a store every cycle if they all hit. Essentially is a
     3-stage pipeline: tag lookup, then read from data memory (or stall for
     miss), then do update. Because update will always occur the cycle after
     [store_hit], we can accept new access on that cycle, even though data
     won't be written until the end of the following cycle. *)
  let%hw updated_word =
    let original = split_lsb ~part_width:8 ~exact:true loaded_word in
    let overwrite_bytes =
      Iface.Write_through.to_bytes
        { addr = active_access.addr
        ; store = active_access.store
        ; store_data = active_access.store_data
        ; store_size = active_access.store_size
        }
      |> List.map ~f:(With_valid.map ~f:(Types.Clocking.reg clocking))
    in
    List.map2_exn overwrite_bytes original ~f:(fun { valid; value } byte ->
      mux2 valid value byte)
    |> concat_lsb
  in
  let%hw store_write = Types.Clocking.reg clocking store_hit in
  (* Read data based on status of active access. *)
  let%hw data_read_addr =
    mux2 writing_back writeback.read_addr
    @@ mux2 load_hit read_stream.read_addr
    @@ (* Store hit *)
    active_access.addr
  in
  access_done <-- (store_hit ||: read_stream.to_l1.last);
  (* Write data for cahce line fill or store. *)
  let%hw data_write_enable = read_from_mem.valid ||: store_write in
  let%hw data_write_addr =
    mux2
      read_from_mem.valid
      read_from_mem.addr
      (Types.Clocking.reg clocking active_access.addr)
  in
  let%hw data_write_value = mux2 store_write updated_word read_from_mem.data in
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
           ; write_data = store_write
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
  ({ write_to_l1 =
       { (* This && is an ugly hack around the fact that we only have one ready bit, but expose two separate interfaces. Don't want to say we're accepting a store when we're actually accepting a load. *)
         store_ready = take_incoming &&: next_access.store
       }
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
