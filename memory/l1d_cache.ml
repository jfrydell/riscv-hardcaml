(** Memory stage of the CPU, including L1 cache, ready/valid interface to memory, and
    shifting/extending data as necessary. *)

open! Core
open! Hardcaml
open Signal

let addr_width = Memory_bus.Bus.addr_width
let bus_width = Memory_bus.Bus.cpu_bus_width
let block_size_bits = Memory_bus.Bus.block_size_bits

(* 512 * 32B blocks = 131 KB L1 cache. *)
let num_sets = 512
let bits_block_offset = address_bits_for (block_size_bits / 8)
let bits_index = address_bits_for num_sets
let bits_tag = addr_width - bits_block_offset - bits_index

(* Data is stored per-word. *)
let num_words = num_sets * (block_size_bits / bus_width)
let bits_word_index = address_bits_for num_words
let bits_word_offset = address_bits_for (bus_width / 8)

(* Accessors *)
let extract_tag addr = sel_top ~width:bits_tag addr

let extract_index addr =
  drop_bottom ~width:bits_block_offset addr |> sel_bottom ~width:bits_index
;;

let extract_word addr =
  drop_bottom ~width:bits_word_offset addr |> sel_bottom ~width:bits_word_index
;;

(** Convert an aligned store to a bus-width list of bytes with data masks. Little endian. *)
let store_to_bytes ~word_offset ~size ~data =
  let rep_data ~width =
    List.init (bus_width / width) ~f:(fun _ -> sel_bottom ~width data) |> concat_lsb
  in
  let data =
    mux size [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
    |> split_lsb ~part_width:8
  in
  let valids =
    [ "0001"; "0011"; "1111" ]
    |> List.map ~f:of_bit_string
    |> mux size
    |> uresize ~width:(bus_width / 8)
    |> log_shift ~f:sll ~by:word_offset
    |> split_lsb ~part_width:1
  in
  List.map2_exn data valids ~f:(fun value valid -> { With_valid.value; valid })
;;

(** Extract load data from a bus-width word. *)
let load_data_from_word ~word ~word_offset ~sign_extend ~size =
  let data =
    log_shift ~f:srl ~by:(word_offset @: of_bit_string "000") word |> sel_bottom ~width:32
  in
  let resize ~from_width =
    let bottom = sel_bottom ~width:from_width data in
    let extend_bit = msb bottom &&: sign_extend in
    if from_width = 32
    then bottom
    else sresize ~width:(32 - from_width) extend_bit @: bottom
  in
  mux size [ resize ~from_width:8; resize ~from_width:16; resize ~from_width:32 ]
;;

(** Access info from pipeline. Latched at input, so should come directly from execute
    stage. *)
module From_pipe = struct
  type 'a t =
    { addr : 'a [@bits addr_width]
    ; load : 'a
    ; store : 'a
    ; size : 'a [@bits 2] (* 00 = byte, 01 = half, 10 = word *)
    ; sign_extend : 'a (** Whether to sign-extend load data. *)
    ; store_data : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

(** Result of access to pipeline. *)
module To_pipe = struct
  type 'a t =
    { load_data : 'a [@bits 32]
    ; stall : 'a
    (** The memory stage is stalled. Output data is invalid, and any access from the
        pipeline is ignored. *)
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; mmu_state : 'a Mmu.State.t
    ; from_pipeline : 'a From_pipe.t
    ; cache_from_mem : 'a Memory_bus.Bus.From_mem.t
    ; walker_from_mem : 'a Memory_bus.Bus.From_mem.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { cache_to_mem : 'a Memory_bus.Bus.To_mem.t
    ; walker_to_mem : 'a Memory_bus.Bus.To_mem.t
    ; to_pipeline : 'a To_pipe.t
    }
  [@@deriving hardcaml]
end

(** Metadata associated with an entry in the cache. *)
module Metadata = struct
  type 'a t =
    { valid : 'a
    ; tag : 'a [@bits bits_tag]
    }
  [@@deriving hardcaml]
end

let create
  scope
  ({ clocking; mmu_state; from_pipeline; cache_from_mem; walker_from_mem } : _ I.t)
  =
  (* A load missed in the cache, so we must stall and fill the cache line. *)
  let%hw load_miss = wire 1 in
  (* Stall waiting for a store to write-through. *)
  let%hw store_stall = wire 1 in
  (* Stall waiting for address translation. *)
  let%hw stall_translate = wire 1 in
  let%hw stall = load_miss ||: store_stall ||: stall_translate in
  let%hw.Mmu.Translate.O.Of_signal translation =
    Mmu.Translate.hierarchical
      ~scope
      { clocking
      ; state = mmu_state
      ; va =
          { valid = from_pipeline.load ||: from_pipeline.store &&: ~:stall
          ; value = from_pipeline.addr
          }
      ; access_type =
          Mmu.Translate.Access_type.Of_signal.of_raw
            (mux2
               from_pipeline.store
               (Mmu.Translate.Access_type.to_raw
                  (Mmu.Translate.Access_type.Of_signal.of_enum
                     Mmu.Translate.Access_type.Cases.Store))
               (Mmu.Translate.Access_type.to_raw
                  (Mmu.Translate.Access_type.Of_signal.of_enum
                     Mmu.Translate.Access_type.Cases.Load)))
      ; walker_from_mem
      }
  in
  stall_translate <-- translation.result.stall;
  (* Keep the complete access registered, including during translation stalls.  The
     active version masks load/store until its physical address is usable. *)
  let%hw.From_pipe.Of_signal registered_access = From_pipe.Of_signal.wires () in
  let%hw.From_pipe.Of_signal next_access =
    From_pipe.Of_signal.mux2 stall registered_access from_pipeline
  in
  From_pipe.(
    Of_signal.assign registered_access (map next_access ~f:(Types.Clocking.reg clocking)));
  let%hw.From_pipe.Of_signal active_access =
    { registered_access with
      addr = translation.result.pa
    ; load = registered_access.load &&: ~:stall_translate
    ; store = registered_access.store &&: ~:stall_translate
    }
  in
  (* Extract all cache addresses from the translated physical address. *)
  let%hw active_tag = extract_tag active_access.addr in
  (* Tag memory. *)
  (* When [update_tag] is set (assuming [ready] is false, as we wouldn't update
     the tag except on a mismatch), tags will match on the next cycle. *)
  let%hw update_tag = wire 1 in
  let%hw.Metadata.Of_signal read_metadata =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_sets
        ~write_ports:
          [| { write_clock = clocking.clock
             ; write_enable = update_tag
             ; write_address = extract_index active_access.addr
             ; write_data = Metadata.Of_signal.pack { tag = active_tag; valid = vdd }
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
  let%hw tag_match = read_metadata.valid &&: (active_tag ==: read_metadata.tag) in
  load_miss <-- (active_access.load &&: ~:tag_match);
  (* Loads extract and extend data from a word loaded from memory. *)
  let%hw word_offset = sel_bottom ~width:bits_word_offset active_access.addr in
  let%hw loaded_word = wire bus_width in
  let%hw load_data =
    load_data_from_word
      ~word:loaded_word
      ~word_offset
      ~sign_extend:active_access.sign_extend
      ~size:active_access.size
  in
  (* Stores write-through, one cycle after loading the tag (when the instruction is technically in W). *)
  let%hw store_request = active_access.store in
  let%hw store_hit = tag_match &&: store_request in
  let%hw store_word =
    let original = split_lsb ~part_width:8 ~exact:true loaded_word in
    let%hw_list.With_valid.Of_signal overwrite_bytes =
      store_to_bytes ~word_offset ~size:active_access.size ~data:active_access.store_data
    in
    List.map2_exn overwrite_bytes original ~f:(fun { valid; value } -> mux2 valid value)
    |> concat_lsb
  in
  (* Cache data. Written by incoming fill data from memory on a load miss, or on a store hit. Read on a load. *)
  let data_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = store_hit ||: (load_miss &&: cache_from_mem.valid)
           ; (* Could probably register load_miss here and elsewhere for timing, as
               we don't need it to rise immediately and know when it will lower. *)
             write_address =
               extract_word (mux2 load_miss cache_from_mem.addr active_access.addr)
           ; write_data = mux2 load_miss cache_from_mem.data store_word
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word next_access.addr
           }
        |]
      ~name:"data"
      ()
  in
  loaded_word <-- data_mem.(0);
  (* Stores stall until acknowledged by memory (or a write buffer). *)
  store_stall <-- (store_request &&: ~:(cache_from_mem.ready));
  (* When a load has missed, request the block from memory until we receive the
     last word to fill the block back from memory. *)
  update_tag <-- (load_miss &&: cache_from_mem.last &&: cache_from_mem.valid);
  let%hw.Memory_bus.Bus.To_mem.Of_signal write_to_mem =
    { valid = store_request
    ; access_type = Memory_bus.Bus.Access_type.write_through
    ; addr = active_access.addr
    ; data = uresize ~width:bus_width active_access.store_data
    ; store_size = active_access.size
    ; last = vdd
    }
  in
  let%hw.Memory_bus.Bus.To_mem.Of_signal read_to_mem =
    { valid = load_miss &&: ~:update_tag
    ; access_type = Memory_bus.Bus.Access_type.read_block
    ; addr = active_access.addr
    ; data = zero bus_width
    ; store_size = zero 2
    ; last = gnd
    }
  in
  (* Loads and stores are mutually exclusive, and responses are ignored unless the
     corresponding access is outstanding. *)
  let%hw.Memory_bus.Bus.To_mem.Of_signal cache_to_mem =
    Memory_bus.Bus.To_mem.Of_signal.mux2 read_to_mem.valid read_to_mem write_to_mem
  in
  ({ cache_to_mem
   ; walker_to_mem = translation.walker_to_mem
   ; to_pipeline = { load_data; stall }
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
