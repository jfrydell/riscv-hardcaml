open! Core
open! Hardcaml
open Signal

let addr_width = Memory_bus.addr_width
let data_width = Memory_bus.data_width
let block_size_bits = Memory_bus.block_size_bits

(* Match the current L1 D-cache geometry. *)
let num_sets = 128
let bits_byte_in_block = address_bits_for (block_size_bits / 8)
let bits_set_index = address_bits_for num_sets
let bits_tag = addr_width - bits_byte_in_block - bits_set_index
let num_words = num_sets * (block_size_bits / data_width)
let bits_word_index = address_bits_for num_words
let bits_byte_in_word = address_bits_for (data_width / 8)
let extract_tag addr = sel_top ~width:bits_tag addr

let extract_set_index addr =
  drop_bottom ~width:bits_byte_in_block addr |> sel_bottom ~width:bits_set_index
;;

let extract_word addr =
  drop_bottom ~width:bits_byte_in_word addr |> sel_bottom ~width:bits_word_index
;;

let insn_from_word ~word ~word_offset =
  log_shift ~f:srl ~by:(word_offset @: of_bit_string "000") word |> sel_bottom ~width:32
;;

module From_pipe = struct
  type 'a t =
    { pc : 'a With_valid.t [@bits addr_width]
    ; trigger_flush : 'a
    }
  [@@deriving hardcaml]
end

module To_pipe = struct
  type 'a t =
    { insn : 'a [@bits 32]
    ; pc : 'a [@bits addr_width] (** Address of the instruction that was just fetched. *)
    ; valid : 'a
    ; fault : 'a
    (** A memory access fault occurred for the fetched address, so no [valid] instruction
        will be produced. *)
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; mmu_state : 'a Mmu.State.t
    ; cache_from_mem : 'a Memory_bus.From_mem.t
    ; walker_from_mem : 'a Memory_bus.From_mem.t
    ; from_pipeline : 'a From_pipe.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { cache_to_mem : 'a Memory_bus.To_mem.t
    ; walker_to_mem : 'a Memory_bus.To_mem.t
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

let create
  scope
  ({ clocking; mmu_state; from_pipeline; cache_from_mem; walker_from_mem } : _ I.t)
  =
  (* Busy = fetch in progress, waiting for translation or fill. 
     Flushing = actively flushing cache (active_pc.valid = gnd).
     Triggering_flush = will start flush once [~:busy]. *)
  let%hw busy = wire 1 in
  let%hw flushing = wire 1 in
  let%hw triggering_flush =
    Utils.sr ~style:`Mealy clocking ~set:from_pipeline.trigger_flush ~reset:flushing
  in
  (* When not busy or flushing, update active PC. If flush trigger, don't fetch. *)
  let%hw.With_valid.Of_signal active_pc = With_valid.Of_signal.wires addr_width in
  let%hw.With_valid.Of_signal next_pc =
    With_valid.map2
      ~f:(mux2 (busy ||: flushing))
      active_pc
      { from_pipeline.pc with valid = from_pipeline.pc.valid &&: ~:triggering_flush }
  in
  (* Cache line walk for flushing. *)
  let%hw flushing_addr =
    Types.Clocking.reg_fb
      clocking
      ~clear_to:(zero bits_set_index)
      ~enable:flushing
      ~width:bits_set_index
      ~f:(fun addr -> addr +:. 1)
  in
  flushing
  <-- Utils.sr
        ~set:(triggering_flush &&: ~:busy)
        ~reset:(all_bits_set flushing_addr)
        clocking;
  (* Miss = translation done but waiting for fill. *)
  let%hw miss = wire 1 in
  let%hw.Mmu.Translate.O.Of_signal translation =
    Mmu.Translate.hierarchical
      ~scope
      { clocking
      ; state = mmu_state
      ; va = { valid = next_pc.valid &&: ~:miss; value = next_pc.value }
      ; access_type =
          Mmu.Translate.Access_type.Of_signal.of_enum
            Mmu.Translate.Access_type.Cases.Instruction
      ; walker_from_mem
      }
  in
  let%hw active_pa = translation.result.pa in
  let%hw fault = translation.result.fault in
  With_valid.iter2 ~f:(fun a n -> a <-- Types.Clocking.reg clocking n) active_pc next_pc;
  let%hw active_tag = extract_tag active_pa in
  let%hw update_tag = wire 1 in
  let%hw.Metadata.Of_signal read_metadata =
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:num_sets
        ~write_ports:
          [| { write_clock = clocking.clock
             ; write_enable = flushing ||: update_tag
             ; write_address = mux2 flushing flushing_addr (extract_set_index active_pa)
             ; write_data =
                 Metadata.Of_signal.pack { tag = active_tag; valid = ~:flushing }
             }
          |]
        ~read_ports:
          [| { read_clock = clocking.clock
             ; read_enable = vdd
             ; read_address = extract_set_index next_pc.value
             }
          |]
        ~name:"tags"
        ()
    in
    Metadata.Of_signal.unpack mem.(0)
  in
  let%hw tag_match =
    active_pc.valid
    &&: translation.result.valid
    &&: read_metadata.valid
    &&: (active_tag ==: read_metadata.tag)
  in
  busy <-- (active_pc.valid &&: ~:(tag_match ||: fault));
  miss <-- (active_pc.valid &&: translation.result.valid &&: ~:tag_match);
  let data_mem =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:num_words
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_enable = cache_from_mem.valid
           ; write_address = extract_word cache_from_mem.addr
           ; write_data = cache_from_mem.data
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_enable = vdd
           ; read_address = extract_word next_pc.value
           }
        |]
      ~name:"data"
      ()
  in
  let%hw loaded_word = data_mem.(0) in
  let%hw word_offset = sel_bottom ~width:bits_byte_in_word active_pa in
  let%hw insn_value = insn_from_word ~word:loaded_word ~word_offset in
  update_tag <-- (miss &&: cache_from_mem.valid &&: cache_from_mem.last);
  ({ cache_to_mem =
       { valid = miss &&: ~:update_tag
       ; uncacheable = gnd
       ; access_type = Memory_bus.Access_type.read_block
       ; addr = active_pa
       ; data = zero data_width
       ; size = zero 2
       ; last = gnd
       }
   ; to_pipeline =
       { insn = insn_value
       ; pc = active_pc.value
       ; valid = tag_match &&: ~:(busy ||: flushing ||: triggering_flush)
       ; fault
       }
   ; walker_to_mem = translation.walker_to_mem
   }
   : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
