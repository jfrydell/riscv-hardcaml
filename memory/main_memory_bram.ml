open! Core
open! Hardcaml
open Signal

module type Config = sig
  (** Number of bytes in memory. *)
  val capacity : int
end

module Make (Config : Config) = struct
  include Cache_common.Make (struct
      let num_sets = Config.capacity * 8 / Memory_bus.block_size_bits
    end)

  let () =
    if num_words < 2
    then
      raise_s [%message "main memory must contain at least two words" (num_words : int)]
  ;;

  let word_base_addr addr =
    drop_bottom ~width:bits_byte_in_word addr @: zero bits_byte_in_word
  ;;

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; from_cpu : 'a Memory_bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { to_cpu : 'a Memory_bus.From_mem.t } [@@deriving hardcaml]
  end

  let create scope ({ clocking; from_cpu } : _ I.t) =
    let%hw access_done = wire 1 in
    let addr = from_cpu.addr in
    (* TODO: support write-back *)
    let incoming_access =
      { Active_access.valid = from_cpu.valid
      ; addr
      ; uncacheable = from_cpu.valid &&: from_cpu.uncacheable
      ; read_word = from_cpu.valid &&: from_cpu.access_type.read_word
      ; read_block = from_cpu.valid &&: from_cpu.access_type.read_block
      ; write_through = from_cpu.valid &&: from_cpu.access_type.write_through
      ; write_back = gnd
      ; store_data = from_cpu.data
      ; store_size = from_cpu.size
      ; tag = zero bits_tag
      ; index = zero bits_set_index
      }
    in
    let%hw.Active_access.Of_signal active_access = Active_access.Of_signal.wires () in
    let%hw loaded_word = wire data_width in
    let%hw.Read_stream.O.Of_signal read_stream =
      Read_stream.hierarchical
        ~scope
        { clocking
        ; start_stream = active_access.read_block &&: ~:access_done
        ; read_word = active_access.read_word &&: ~:access_done
        ; read_addr = active_access.addr
        }
    in
    let%hw.Read_stream.O.Of_signal read_stream_prev =
      Read_stream.O.map read_stream ~f:(Types.Clocking.reg clocking)
    in
    let%hw.Writing_store.Of_signal writing_store =
      Writing_store.(
        of_active active_access ~valid:active_access.write_through
        |> map ~f:(Types.Clocking.reg clocking))
    in
    let%hw updated_word =
      split_lsb ~part_width:8 ~exact:true loaded_word
      |> List.map2_exn writing_store.bytes ~f:(fun { valid; value } byte ->
        mux2 valid value byte)
      |> concat_lsb
    in
    let%hw data_read_addr =
      mux2
        (active_access.read_block ||: active_access.read_word)
        read_stream.read_addr
        active_access.addr
    in
    let%hw take_incoming = ~:(active_access.valid) ||: access_done in
    let%hw.Active_access.Of_signal next_access =
      Active_access.Of_signal.mux2 take_incoming incoming_access active_access
    in
    Active_access.Of_signal.assign
      active_access
      (Active_access.map next_access ~f:(Types.Clocking.reg clocking));
    access_done
    <-- (active_access.write_through
         ||: active_access.write_back
         ||: read_stream_prev.last);
    let%hw data_write_enable = writing_store.valid ||: active_access.write_back in
    let%hw data_write_addr =
      mux2 active_access.write_back active_access.addr writing_store.addr
    in
    let%hw data_write_value =
      mux2 active_access.write_back active_access.store_data updated_word
    in
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
        ~name:"main_memory_bram"
        ()
    in
    loaded_word <-- data_mem.(0);
    let%hw.Memory_bus.From_mem.Of_signal to_cpu =
      { valid = read_stream_prev.valid
      ; addr = read_stream_prev.read_addr
      ; data = loaded_word
      ; last = read_stream_prev.last
      ; ready = take_incoming
      }
    in
    ({ to_cpu } : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"main_memory_bram" create
  ;;
end
