(** SDRAM-backed main memory, converting all access requests to word-granularity reads and
    writes. Assumes SDRAM has the same word width as Memory.Bus.data_width, and supports
    byte-enables. *)

open! Core
open! Hardcaml
open Signal

module type Config = sig
  (** Number of words in memory. *)
  val capacity_words : int
end

module Make (Config : Config) = struct
  let bits_word_in_mem = address_bits_for Config.capacity_words
  let bytes_per_word = Memory_bus.data_width / 8
  let bits_byte_in_word = address_bits_for bytes_per_word
  let bytes_per_block = Memory_bus.block_size_bits / 8
  let bits_byte_in_block = address_bits_for bytes_per_block
  let words_per_block = Memory_bus.block_size_bits / Memory_bus.data_width
  let bits_word_in_block = address_bits_for words_per_block
  let bits_remaining = address_bits_for (words_per_block + 1)

  let addr_to_word_address addr =
    uresize ~width:bits_word_in_mem (drop_bottom ~width:bits_byte_in_word addr)
  ;;

  module To_dram = struct
    type 'a t =
      { read : 'a
      ; write : 'a
      ; address : 'a [@bits bits_word_in_mem]
      ; write_data : 'a [@bits Memory_bus.data_width]
      ; write_mask : 'a [@bits bytes_per_word]
      }
    [@@deriving hardcaml]
  end

  module From_dram = struct
    type 'a t =
      { read_data : 'a With_valid.t [@bits Memory_bus.data_width]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; from_cpu : 'a Memory_bus.To_mem.t
      ; from_dram : 'a From_dram.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { to_cpu : 'a Memory_bus.From_mem.t
      ; to_dram : 'a To_dram.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; from_cpu; from_dram } : _ I.t) =
    (* Take incoming access once the previous one is done, and set [access_type] to be high only when valid. *)
    let ready = wire 1 in
    let%hw.Memory_bus.To_mem.Of_signal incoming_access =
      { from_cpu with
        access_type =
          Memory_bus.Access_type.Of_signal.(
            mux2 from_cpu.valid from_cpu.access_type (zero ()))
      }
    in
    let%hw.Memory_bus.To_mem.Of_signal active_access =
      Memory_bus.To_mem.Of_signal.wires ()
    in
    let%hw.Memory_bus.To_mem.Of_signal next_access =
      Memory_bus.To_mem.Of_signal.mux2 ready incoming_access active_access
    in
    Memory_bus.To_mem.Of_signal.assign
      active_access
      (Memory_bus.To_mem.map next_access ~f:(Types.Clocking.reg clocking));
    (* Track address of currently-issuing DRAM read/write requests and (for
       reads) responses, as well as how many are remaining. For everything but
       read-block, there is only one request. *)
    let%hw_var issue_address = Types.Clocking.Var.reg clocking ~width:bits_word_in_mem in
    let%hw_var issues_remaining = Types.Clocking.Var.reg clocking ~width:bits_remaining in
    let%hw_var response_address =
      Types.Clocking.Var.reg clocking ~width:Memory_bus.addr_width
    in
    let%hw_var responses_remaining =
      Types.Clocking.Var.reg clocking ~width:bits_remaining
    in
    let%hw issue_active = issues_remaining.value <>:. 0 in
    let%hw response_active = responses_remaining.value <>:. 0 in
    ready <-- (~:issue_active &&: ~:response_active);
    Always.(
      compile
        [ when_
            ready
            [ (* TODO: update tests to allow returning requested word first, simplifying this *)
              if_
                incoming_access.access_type.read_block
                [ issue_address
                  <-- (drop_bottom ~width:bits_byte_in_block incoming_access.addr
                       @: zero bits_word_in_block
                       |> uresize ~width:bits_word_in_mem)
                ; response_address
                  <-- drop_bottom ~width:bits_byte_in_block incoming_access.addr
                      @: zero bits_byte_in_block
                ]
                [ issue_address <-- addr_to_word_address incoming_access.addr
                ; response_address <-- incoming_access.addr
                ]
            ; if_
                incoming_access.valid
                [ if_
                    incoming_access.access_type.read_block
                    [ issues_remaining <--. words_per_block ]
                    [ issues_remaining <--. 1 ]
                ; if_
                    incoming_access.access_type.read_block
                    [ responses_remaining <--. words_per_block ]
                  @@ elif
                       incoming_access.access_type.read_word
                       [ responses_remaining <--. 1 ]
                       [ responses_remaining <--. 0 ]
                ]
                [ issues_remaining <--. 0; responses_remaining <--. 0 ]
            ]
        ; when_
            (from_dram.ready &&: issue_active)
            [ issues_remaining <-- issues_remaining.value -:. 1
            ; if_
                (all_bits_set (sel_bottom ~width:bits_word_in_block issue_address.value))
                [ issue_address
                  <-- drop_bottom ~width:bits_word_in_block issue_address.value
                      @: zero bits_word_in_block
                ]
                [ issue_address <-- issue_address.value +:. 1 ]
            ]
        ; when_
            (from_dram.read_data.valid &&: response_active)
            [ responses_remaining <-- responses_remaining.value -:. 1
            ; response_address <-- response_address.value +:. bytes_per_word
            ]
        ]);
    let%hw write_through_data =
      let rep_data ~width =
        List.init (Memory_bus.data_width / width) ~f:(fun _ ->
          sel_bottom ~width active_access.data)
        |> concat_lsb
      in
      mux active_access.size [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
    in
    let%hw write_through_mask =
      [ "0001"; "0011"; "1111" ]
      |> List.map ~f:of_bit_string
      |> mux active_access.size
      |> uresize ~width:bytes_per_word
      |> log_shift ~f:sll ~by:(sel_bottom ~width:bits_byte_in_word active_access.addr)
    in
    let%hw.Memory_bus.From_mem.Of_signal to_cpu =
      { valid = from_dram.read_data.valid &&: (responses_remaining.value <>:. 0)
      ; addr = response_address.value
      ; data = from_dram.read_data.value
      ; last = responses_remaining.value ==:. 1
      ; ready
      }
    in
    let%hw.To_dram.Of_signal to_dram =
      { read = issue_active &&: Memory_bus.Access_type.is_read active_access.access_type
      ; write = issue_active &&: Memory_bus.Access_type.is_write active_access.access_type
      ; address = issue_address.value
      ; write_data =
          mux2 active_access.access_type.write_back active_access.data write_through_data
      ; write_mask =
          mux2
            active_access.access_type.write_back
            (ones bytes_per_word)
            write_through_mask
      }
    in
    ({ to_cpu; to_dram } : _ O.t)
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"main_memory_dram" create
  ;;
end
