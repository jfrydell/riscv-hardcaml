open! Core
open! Hardcaml
open Signal

let addr_width = 32
let cpu_bus_width = 64
let block_size_bits = 256

(** The operation performed by a unified memory-bus request. Exactly one field should be
    asserted for a valid request. *)
module Access_type = struct
  type 'a t =
    { read_block : 'a
    ; read_word : 'a
    ; write_back : 'a
    (** Write back a dirty cache block, overwriting the entire block in memory (one word
        at a time). *)
    ; write_through : 'a (** Write to a single word (or half or byte) of memory. *)
    }
  [@@deriving hardcaml]

  let read_block = { (Of_signal.zero ()) with read_block = vdd }
  let read_word = { (Of_signal.zero ()) with read_word = vdd }
  let write_back = { (Of_signal.zero ()) with write_back = vdd }
  let write_through = { (Of_signal.zero ()) with write_through = vdd }
  let is_write t = t.write_back ||: t.write_through
end

(** Requests and write data sent toward memory.

    We do not require any guarantees about the stability of the request once [valid] is
    asserted; it is ignored completely until [ready] is high.

    After accepting a request (with [ready] high), receivers keep [ready] low (not
    accepting any new request) until at least the cycle in which the response's [last] is
    asserted. So, requestors may safely hold [valid] high until the response is received
    (combinationally lowering at [last]).

    [data] and [last] carry write-back beats. [data] and [size] carry a write-through
    store. *)
module To_mem = struct
  type 'a t =
    { valid : 'a
    (** The outgoing request is valid. All other fields are ignored unless [valid] and
        [From_mem.ready] are both high. *)
    ; uncacheable : 'a
    (** The request must bypass any cache and access the underlying memory directly. *)
    ; access_type : 'a Access_type.t
    ; addr : 'a [@bits addr_width]
    ; data : 'a [@bits cpu_bus_width]
    ; size : 'a [@bits 2]
    (** For write-throughs and read-words, specifies the access size (byte, half, or word;
        11 is invalid). [addr] must be aligned to this granularity as well (TODO: relax). *)
    ; last : 'a (** The last word of a block is being written-through. *)
    }
  [@@deriving hardcaml]

  (** Splits a write-through into bytes to be written with enable bits (byte-granularity
      enables are set regardless of whether this is a valid write-through). *)
  let write_through_bytes { addr; data; size; _ } =
    let word_offset = sel_bottom ~width:(address_bits_for (cpu_bus_width / 8)) addr in
    let rep_data ~width =
      List.init (cpu_bus_width / width) ~f:(fun _ -> sel_bottom ~width data) |> concat_lsb
    in
    let data =
      mux size [ rep_data ~width:8; rep_data ~width:16; rep_data ~width:32 ]
      |> split_lsb ~part_width:8
    in
    let valids =
      [ "0001"; "0011"; "1111" ]
      |> List.map ~f:of_bit_string
      |> mux size
      |> uresize ~width:(cpu_bus_width / 8)
      |> log_shift ~f:sll ~by:word_offset
      |> split_lsb ~part_width:1
    in
    List.map2_exn data valids ~f:(fun value valid -> { With_valid.value; valid })
  ;;
end

(** Responses and flow control returned from memory. *)
module From_mem = struct
  type 'a t =
    { valid : 'a
    (** The [address], [data], and [last] signals represent a valid response to a read. *)
    ; addr : 'a [@bits addr_width]
    ; data : 'a [@bits cpu_bus_width]
    ; last : 'a
    (** This is the last word of a block-granularity read, or the only word in response to
        a word-granularity read. *)
    ; ready : 'a
    (** Accepts a [To_mem] request. This may rise as early as the cycle that the previous
        response's [last] is high, but no earlier. *)
    }
  [@@deriving hardcaml]
end
