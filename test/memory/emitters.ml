open! Core
open! Hardcaml
module I = Memory.Bus.To_mem
module O = Memory.Bus.From_mem
module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

let word_size_bytes = Memory.Bus.data_width / 8
let block_size_bytes = Memory.Bus.block_size_bits / 8
let words_per_block = Memory.Bus.block_size_bits / Memory.Bus.data_width
let conflicting_block_stride = 512 * block_size_bytes
let io_addr_bit = 1 lsl (Memory.Bus.addr_width - 1)
let word_base_addr addr = addr land lnot (word_size_bytes - 1)
let block_base_addr addr = addr land lnot (block_size_bytes - 1)
let is_io_addr addr = addr land io_addr_bit <> 0

let check_nonnegative_delay cycles =
  if cycles < 0 then raise_s [%message "delay must be nonnegative" (cycles : int)]
;;

let address_generator ?(size = 0) ?(max_set = 0) ?(io_accesses = false) () =
  let open Quickcheck.Generator.Let_syntax in
  let%bind set_index = Int.gen_incl 0 max_set in
  let%bind way_number = Int.gen_incl 0 3
  and word = Int.gen_incl 0 ((2 * words_per_block) - 1)
  and io =
    if io_accesses
    then
      Quickcheck.Generator.weighted_union
        [ 3., Quickcheck.Generator.return false; 1., Quickcheck.Generator.return true ]
    else Quickcheck.Generator.return false
  in
  let%map byte_offset =
    match size with
    | 0 -> Int.gen_incl 0 (word_size_bytes - 1)
    | 1 -> Quickcheck.Generator.of_list [ 0; 2; 4; 6 ]
    | 2 -> Quickcheck.Generator.of_list [ 0; 4 ]
    | size -> raise_s [%message "unsupported store size" (size : int)]
  in
  let addr =
    (way_number * conflicting_block_stride)
    + (((set_index * words_per_block) + word) * word_size_bytes)
    + byte_offset
  in
  if io then addr lor io_addr_bit else addr
;;

let apply_write_through_store ~original ~addr ~store_data ~store_size =
  let num_bytes =
    match store_size with
    | 0 -> 1
    | 1 -> 2
    | 2 -> 4
    | size -> raise_s [%message "unsupported store size" (size : int)]
  in
  let offset = addr mod word_size_bytes in
  let original = Bits.split_lsb ~part_width:8 ~exact:true original in
  let store_data = Bits.split_lsb ~part_width:8 ~exact:true store_data in
  List.mapi original ~f:(fun index original ->
    if index < offset || index >= offset + num_bytes
    then original
    else List.nth_exn store_data (index - offset))
  |> Bits.concat_lsb
;;

let read_word_from_mem mem addr =
  List.init word_size_bytes ~f:(fun byte_index ->
    let byte_addr = addr + byte_index in
    let word_addr = word_base_addr byte_addr in
    let word =
      Hashtbl.find mem word_addr
      |> Option.value ~default:(Bits.zero Memory.Bus.data_width)
    in
    let low = (byte_addr - word_addr) * 8 in
    Bits.select word ~high:(low + 7) ~low)
  |> Bits.concat_lsb
;;

module Event = struct
  type t =
    | Read_block of { addr : int }
    | Read_word of
        { addr : int
        ; size : int
        }
    | Write_back of
        { addr : int
        ; data : Bits.t
        ; last : bool
        (** At the moment, we just toggle this randomly, rather than always writing back
            entire blocks; the current L2 ignores it anyway. *)
        }
    | Write_through of
        { addr : int
        ; data : Bits.t
        ; size : int
        }
    | Delay of int
  [@@deriving sexp_of]

  let read_generator ~max_set ~io_accesses =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 3.
        , let%bind size = Int.gen_incl 0 2 in
          let%map addr = address_generator ~size ~max_set ~io_accesses () in
          if is_io_addr addr then Read_word { addr; size } else Read_block { addr } )
      ; ( 1.
        , let%map cycles = Int.gen_incl 0 20 in
          Delay cycles )
      ]
  ;;

  let read_block_generator ~max_set = read_generator ~max_set ~io_accesses:false

  let write_through_generator ~max_set ~io_accesses =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 3.
        , let%bind size = Int.gen_incl 0 2
          and data =
            List.gen_with_length Memory.Bus.data_width (Int.gen_incl 0 1)
            |> Quickcheck.Generator.map ~f:Bits.of_bit_list
          in
          let%map addr = address_generator ~size ~max_set ~io_accesses () in
          Write_through { addr; data; size } )
      ; ( 1.
        , let%map cycles = Int.gen_incl 0 20 in
          Delay cycles )
      ]
  ;;
end

module Bits_set = Set.Make_plain (Bits)

module Block_values = struct
  type t = Bits_set.t Int.Map.t

  let from_mem ~mem ~addr =
    let base = block_base_addr addr in
    List.init words_per_block ~f:(fun i ->
      let addr = base + (word_size_bytes * i) in
      ( addr
      , Hashtbl.find mem addr
        |> Option.value ~default:(Bits.zero Memory.Bus.data_width)
        |> Bits_set.singleton ))
    |> Int.Map.of_alist_exn
  ;;

  let union =
    Map.merge ~f:(fun ~key -> function
      | `Both (v1, v2) -> Some (Set.union v1 v2)
      | `Left _ | `Right _ ->
        raise_s
          [%message "Block_values.union called with separate address blocks" (key : int)])
  ;;

  let union_mem t ~mem ~addr = union t (from_mem ~mem ~addr)
end

let merge_inputs = Step.merge_inputs
let zero = I.Of_bits.zero ()

let request_of_event = function
  | Event.Read_block { addr } ->
    { I.valid = Bits.vdd
    ; uncacheable = Bits.gnd
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with read_block = Bits.vdd }
    ; addr = Bits.of_unsigned_int ~width:Memory.Bus.addr_width addr
    ; data = Bits.zero Memory.Bus.data_width
    ; size = Bits.zero 2
    ; last = Bits.gnd
    }
  | Event.Read_word { addr; size } ->
    { I.valid = Bits.vdd
    ; uncacheable = Bits.vdd
    ; access_type = { (Memory.Bus.Access_type.Of_bits.zero ()) with read_word = Bits.vdd }
    ; addr = Bits.of_unsigned_int ~width:Memory.Bus.addr_width addr
    ; data = Bits.zero Memory.Bus.data_width
    ; size = Bits.of_unsigned_int ~width:2 size
    ; last = Bits.gnd
    }
  | Event.Write_back { addr; data; last } ->
    { I.valid = Bits.vdd
    ; uncacheable = Bits.gnd
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with write_back = Bits.vdd }
    ; addr = Bits.of_unsigned_int ~width:Memory.Bus.addr_width addr
    ; data
    ; size = Bits.zero 2
    ; last = Bits.of_bool last
    }
  | Event.Write_through { addr; data; size } ->
    { I.valid = Bits.vdd
    ; uncacheable = Bits.of_bool (is_io_addr addr)
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with write_through = Bits.vdd }
    ; addr = Bits.of_unsigned_int ~width:Memory.Bus.addr_width addr
    ; data
    ; size = Bits.of_unsigned_int ~width:2 size
    ; last = Bits.vdd
    }
  | Event.Delay _ -> zero
;;

let check_response_word ~expected_addr ~expected_data (response : Bits.t O.t) =
  let actual_addr = Bits.to_unsigned_int response.addr in
  if actual_addr <> expected_addr || not (Set.mem expected_data response.data)
  then
    raise_s
      [%message
        "unexpected read data"
          (actual_addr : int)
          (expected_addr : int)
          (response.data : Bits.Hex.t)
          (expected_data : Bits_set.t)]
;;

module Expected_read = struct
  type t =
    | Block of
        { addr : int
        ; values : Block_values.t ref
        ; mutable words_seen : int
        }
    | Word of
        { addr : int
        ; values : Bits_set.t ref
        }

  let create ~model_mem = function
    | Event.Read_block { addr } ->
      let values =
        Option.value_map
          model_mem
          ~default:(Block_values.from_mem ~mem:(Int.Table.create ()) ~addr)
          ~f:(fun mem -> Block_values.from_mem ~mem ~addr)
      in
      Block { addr; values = ref values; words_seen = 0 }
    | Event.Read_word { addr; _ } ->
      let values =
        Option.value_map
          model_mem
          ~default:(Bits_set.singleton (Bits.zero Memory.Bus.data_width))
          ~f:(fun mem -> Bits_set.singleton (read_word_from_mem mem addr))
      in
      Word { addr; values = ref values }
    | Write_back _ | Write_through _ | Delay _ ->
      raise_s [%message "Expected_read.create called for a non-read event"]
  ;;

  let update_from_mem t mem =
    match t with
    | Block { addr; values; _ } -> values := Block_values.union_mem !values ~mem ~addr
    | Word { addr; values } -> values := Set.add !values (read_word_from_mem mem addr)
  ;;

  let check_response t (response : Bits.t O.t) =
    match t with
    | Block ({ addr; values; words_seen } as block) ->
      let expected_addr = block_base_addr addr + (words_seen * word_size_bytes) in
      check_response_word
        ~expected_addr
        ~expected_data:(Map.find_exn !values expected_addr)
        response;
      let expected_last = words_seen = words_per_block - 1 in
      let actual_last = Bits.to_bool response.last in
      if Bool.(actual_last <> expected_last)
      then
        raise_s
          [%message
            "got incorrect number of words in block read"
              (actual_last : bool)
              (expected_last : bool)];
      block.words_seen <- words_seen + 1;
      actual_last
    | Word { addr; values } ->
      check_response_word ~expected_addr:addr ~expected_data:!values response;
      if not (Bits.to_bool response.last)
      then raise_s [%message "word read response did not assert last"];
      true
  ;;
end

let update_model_for_write model_mem = function
  | Event.Write_through { addr; data; size } ->
    Option.iter model_mem ~f:(fun mem ->
      let word_addr = word_base_addr addr in
      let original =
        Hashtbl.find mem word_addr
        |> Option.value ~default:(Bits.zero Memory.Bus.data_width)
      in
      Hashtbl.set
        mem
        ~key:word_addr
        ~data:
          (apply_write_through_store ~original ~addr ~store_data:data ~store_size:size))
  | Event.Write_back { addr; data; _ } ->
    Option.iter model_mem ~f:(fun mem -> Hashtbl.set mem ~key:(word_base_addr addr) ~data)
  | Read_block _ | Read_word _ | Delay _ -> ()
;;

let emit_request ~model_mem ~active_read ~dispatched_reads ~outstanding_reads event =
  let request = request_of_event event in
  let expected_read =
    match event with
    | Event.Read_block _ | Read_word _ ->
      Int.incr outstanding_reads;
      let expected_read = Expected_read.create ~model_mem event in
      active_read := Some expected_read;
      Some expected_read
    | Write_back _ | Write_through _ -> None
    | Delay _ -> raise_s [%message "emit_request called for a delay event"]
  in
  let rec loop () =
    let%bind.Step outs = Step.cycle request in
    if Bits.to_bool outs.before_edge.ready
    then (
      update_model_for_write model_mem event;
      Option.iter expected_read ~f:(fun expected_read ->
        active_read := None;
        Queue.enqueue dispatched_reads expected_read);
      Step.return ())
    else loop ()
  in
  loop ()
;;

let spawn ?model_mem ~events ~inputs ~outputs () =
  (* Requests are issued by the main task below. Reads move from [active_read] to
     [dispatched_reads] when [ready] acknowledges them. Since read data may precede that
     acknowledgement, the response task buffers response beats until their read has
     entered the queue. *)
  let active_read = ref None in
  let dispatched_reads = Queue.create () in
  let responses = Queue.create () in
  let outstanding_reads = ref 0 in
  let update_expected_values () =
    Option.iter model_mem ~f:(fun mem ->
      Option.iter !active_read ~f:(fun read -> Expected_read.update_from_mem read mem);
      Queue.iter dispatched_reads ~f:(fun read -> Expected_read.update_from_mem read mem))
  in
  let check_queued_responses () =
    let rec loop () =
      match Queue.peek dispatched_reads, Queue.peek responses with
      | Some expected, Some response ->
        let complete = Expected_read.check_response expected response in
        ignore (Queue.dequeue_exn responses : Bits.t O.t);
        if complete
        then (
          ignore (Queue.dequeue_exn dispatched_reads : Expected_read.t);
          Int.decr outstanding_reads);
        loop ()
      | None, _ | _, None -> ()
    in
    loop ()
  in
  let rec response_loop () =
    let%bind.Step outs = Step.cycle Step.input_hold in
    (* Include the memory value from every cycle in which a read may be in flight. *)
    update_expected_values ();
    if Bits.to_bool outs.before_edge.valid then Queue.enqueue responses outs.before_edge;
    check_queued_responses ();
    response_loop ()
  in
  let rec wait_for_responses () =
    if !outstanding_reads = 0
    then (
      if not (Queue.is_empty responses)
      then raise_s [%message "received a response without a corresponding read request"];
      Step.delay zero ~num_cycles:1)
    else (
      let%bind.Step _ = Step.cycle zero in
      wait_for_responses ())
  in
  let rec loop events =
    match Sequence.next events with
    | None -> wait_for_responses ()
    | Some (Event.Delay cycles, events) ->
      check_nonnegative_delay cycles;
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      loop events
    | Some
        ( ((Event.Read_block _ | Read_word _ | Write_back _ | Write_through _) as event)
        , events ) ->
      let%bind.Step () =
        emit_request ~model_mem ~active_read ~dispatched_reads ~outstanding_reads event
      in
      loop events
  in
  Step.spawn_io ~inputs ~outputs (fun _ ->
    let%bind.Step _ = Step.spawn (fun _ -> response_loop ()) in
    loop events)
;;
