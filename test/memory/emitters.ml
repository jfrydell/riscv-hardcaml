open! Core
open! Hardcaml

let word_size_bytes = Memory.Iface.cpu_bus_width / 8
let block_size_bytes = Memory.Iface.block_size_bits / 8
let words_per_block = Memory.Iface.block_size_bits / Memory.Iface.cpu_bus_width
let conflicting_block_stride = 512 * block_size_bytes

let check_nonnegative_delay cycles =
  if cycles < 0 then raise_s [%message "delay must be nonnegative" (cycles : int)]
;;

let word_base_addr addr = addr land lnot (word_size_bytes - 1)
let block_base_addr addr = addr land lnot (block_size_bytes - 1)

let address_generator ?(size = 0) ?(max_set = 0) () =
  let open Quickcheck.Generator.Let_syntax in
  let%bind set_index = Int.gen_incl 0 max_set in
  let%bind way_number = Int.gen_incl 0 3
  and word = Int.gen_incl 0 ((2 * words_per_block) - 1) in
  let%map byte_offset =
    match size with
    | 0 -> Int.gen_incl 0 (word_size_bytes - 1)
    | 1 -> Quickcheck.Generator.of_list [ 0; 2; 4; 6 ]
    | 2 -> Quickcheck.Generator.of_list [ 0; 4 ]
    | size -> raise_s [%message "unsupported store size" (size : int)]
  in
  (way_number * conflicting_block_stride)
  + (((set_index * words_per_block) + word) * word_size_bytes)
  + byte_offset
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
  List.mapi original ~f:(fun index original ->
    if index < offset || index >= offset + num_bytes
    then original
    else (
      let byte_index = index - offset in
      let byte = (store_data lsr (byte_index * 8)) land 0xff in
      Bits.of_int_trunc ~width:8 byte))
  |> Bits.concat_lsb
;;

module Read_block = struct
  module I = Memory.Iface.Read_block.To_mem
  module O = Memory.Iface.Read_block.From_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  module Event = struct
    type t =
      | Read of { addr : int }
      | Delay of int
    [@@deriving sexp_of]

    let quickcheck_generator ~max_set =
      let open Quickcheck.Generator.Let_syntax in
      Quickcheck.Generator.weighted_union
        [ ( 3.
          , let%map addr = address_generator ~max_set () in
            Read { addr } )
        ; ( 1.
          , let%map cycles = Int.gen_incl 0 20 in
            Delay cycles )
        ]
    ;;
  end

  let merge_inputs = Step.merge_inputs

  (** We require that a read returns a value that was in memory on some cycle between the
      read starting and completing. This module tracks the values held by a block over
      time. *)
  module Block_values = struct
    module Bits_set = Set.Make_plain (Bits)

    type t = Bits_set.t Int.Map.t

    let from_mem ~mem ~addr =
      let base = block_base_addr addr in
      List.init words_per_block ~f:(fun i ->
        let addr = base + (word_size_bytes * i) in
        ( addr
        , Hashtbl.find mem addr
          |> Option.value ~default:(Bits.zero Memory.Iface.cpu_bus_width)
          |> Bits_set.singleton ))
      |> Int.Map.of_alist_exn
    ;;

    let union =
      Map.merge ~f:(fun ~key -> function
        | `Both (v1, v2) -> Some (Set.union v1 v2)
        | `Left _ | `Right _ ->
          raise_s
            [%message
              "Block_values.union called with separate address blocks; key not contained \
               in both"
                (key : int)])
    ;;

    let union_mem t ~mem ~addr = from_mem ~mem ~addr |> union t
  end

  (* Emit a read, holding [load] high until there is only one word remaining.
       (We could lower earlier; this ensures a 1-cycle delay event puts one
       cycle between loads assuming the response is valid). *)
  let rec emit_read addr remaining_words =
    if remaining_words > 1
    then (
      let%bind.Step outs =
        Step.cycle
          { addr = Bits.of_int_trunc ~width:Memory.Iface.addr_width addr
          ; load = Bits.vdd
          }
      in
      (* Ignore [last], as that must be for a prior emitted read. A bit of a
         hack, but makes running this in a loop work. *)
      if Bits.to_bool outs.before_edge.valid && not (Bits.to_bool outs.before_edge.last)
      then emit_read addr (remaining_words - 1)
      else emit_read addr remaining_words)
    else Step.return ()
  ;;

  (** Update the set of modeled values each dispatched read in a queue could return. To be
      called every cycle. *)
  let update_dispatched_model ~model_mem ~dispatched_reads =
    Queue.iter dispatched_reads ~f:(fun (addr, vals) ->
      vals := Block_values.union_mem !vals ~mem:model_mem ~addr)
  ;;

  (** Check the value returned for the current read, and update dispatched reads each
      cycle. *)
  let rec check_read ~model_mem ~model_vals ~dispatched_reads addr remaining_words =
    let%bind.Step outs = Step.cycle Step.input_hold in
    update_dispatched_model ~model_mem ~dispatched_reads;
    let model_vals = Block_values.union_mem ~mem:model_mem ~addr model_vals in
    if Bits.to_bool outs.before_edge.valid
    then (
      let expected_addr =
        block_base_addr addr + ((words_per_block - remaining_words) * word_size_bytes)
      in
      let expected_data = Map.find_exn model_vals expected_addr in
      let actual_addr = Bits.to_int_trunc outs.before_edge.addr in
      let actual_data = outs.before_edge.data in
      if (not (Set.exists expected_data ~f:(Bits.equal outs.before_edge.data)))
         || not (expected_addr = actual_addr)
      then
        raise_s
          [%message
            "unexpected read data"
              (actual_addr : int)
              (expected_addr : int)
              (actual_data : Bits.Hex.t)
              (expected_data : Block_values.Bits_set.t)];
      match Bits.to_bool outs.before_edge.last, remaining_words = 1 with
      | true, true -> check_next_read ~model_mem_opt:(Some model_mem) ~dispatched_reads
      | false, false ->
        check_read ~model_mem ~model_vals ~dispatched_reads addr (remaining_words - 1)
      | cache_last, expected_last ->
        raise_s
          [%message
            "got incorrect number of words in load"
              (cache_last : bool)
              (expected_last : bool)])
    else check_read ~model_mem ~model_vals ~dispatched_reads addr remaining_words

  (** In a loop, dequeue a dispatched read and (if [model_mem_opt] is given) check its
      returned value. *)
  and check_next_read ~model_mem_opt ~dispatched_reads =
    match model_mem_opt with
    | Some model_mem ->
      (match Queue.dequeue dispatched_reads with
       | Some (addr, model_vals) ->
         check_read
           ~model_mem
           ~model_vals:!model_vals
           ~dispatched_reads
           addr
           words_per_block
       | None ->
         let%bind.Step _ = Step.cycle Step.input_hold in
         update_dispatched_model ~model_mem ~dispatched_reads;
         check_next_read ~model_mem_opt ~dispatched_reads)
    | None ->
      let _ = Queue.dequeue dispatched_reads in
      let%bind.Step _ = Step.cycle Step.input_hold in
      check_next_read ~model_mem_opt ~dispatched_reads
  ;;

  let spawn ?model_mem ~events ~inputs ~outputs () =
    let zero = I.Of_bits.zero () in
    (* Queue dispatched reads (addr and current mem states), to check return values. *)
    let dispatched_reads = Queue.create () in
    let dispatch_read ~addr ~model_mem =
      match model_mem with
      | Some model_mem ->
        Queue.enqueue
          dispatched_reads
          (addr, ref (Block_values.from_mem ~mem:model_mem ~addr))
      | None -> ()
    in
    (* Issue reads. *)
    let rec issue_loop events =
      match Sequence.next events with
      | None -> Step.delay zero ~num_cycles:1
      | Some (Event.Delay cycles, events) ->
        check_nonnegative_delay cycles;
        let%bind.Step () = Step.delay zero ~num_cycles:cycles in
        issue_loop events
      | Some (Event.Read { addr }, events) ->
        dispatch_read ~model_mem ~addr;
        let%bind.Step () = emit_read addr words_per_block in
        issue_loop events
    in
    (* Issue events while repeatedy check reads (and updating dispatched model each cycle). *)
    Step.spawn_io ~inputs ~outputs (fun _ ->
      let%bind.Step _ =
        Step.spawn (fun _ -> check_next_read ~model_mem_opt:model_mem ~dispatched_reads)
      in
      issue_loop events)
  ;;
end

module Write_through = struct
  module I = Memory.Iface.Write_through.To_mem
  module O = Memory.Iface.Write_through.From_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  module Event = struct
    type t =
      | Store of
          { addr : int
          ; data : int
          ; size : int
          }
      | Delay of int
    [@@deriving sexp_of]

    let quickcheck_generator ~max_set =
      let open Quickcheck.Generator.Let_syntax in
      Quickcheck.Generator.weighted_union
        [ ( 3.
          , let%bind size = Int.gen_incl 0 2
            and data = Int.gen_uniform_incl Int.min_value Int.max_value in
            let%map addr = address_generator ~size ~max_set () in
            Store { addr; data; size } )
        ; ( 1.
          , let%map cycles = Int.gen_incl 0 20 in
            Delay cycles )
        ]
    ;;
  end

  let merge_inputs = Step.merge_inputs

  let spawn ?model_mem ~events ~inputs ~outputs () =
    let zero = I.Of_bits.zero () in
    let rec emit_store ~addr ~data ~size =
      let%bind.Step outs =
        Step.cycle
          { addr = Bits.of_int_trunc ~width:Memory.Iface.addr_width addr
          ; store = Bits.vdd
          ; store_data = Bits.of_int_trunc ~width:Memory.Iface.addr_width data
          ; store_size = Bits.of_int_trunc ~width:2 size
          }
      in
      if Bits.to_bool outs.before_edge.store_ready
      then (
        (match model_mem with
         | None -> ()
         | Some mem ->
           let word_addr = word_base_addr addr in
           let original =
             Hashtbl.find mem word_addr
             |> Option.value ~default:(Bits.zero Memory.Iface.cpu_bus_width)
           in
           let updated =
             apply_write_through_store ~original ~addr ~store_data:data ~store_size:size
           in
           Hashtbl.set mem ~key:word_addr ~data:updated);
        Step.return ())
      else emit_store ~addr ~data ~size
    in
    let rec loop events =
      match Sequence.next events with
      | None -> Step.delay ~num_cycles:1 zero
      | Some (Event.Delay cycles, events) ->
        check_nonnegative_delay cycles;
        let%bind.Step () = Step.delay zero ~num_cycles:cycles in
        loop events
      | Some (Event.Store { addr; data; size }, events) ->
        let%bind.Step () = emit_store ~addr ~data ~size in
        loop events
    in
    Step.spawn_io ~inputs ~outputs (fun _ -> loop events)
  ;;
end
