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

let address_generator ?(size = 0) () =
  let open Quickcheck.Generator.Let_syntax in
  let%bind block = Int.gen_incl 0 3
  and word = Int.gen_incl 0 (words_per_block - 1) in
  let%map byte =
    match size with
    | 0 -> Int.gen_incl 0 (word_size_bytes - 1)
    | 1 -> Quickcheck.Generator.of_list [ 0; 2; 4; 6 ]
    | 2 -> Quickcheck.Generator.of_list [ 0; 4 ]
    | size -> raise_s [%message "unsupported store size" (size : int)]
  in
  (block * conflicting_block_stride) + (word * word_size_bytes) + byte
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

module Write_back = struct
  module I = Memory.Iface.Write_back.To_mem
  module O = Memory.Iface.Write_back.From_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  module Event = struct
    type t = Delay of int [@@deriving sexp_of]

    let quickcheck_generator =
      Quickcheck.Generator.map (Int.gen_incl 0 20) ~f:(fun cycles -> Delay cycles)
    ;;
  end

  let merge_inputs = Step.merge_inputs

  let spawn ?model_mem ~events ~inputs ~outputs () =
    let _ = model_mem in
    let rec loop events =
      match Sequence.next events with
      | None -> Step.return ()
      | Some (Event.Delay cycles, events) ->
        check_nonnegative_delay cycles;
        let%bind.Step () = Step.delay (I.Of_bits.zero ()) ~num_cycles:cycles in
        loop events
    in
    Step.spawn_io ~inputs ~outputs (fun _ -> loop events)
  ;;
end

module Read_block = struct
  module I = Memory.Iface.Read_block.To_mem
  module O = Memory.Iface.Read_block.From_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  module Event = struct
    type t =
      | Read of { addr : int }
      | Delay of int
    [@@deriving sexp_of]

    let quickcheck_generator =
      let open Quickcheck.Generator.Let_syntax in
      Quickcheck.Generator.weighted_union
        [ ( 3.
          , let%map addr = address_generator () in
            Read { addr } )
        ; ( 1.
          , let%map cycles = Int.gen_incl 0 20 in
            Delay cycles )
        ]
    ;;
  end

  let merge_inputs = Step.merge_inputs

  let spawn ?model_mem ~events ~inputs ~outputs () =
    let zero = I.Of_bits.zero () in
    let rec emit_read addr remaining_words =
      let%bind.Step outs =
        Step.cycle
          { addr = Bits.of_int_trunc ~width:Memory.Iface.addr_width addr
          ; load = Bits.vdd
          }
      in
      (* Check after edge (current cycle) so we can disable load request once
         [last] goes high.
         TODO: maybe add [ready] signal to load and lower then; this is really
         holding until about to maybe accept duplicate, which is longer than
         needed. *)
      if Bits.to_bool outs.after_edge.valid
      then (
        (match model_mem with
         | None -> ()
         | Some mem ->
           let expected_addr =
             block_base_addr addr + ((words_per_block - remaining_words) * word_size_bytes)
           in
           let expected_data =
             Hashtbl.find mem expected_addr
             |> Option.value ~default:(Bits.zero Memory.Iface.cpu_bus_width)
           in
           let actual_data = outs.after_edge.data in
           if not (Bits.equal outs.after_edge.data expected_data)
           then
             raise_s
               [%message
                 "unexpected read data"
                   (addr : int)
                   (expected_addr : int)
                   (actual_data : Bits.Hex.t)
                   (expected_data : Bits.Hex.t)]);
        match Bits.to_bool outs.after_edge.last, remaining_words = 1 with
        | true, true -> Step.return ()
        | false, false -> emit_read addr (remaining_words - 1)
        | cache_last, expected_last ->
          raise_s
            [%message
              "got incorrect number of words in load"
                (cache_last : bool)
                (expected_last : bool)])
      else emit_read addr remaining_words
    in
    let rec loop events =
      match Sequence.next events with
      | None -> Step.delay zero ~num_cycles:1
      | Some (Event.Delay cycles, events) ->
        check_nonnegative_delay cycles;
        let%bind.Step () = Step.delay zero ~num_cycles:cycles in
        loop events
      | Some (Event.Read { addr }, events) ->
        let%bind.Step () = emit_read addr words_per_block in
        loop events
    in
    Step.spawn_io ~inputs ~outputs (fun _ -> loop events)
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

    let quickcheck_generator =
      let open Quickcheck.Generator.Let_syntax in
      Quickcheck.Generator.weighted_union
        [ ( 3.
          , let%bind size = Int.gen_incl 0 2
            and data = Int.gen_uniform_incl Int.min_value Int.max_value in
            let%map addr = address_generator ~size () in
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
      | None -> Step.return ()
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
