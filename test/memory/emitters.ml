open! Core
open! Hardcaml
module I = Memory.Bus.To_mem
module O = Memory.Bus.From_mem
module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

let word_size_bytes = Memory.Bus.cpu_bus_width / 8
let block_size_bytes = Memory.Bus.block_size_bits / 8
let words_per_block = Memory.Bus.block_size_bits / Memory.Bus.cpu_bus_width
let conflicting_block_stride = 512 * block_size_bytes
let word_base_addr addr = addr land lnot (word_size_bytes - 1)
let block_base_addr addr = addr land lnot (block_size_bytes - 1)

let check_nonnegative_delay cycles =
  if cycles < 0 then raise_s [%message "delay must be nonnegative" (cycles : int)]
;;

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

let read_word_from_mem mem addr =
  List.init word_size_bytes ~f:(fun byte_index ->
    let byte_addr = addr + byte_index in
    let word_addr = word_base_addr byte_addr in
    let word =
      Hashtbl.find mem word_addr
      |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
    in
    let low = (byte_addr - word_addr) * 8 in
    Bits.select word ~high:(low + 7) ~low)
  |> Bits.concat_lsb
;;

module Event = struct
  type t =
    | Read_block of { addr : int }
    | Read_word of { addr : int }
    | Write_back of
        { addr : int
        ; data : int
        ; last : bool
        }
    | Write_through of
        { addr : int
        ; data : int
        ; size : int
        }
    | Delay of int
  [@@deriving sexp_of]

  let read_block_generator ~max_set =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 3.
        , let%map addr = address_generator ~max_set () in
          Read_block { addr } )
      ; ( 1.
        , let%map cycles = Int.gen_incl 0 20 in
          Delay cycles )
      ]
  ;;

  let write_through_generator ~max_set =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 3.
        , let%bind size = Int.gen_incl 0 2
          and data = Int.gen_uniform_incl Int.min_value Int.max_value in
          let%map addr = address_generator ~size ~max_set () in
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
        |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
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
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with read_block = Bits.vdd }
    ; addr = Bits.of_int_trunc ~width:Memory.Bus.addr_width addr
    ; data = Bits.zero Memory.Bus.cpu_bus_width
    ; store_size = Bits.zero 2
    ; last = Bits.gnd
    }
  | Event.Read_word { addr } ->
    { I.valid = Bits.vdd
    ; access_type = { (Memory.Bus.Access_type.Of_bits.zero ()) with read_word = Bits.vdd }
    ; addr = Bits.of_int_trunc ~width:Memory.Bus.addr_width addr
    ; data = Bits.zero Memory.Bus.cpu_bus_width
    ; store_size = Bits.zero 2
    ; last = Bits.gnd
    }
  | Event.Write_back { addr; data; last } ->
    { I.valid = Bits.vdd
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with write_back = Bits.vdd }
    ; addr = Bits.of_int_trunc ~width:Memory.Bus.addr_width addr
    ; data = Bits.of_int_trunc ~width:Memory.Bus.cpu_bus_width data
    ; store_size = Bits.zero 2
    ; last = Bits.of_bool last
    }
  | Event.Write_through { addr; data; size } ->
    { I.valid = Bits.vdd
    ; access_type =
        { (Memory.Bus.Access_type.Of_bits.zero ()) with write_through = Bits.vdd }
    ; addr = Bits.of_int_trunc ~width:Memory.Bus.addr_width addr
    ; data = Bits.of_int_trunc ~width:Memory.Bus.cpu_bus_width data
    ; store_size = Bits.of_int_trunc ~width:2 size
    ; last = Bits.vdd
    }
  | Event.Delay _ -> zero
;;

let check_response_word ~expected_addr ~expected_data (response : Bits.t O.t) =
  let actual_addr = Bits.to_int_trunc response.addr in
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

let emit_read_block ~model_mem ~addr =
  let event = Event.Read_block { addr } in
  let request = request_of_event event in
  let rec loop model_values words_seen response_complete =
    let%bind.Step outs = Step.cycle request in
    let model_values =
      Option.value_map model_mem ~default:model_values ~f:(fun mem ->
        Block_values.union_mem model_values ~mem ~addr)
    in
    let response = outs.before_edge in
    let words_seen, response_complete =
      if Bits.to_bool response.valid
      then (
        if response_complete then raise_s [%message "read response continued after last"];
        let expected_addr = block_base_addr addr + (words_seen * word_size_bytes) in
        let expected_data = Map.find_exn model_values expected_addr in
        check_response_word ~expected_addr ~expected_data response;
        let expected_last = words_seen = words_per_block - 1 in
        let actual_last = Bits.to_bool response.last in
        if Bool.(actual_last <> expected_last)
        then
          raise_s
            [%message
              "got incorrect number of words in block read"
                (actual_last : bool)
                (expected_last : bool)];
        words_seen + 1, actual_last)
      else words_seen, response_complete
    in
    if Bits.to_bool response.ready
    then (
      if not response_complete
      then raise_s [%message "read request accepted before its response completed"];
      Step.return ())
    else loop model_values words_seen response_complete
  in
  let model_values =
    Option.value_map
      model_mem
      ~default:(Block_values.from_mem ~mem:(Int.Table.create ()) ~addr)
      ~f:(fun mem -> Block_values.from_mem ~mem ~addr)
  in
  loop model_values 0 false
;;

let emit_read_word ~model_mem ~addr =
  let event = Event.Read_word { addr } in
  let request = request_of_event event in
  let initial_values =
    Option.value_map
      model_mem
      ~default:(Bits_set.singleton (Bits.zero Memory.Bus.cpu_bus_width))
      ~f:(fun mem -> Bits_set.singleton (read_word_from_mem mem addr))
  in
  let rec loop expected_data response_complete =
    let%bind.Step outs = Step.cycle request in
    let expected_data =
      Option.value_map model_mem ~default:expected_data ~f:(fun mem ->
        Set.add expected_data (read_word_from_mem mem addr))
    in
    let response = outs.before_edge in
    let response_complete =
      if Bits.to_bool response.valid
      then (
        if response_complete
        then raise_s [%message "word read returned more than one response beat"];
        check_response_word ~expected_addr:addr ~expected_data response;
        if not (Bits.to_bool response.last)
        then raise_s [%message "word read response did not assert last"];
        true)
      else response_complete
    in
    if Bits.to_bool response.ready
    then (
      if not response_complete
      then raise_s [%message "word read accepted before its response completed"];
      Step.return ())
    else loop expected_data response_complete
  in
  loop initial_values false
;;

let update_model_for_write model_mem = function
  | Event.Write_through { addr; data; size } ->
    Option.iter model_mem ~f:(fun mem ->
      let word_addr = word_base_addr addr in
      let original =
        Hashtbl.find mem word_addr
        |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
      in
      Hashtbl.set
        mem
        ~key:word_addr
        ~data:
          (apply_write_through_store ~original ~addr ~store_data:data ~store_size:size))
  | Event.Write_back { addr; data; _ } ->
    Option.iter model_mem ~f:(fun mem ->
      Hashtbl.set
        mem
        ~key:(word_base_addr addr)
        ~data:(Bits.of_int_trunc ~width:Memory.Bus.cpu_bus_width data))
  | Read_block _ | Read_word _ | Delay _ -> ()
;;

let emit_write ~model_mem event =
  let request = request_of_event event in
  let rec loop () =
    let%bind.Step outs = Step.cycle request in
    if Bits.to_bool outs.before_edge.ready
    then (
      update_model_for_write model_mem event;
      Step.return ())
    else loop ()
  in
  loop ()
;;

let spawn ?model_mem ~events ~inputs ~outputs () =
  let rec loop events =
    match Sequence.next events with
    | None -> Step.delay zero ~num_cycles:1
    | Some (Event.Delay cycles, events) ->
      check_nonnegative_delay cycles;
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      loop events
    | Some (Event.Read_block { addr }, events) ->
      let%bind.Step () = emit_read_block ~model_mem ~addr in
      loop events
    | Some (Event.Read_word { addr }, events) ->
      let%bind.Step () = emit_read_word ~model_mem ~addr in
      loop events
    | Some (((Event.Write_back _ | Event.Write_through _) as event), events) ->
      let%bind.Step () = emit_write ~model_mem event in
      loop events
  in
  Step.spawn_io ~inputs ~outputs (fun _ -> loop events)
;;
