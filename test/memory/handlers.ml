open! Core
open! Hardcaml
module I = Memory.Bus.From_mem
module O = Memory.Bus.To_mem
module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

let word_size_bytes = Memory.Bus.cpu_bus_width / 8
let block_size_bytes = Memory.Bus.block_size_bits / 8
let words_per_block = Memory.Bus.block_size_bits / Memory.Bus.cpu_bus_width
let word_base_addr addr = addr land lnot (word_size_bytes - 1)
let block_base_addr addr = addr land lnot (block_size_bytes - 1)
let zero = I.Of_bits.zero ()
let ready = { zero with ready = Bits.vdd }

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

let apply_write_through_store ~original ~addr ~store_data ~store_size =
  let num_bytes =
    match Bits.to_unsigned_int store_size with
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

let request_kind ({ access_type; _ } : Bits.t O.t) =
  let kinds =
    [ Bits.to_bool access_type.read_block, `Read_block
    ; Bits.to_bool access_type.read_word, `Read_word
    ; Bits.to_bool access_type.write_back, `Write_back
    ; Bits.to_bool access_type.write_through, `Write_through
    ]
    |> List.filter_map ~f:(fun (valid, kind) -> Option.some_if valid kind)
  in
  match kinds with
  | [ kind ] -> kind
  | _ ->
    raise_s
      [%message
        "valid memory request must assert exactly one access type"
          (access_type : Bits.t Memory.Bus.Access_type.t)]
;;

let merge_inputs = Step.merge_inputs

let spawn ~mem ~delay_cycles ~inputs ~outputs =
  (* Process any new request that arrived on the previous cycle. [ready] must
     have been asserted the cycle that [prev_outs] was called. *)
  let rec loop (prev_outs : Step.O_data.t) =
    let request = prev_outs.before_edge in
    if not (Bits.to_bool request.valid)
    then (
      let%bind.Step outs = Step.cycle ready in
      loop outs)
    else handle_request request
  and handle_request (request : Bits.t O.t) =
    match request_kind request with
    | `Read_block ->
      stream_block
        request
        ~addr:(Bits.to_unsigned_int request.addr |> block_base_addr)
        ~remaining_words:words_per_block
    | `Read_word ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      let addr = Bits.to_unsigned_int request.addr in
      let%bind.Step outs =
        Step.cycle
          { valid = Bits.vdd
          ; addr = request.addr
          ; data = read_word_from_mem mem addr
          ; last = Bits.vdd
          ; ready = Bits.vdd
          }
      in
      loop outs
    | `Write_back ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      Hashtbl.set
        mem
        ~key:(Bits.to_unsigned_int request.addr |> word_base_addr)
        ~data:request.data;
      let%bind.Step outs = Step.cycle ready in
      loop outs
    | `Write_through ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      let addr = Bits.to_unsigned_int request.addr in
      let word_addr = word_base_addr addr in
      let original =
        Hashtbl.find mem word_addr
        |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
      in
      Hashtbl.set
        mem
        ~key:word_addr
        ~data:
          (apply_write_through_store
             ~original
             ~addr
             ~store_data:request.data
             ~store_size:request.store_size);
      let%bind.Step outs = Step.cycle ready in
      loop outs
  and stream_block _request ~addr ~remaining_words =
    let cycles = delay_cycles () in
    let%bind.Step () = Step.delay zero ~num_cycles:cycles in
    let last = remaining_words = 1 in
    let%bind.Step outs =
      Step.cycle
        { valid = Bits.vdd
        ; addr = Bits.of_unsigned_int ~width:Memory.Bus.addr_width addr
        ; data =
            Hashtbl.find mem addr
            |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
        ; last = Bits.of_bool last
        ; ready = Bits.of_bool last
        }
    in
    if last
    then loop outs
    else
      stream_block
        _request
        ~addr:(addr + word_size_bytes)
        ~remaining_words:(remaining_words - 1)
  in
  Step.spawn_io ~inputs ~outputs (fun _ ->
    let%bind.Step outs = Step.cycle ready in
    loop outs)
;;
