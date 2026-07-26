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
    match Bits.to_int_trunc store_size with
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
      let byte = (Bits.to_int_trunc store_data lsr (byte_index * 8)) land 0xff in
      Bits.of_int_trunc ~width:8 byte))
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
  let rec loop (prev_outs : Step.O_data.t) =
    (* On a completion cycle the arbiter may already present the next request before the
       edge. Dispatching that value directly avoids an idle cycle between transactions. *)
    let request = prev_outs.before_edge in
    if not (Bits.to_bool request.valid)
    then (
      let%bind.Step outs = Step.cycle zero in
      loop outs)
    else handle_request request
  and continue_after_completion (outs : Step.O_data.t) =
    (* An arbiter can present a different owner's request during the completion cycle.
       Consume that request immediately. If nobody was waiting, [loop] first lowers
       [ready], then waits for a later request. *)
    if Bits.to_bool outs.before_edge.valid
    then handle_request outs.before_edge
    else loop outs
  and handle_request (request : Bits.t O.t) =
    match request_kind request with
    | `Read_block ->
      stream_block
        request
        ~addr:(Bits.to_int_trunc request.addr |> block_base_addr)
        ~remaining_words:words_per_block
    | `Read_word ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      let addr = Bits.to_int_trunc request.addr in
      let%bind.Step outs =
        Step.cycle
          { valid = Bits.vdd
          ; addr = request.addr
          ; data = read_word_from_mem mem addr
          ; last = Bits.vdd
          ; ready = Bits.vdd
          }
      in
      continue_after_completion outs
    | `Write_back ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      Hashtbl.set
        mem
        ~key:(Bits.to_int_trunc request.addr |> word_base_addr)
        ~data:request.data;
      let%bind.Step outs = Step.cycle { zero with ready = Bits.vdd } in
      continue_after_completion outs
    | `Write_through ->
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      let addr = Bits.to_int_trunc request.addr in
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
      let%bind.Step outs = Step.cycle { zero with ready = Bits.vdd } in
      continue_after_completion outs
  and stream_block _request ~addr ~remaining_words =
    let cycles = delay_cycles () in
    let%bind.Step () = Step.delay zero ~num_cycles:cycles in
    let last = remaining_words = 1 in
    let%bind.Step outs =
      Step.cycle
        { valid = Bits.vdd
        ; addr = Bits.of_int_trunc ~width:Memory.Bus.addr_width addr
        ; data =
            Hashtbl.find mem addr
            |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
        ; last = Bits.of_bool last
        ; ready = Bits.of_bool last
        }
    in
    if last
    then continue_after_completion outs
    else
      stream_block
        _request
        ~addr:(addr + word_size_bytes)
        ~remaining_words:(remaining_words - 1)
  in
  Step.spawn_io ~inputs ~outputs loop
;;
