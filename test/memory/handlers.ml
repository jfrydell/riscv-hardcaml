open! Core
open! Hardcaml

let word_size_bytes = Memory.Iface.cpu_bus_width / 8
let block_size_bytes = Memory.Iface.block_size_bits / 8
let words_per_block = Memory.Iface.block_size_bits / Memory.Iface.cpu_bus_width
let word_base_addr addr = addr land lnot (word_size_bytes - 1)
let block_base_addr addr = addr land lnot (block_size_bytes - 1)

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

module Write_back = struct
  module I = Memory.Iface.Write_back.From_mem
  module O = Memory.Iface.Write_back.To_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  let ready : Bits.t I.t = { ready = Bits.vdd }
  let not_ready = I.Of_bits.zero ()

  let handle ~mem ~delay_cycles () =
    let rec wait_for_request ~ready_seen =
      let%bind.Step outs = Step.cycle ready_seen in
      if Bits.to_bool outs.before_edge.write
      then accept_request outs.before_edge
      else wait_for_request ~ready_seen
    and accept_request outs =
      Hashtbl.set mem ~key:(Bits.to_int_trunc outs.addr) ~data:outs.data;
      delay_before_ready ()
    and delay_before_ready () =
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay not_ready ~num_cycles:cycles in
      wait_for_request ~ready_seen:ready
    in
    delay_before_ready ()
  ;;

  let merge_inputs = Step.merge_inputs
  let spawn ~mem ~delay_cycles = Step.spawn_io (fun _ -> handle ~mem ~delay_cycles ())
end

module Read_block = struct
  module I = Memory.Iface.Read_block.From_mem
  module O = Memory.Iface.Read_block.To_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  let zero = I.Of_bits.zero ()

  let handle ~mem ~delay_cycles prev_outs =
    let rec stream_out addr remaining_words =
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay zero ~num_cycles:cycles in
      let last = remaining_words = 1 in
      let%bind.Step outs =
        Step.cycle
          { data =
              Hashtbl.find mem addr
              |> Option.value ~default:(Bits.zero Memory.Iface.cpu_bus_width)
          ; addr = Bits.of_int_trunc ~width:Memory.Iface.addr_width addr
          ; valid = Bits.vdd
          ; last = Bits.of_bool last
          }
      in
      if last then loop outs else stream_out (addr + word_size_bytes) (remaining_words - 1)
    and loop (prev_outs : Step.O_data.t) =
      if Bits.to_bool prev_outs.before_edge.load
      then (
        let addr = Bits.to_int_trunc prev_outs.before_edge.addr |> block_base_addr in
        stream_out addr words_per_block)
      else (
        let%bind.Step outs = Step.cycle zero in
        loop outs)
    in
    loop prev_outs
  ;;

  let merge_inputs = Step.merge_inputs
  let spawn ~mem ~delay_cycles = Step.spawn_io (handle ~mem ~delay_cycles)
end

module Write_through = struct
  module I = Memory.Iface.Write_through.From_mem
  module O = Memory.Iface.Write_through.To_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  let ready : Bits.t I.t = { store_ready = Bits.vdd }
  let not_ready = I.Of_bits.zero ()

  let handle ~mem ~delay_cycles () =
    let rec wait_for_request () =
      let%bind.Step outs = Step.cycle not_ready in
      if Bits.to_bool outs.before_edge.store
      then accept_request ()
      else wait_for_request ()
    and accept_request () =
      let cycles = delay_cycles () in
      let%bind.Step () = Step.delay not_ready ~num_cycles:cycles in
      let%bind.Step outs = Step.cycle ready in
      if Bits.to_bool outs.before_edge.store
      then (
        let addr = Bits.to_int_trunc outs.before_edge.addr in
        let word_addr = word_base_addr addr in
        let original =
          Hashtbl.find mem word_addr
          |> Option.value ~default:(Bits.zero Memory.Iface.cpu_bus_width)
        in
        let updated =
          apply_write_through_store
            ~original
            ~addr
            ~store_data:outs.before_edge.store_data
            ~store_size:outs.before_edge.store_size
        in
        Hashtbl.set mem ~key:word_addr ~data:updated);
      wait_for_request ()
    in
    wait_for_request ()
  ;;

  let merge_inputs = Step.merge_inputs
  let spawn ~mem ~delay_cycles = Step.spawn_io (fun _ -> handle ~mem ~delay_cycles ())
end
