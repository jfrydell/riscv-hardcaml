open! Core
open! Hardcaml

module Main_memory = Memory.Main_memory_dram.Make (struct
    let capacity_words = 65536
  end)

module Dram_step =
  Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make
    (Main_memory.From_dram)
    (Main_memory.To_dram)

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request : 'a Memory.Bus.To_mem.t
      ; from_dram : 'a Main_memory.From_dram.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { response : 'a Memory.Bus.From_mem.t
      ; to_dram : 'a Main_memory.To_dram.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request; from_dram } : _ I.t) =
    let memory =
      Main_memory.hierarchical ~scope { clocking; from_cpu = request; from_dram }
    in
    ({ response = memory.to_cpu; to_dram = memory.to_dram } : _ O.t)
  ;;
end

open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

let run = run ~create:Dut.create
let word_size_bytes = Memory.Bus.data_width / 8

let read_mem mem address =
  Hashtbl.find mem address |> Option.value ~default:(Bits.zero Memory.Bus.data_width)
;;

let write_mem ~mem ~address ~data ~mask =
  let old_data = read_mem mem address in
  let old_bytes = Bits.split_lsb ~part_width:8 ~exact:true old_data in
  let new_bytes = Bits.split_lsb ~part_width:8 ~exact:true data in
  let data =
    List.mapi old_bytes ~f:(fun byte_index old_byte ->
      let enabled = Bits.select mask ~high:byte_index ~low:byte_index |> Bits.to_bool in
      if enabled then List.nth_exn new_bytes byte_index else old_byte)
    |> Bits.concat_lsb
  in
  Hashtbl.set mem ~key:address ~data
;;

let spawn_dram ~mem =
  let random = Random.State.make [| 0x53445241; 0x4d |] in
  let next_delay () = Random.State.int random 4 in
  let dram_input ~ready ~read_data =
    { Main_memory.From_dram.ready = Bits.of_bool ready
    ; read_data =
        { valid = Bits.of_bool (Option.is_some read_data)
        ; value = Option.value read_data ~default:(Bits.zero Memory.Bus.data_width)
        }
    }
  in
  let idle = dram_input ~ready:false ~read_data:None in
  let ready = dram_input ~ready:true ~read_data:None in
  let rec loop (previous_outputs : Dram_step.O_data.t) =
    (* The cycle producing [previous_outputs] asserted [ready], so the command
       immediately before that edge was accepted. *)
    let command = previous_outputs.before_edge in
    let read = Bits.to_bool command.read in
    let write = Bits.to_bool command.write in
    if read && write then raise_s [%message "DRAM read and write asserted together"];
    if not (read || write)
    then (
      let%bind.Dram_step outputs = Dram_step.cycle ready in
      loop outputs)
    else (
      (* TODO: separate task for processing to allow ready to raise before
         response (not done by current HW SDRAM controller). *)
      let address = Bits.to_unsigned_int command.address * word_size_bytes in
      if write
      then write_mem ~mem ~address ~data:command.write_data ~mask:command.write_mask;
      let%bind.Dram_step () = Dram_step.delay idle ~num_cycles:(next_delay ()) in
      let%bind.Dram_step () =
        if read
        then (
          let%bind.Dram_step _ =
            Dram_step.cycle
              (dram_input ~ready:false ~read_data:(Some (read_mem mem address)))
          in
          Dram_step.return ())
        else Dram_step.return ()
      in
      let%bind.Dram_step () = Dram_step.delay idle ~num_cycles:(next_delay ()) in
      let%bind.Dram_step outputs = Dram_step.cycle ready in
      loop outputs)
  in
  Dram_step.spawn_io
    ~inputs:(fun ~(parent : _ Step.I.t) ~child -> { parent with from_dram = child })
    ~outputs:(fun (outputs : _ Step.O.t) -> outputs.to_dram)
    (fun _ ->
      let%bind.Dram_step outputs = Dram_step.cycle ready in
      loop outputs)
;;

let spawn_emitter ~model_mem ~events =
  Emitters.spawn
    ~model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request = Emitters.merge_inputs ~parent:parent.request ~child })
    ~outputs:(fun (outputs : _ Step.O.t) -> outputs.response)
    ()
;;

let request_data_generator =
  List.gen_with_length Memory.Bus.data_width (Int.gen_incl 0 1)
  |> Quickcheck.Generator.map ~f:Bits.of_bit_list
;;

let write_back_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind addr = Emitters.address_generator ~max_set:4 ~io_accesses:false ()
  and data = request_data_generator
  and last = Bool.quickcheck_generator in
  Quickcheck.Generator.return (Emitters.Event.Write_back { addr; data; last })
;;

let read_word_generator =
  Quickcheck.Generator.map
    (Emitters.address_generator ~size:2 ~max_set:4 ~io_accesses:false ())
    ~f:(fun addr -> Emitters.Event.Read_word { addr; size = 2 })
;;

let access_generator =
  Quickcheck.Generator.weighted_union
    [ 1., Emitters.Event.write_through_generator ~max_set:4 ~io_accesses:false
    ; 1., Emitters.Event.read_block_generator ~max_set:4
    ; 1., read_word_generator
    ; 1., write_back_generator
    ]
;;

let events_generator accesses =
  Quickcheck.Generator.list_with_length accesses access_generator
;;

let with_memories () =
  let mem = Int.Table.create () in
  for word = 0 to 16383 do
    let data = Bits.of_unsigned_int ~width:Memory.Bus.data_width (word * 0x10203) in
    Hashtbl.set mem ~key:(word * word_size_bytes) ~data
  done;
  Hashtbl.copy mem, Hashtbl.copy mem
;;

let%test_unit "randomized requests through main memory DRAM" =
  Quickcheck.test
    ~seed:(`Deterministic "main-memory-dram-test")
    ~sexp_of:[%sexp_of: Emitters.Event.t list]
    ~trials:100
    (events_generator 50)
    ~f:(fun events ->
      let mem, model_mem = with_memories () in
      run ~timeout:20_000 (fun () ->
        let%bind.Step _dram = spawn_dram ~mem in
        let%bind.Step emitter =
          spawn_emitter ~model_mem ~events:(Sequence.of_list events)
        in
        Step.wait_for emitter))
;;
