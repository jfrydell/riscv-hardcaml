open! Core
open! Hardcaml

module Main_memory = Memory.Main_memory_bram.Make (struct
    let capacity = 65536
  end)

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request : 'a Memory.Bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { response : 'a Memory.Bus.From_mem.t } [@@deriving hardcaml]
  end

  let create scope ({ clocking; request } : _ I.t) =
    let memory = Main_memory.hierarchical ~scope { clocking; from_cpu = request } in
    ({ response = memory.to_cpu } : _ O.t)
  ;;
end

open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

let run = run ~create:Dut.create
let bits_of_hex hex = Bits.of_hex ~width:Memory.Bus.cpu_bus_width hex

let with_memories ?(backing_mem = Int.Table.create ()) () =
  Hashtbl.copy backing_mem, Hashtbl.copy backing_mem
;;

let spawn_emitter ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request = Emitters.merge_inputs ~parent:parent.request ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response)
    ()
;;

let testbench ~model_mem ~events _ =
  let%bind.Step emitter = spawn_emitter ~model_mem ~events () in
  Step.wait_for emitter
;;

let%test_unit "write then read through main memory BRAM" =
  let _, model_mem = with_memories () in
  run
    (testbench
       ~model_mem
       ~events:
         (Sequence.of_list
            [ Emitters.Event.Write_through
                { addr = 0x104; data = bits_of_hex "00000000deadbeef"; size = 2 }
            ; Delay 4
            ; Read_block { addr = 0x104 }
            ]))
;;

let access_generator ~accesses =
  let access_generator =
    Quickcheck.Generator.weighted_union
      [ 1., Emitters.Event.write_through_generator ~max_set:4 ~io_accesses:false
      ; 1., Emitters.Event.read_block_generator ~max_set:4
      ]
  in
  Quickcheck.Generator.list_with_length accesses access_generator
;;

let%test_unit "small randomized tests" =
  Quickcheck.test
    ~seed:(`Deterministic "main-memory-bram-test-small")
    ~sexp_of:[%sexp_of: Emitters.Event.t list]
    ~trials:100
    (access_generator ~accesses:10)
    ~f:(fun events ->
      let _, model_mem = with_memories () in
      run ~timeout:10_000 (testbench ~model_mem ~events:(Sequence.of_list events)))
;;

let%test_unit "larger randomized tests" =
  Quickcheck.test
    ~seed:(`Deterministic "main-memory-bram-test-large")
    ~sexp_of:[%sexp_of: Emitters.Event.t list]
    ~trials:50
    (access_generator ~accesses:1_000)
    ~f:(fun events ->
      let _, model_mem = with_memories () in
      run ~timeout:20_000 (testbench ~model_mem ~events:(Sequence.of_list events)))
;;
