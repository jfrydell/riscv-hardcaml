open! Core
open! Hardcaml

open
  Hardcaml_test_harness.Step_harness.Functional.Make_monadic
    (Memory.L2_cache.I)
    (Memory.L2_cache.O)

let run = run ~create:Memory.L2_cache.create

let spawn_handlers ~mem =
  let%bind.Step _ =
    Handlers.Write_back.spawn
      ~mem
      ~delay_cycles:(fun () -> 0)
      ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
        { parent with
          write_from_mem =
            Handlers.Write_back.merge_inputs ~parent:parent.write_from_mem ~child
        })
      ~outputs:(fun (p : _ Step.O.t) -> p.write_to_mem)
  in
  let%bind.Step _ =
    Handlers.Read_block.spawn
      ~mem
      ~delay_cycles:(fun () -> 0)
      ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
        { parent with
          read_from_mem =
            Handlers.Read_block.merge_inputs ~parent:parent.read_from_mem ~child
        })
      ~outputs:(fun (p : _ Step.O.t) -> p.read_to_mem)
  in
  Step.return ()
;;

let spawn_write_emitter ?model_mem ~events () =
  Emitters.Write_through.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with
        write_from_l1 =
          Emitters.Write_through.merge_inputs ~parent:parent.write_from_l1 ~child
      })
    ~outputs:(fun (p : _ Step.O.t) -> p.write_to_l1)
    ()
;;

let spawn_read_emitter ?model_mem ~events () =
  Emitters.Read_block.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with
        read_from_l1 = Emitters.Read_block.merge_inputs ~parent:parent.read_from_l1 ~child
      })
    ~outputs:(fun (p : _ Step.O.t) -> p.read_to_l1)
    ()
;;

let bits_of_hex hex = Bits.of_hex ~width:Memory.Iface.cpu_bus_width hex

let with_memories ?(backing_mem = Int.Table.create ()) () =
  let mem = Hashtbl.copy backing_mem in
  let model_mem = Hashtbl.copy backing_mem in
  mem, model_mem
;;

let testbench ~mem ~model_mem ~write_events ~read_events _ =
  let%bind.Step () = spawn_handlers ~mem in
  let%bind.Step writer = spawn_write_emitter ~model_mem ~events:write_events () in
  let%bind.Step reader = spawn_read_emitter ~model_mem ~events:read_events () in
  let%bind.Step () = Step.wait_for writer in
  Step.wait_for reader
;;

let%test_unit "write then read through l2 cache" =
  let mem, model_mem =
    with_memories
      ~backing_mem:
        (Int.Table.of_alist_exn
           [ 0x100, bits_of_hex "0011223344556677"
           ; 0x108, bits_of_hex "8899aabbccddeeff"
           ; 0x110, bits_of_hex "0123456789abcdef"
           ; 0x118, bits_of_hex "fedcba9876543210"
           ])
      ()
  in
  run
    (testbench
       ~mem
       ~model_mem
       ~write_events:
         (Sequence.of_list
            [ Emitters.Write_through.Event.Store
                { addr = 0x104; data = 0xdeadbeef; size = 2 }
            ])
       ~read_events:
         (Sequence.of_list [ Emitters.Read_block.Event.Delay 40; Read { addr = 0x104 } ]))
;;

let access_generator ~writes ~reads =
  let open Quickcheck.Generator.Let_syntax in
  let%map write_events =
    Quickcheck.Generator.list_with_length
      writes
      Emitters.Write_through.Event.quickcheck_generator
  and read_events =
    Quickcheck.Generator.list_with_length
      reads
      Emitters.Read_block.Event.quickcheck_generator
  in
  write_events, read_events
;;

let%test_unit "small random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "l2-test-small")
    ~sexp_of:
      [%sexp_of: Emitters.Write_through.Event.t list * Emitters.Read_block.Event.t list]
    ~trials:1000
    (access_generator ~writes:5 ~reads:5)
    ~f:(fun (write_events, read_events) ->
      let mem, model_mem = with_memories () in
      run
      (* ~waves_config: *)
      (*   (Hardcaml_test_harness.Waves_config.to_file "/tmp/test.hardcaml_waveform.Z") *)
        ~timeout:10_000
        (testbench
           ~mem
           ~model_mem
           ~write_events:(Sequence.of_list write_events)
           ~read_events:(Sequence.of_list read_events)))
;;

let%test_unit "larger random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "l2-test-large")
    ~sexp_of:
      [%sexp_of: Emitters.Write_through.Event.t list * Emitters.Read_block.Event.t list]
    ~trials:500
    (access_generator ~writes:500 ~reads:500)
    ~f:(fun (write_events, read_events) ->
      let mem, model_mem = with_memories () in
      run
        ~timeout:10_000
        (testbench
           ~mem
           ~model_mem
           ~write_events:(Sequence.of_list write_events)
           ~read_events:(Sequence.of_list read_events)))
;;
