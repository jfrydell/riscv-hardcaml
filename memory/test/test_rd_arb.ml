open! Core
open! Hardcaml

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; write_from_l1 : 'a Memory.Iface.Write_through.To_mem.t
      ; read0_from_l1 : 'a Memory.Iface.Read_block.To_mem.t
      ; read1_from_l1 : 'a Memory.Iface.Read_block.To_mem.t
      ; write_from_mem : 'a Memory.Iface.Write_through.From_mem.t
      ; read_from_mem : 'a Memory.Iface.Read_block.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { write_to_l1 : 'a Memory.Iface.Write_through.From_mem.t
      ; read0_to_l1 : 'a Memory.Iface.Read_block.From_mem.t
      ; read1_to_l1 : 'a Memory.Iface.Read_block.From_mem.t
      ; write_to_mem : 'a Memory.Iface.Write_through.To_mem.t
      ; read_to_mem : 'a Memory.Iface.Read_block.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create
    scope
    ({ clocking
     ; write_from_l1
     ; read0_from_l1
     ; read1_from_l1
     ; write_from_mem
     ; read_from_mem
     } :
      _ I.t)
    =
    let read_to_mem, read_resps =
      Memory.Arbiters.arb_rd
        ~scope
        ~clocking
        ~reqs:[ read0_from_l1; read1_from_l1 ]
        ~resp:read_from_mem
    in
    let read0_to_l1, read1_to_l1 =
      match read_resps with
      | [ read0_to_l1; read1_to_l1 ] -> read0_to_l1, read1_to_l1
      | _ -> raise_s [%message "arbiter returned unexpected number of response ports"]
    in
    { O.write_to_l1 = write_from_mem
    ; read0_to_l1
    ; read1_to_l1
    ; write_to_mem = write_from_l1
    ; read_to_mem
    }
  ;;
end

open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

let run ?(timeout = 1000) = run ~timeout ~create:Dut.create

let spawn_handlers ~mem =
  let%bind.Step _ =
    Handlers.Write_through.spawn
      ~mem
      ~delay_cycles:(fun () -> 0)
      ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
        { parent with
          write_from_mem =
            Handlers.Write_through.merge_inputs ~parent:parent.write_from_mem ~child
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

let spawn_read0_emitter ?model_mem ~events () =
  Emitters.Read_block.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with
        read0_from_l1 =
          Emitters.Read_block.merge_inputs ~parent:parent.read0_from_l1 ~child
      })
    ~outputs:(fun (p : _ Step.O.t) -> p.read0_to_l1)
    ()
;;

let spawn_read1_emitter ?model_mem ~events () =
  Emitters.Read_block.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with
        read1_from_l1 =
          Emitters.Read_block.merge_inputs ~parent:parent.read1_from_l1 ~child
      })
    ~outputs:(fun (p : _ Step.O.t) -> p.read1_to_l1)
    ()
;;

let with_memories ?(backing_mem = Int.Table.create ()) () =
  let mem = Hashtbl.copy backing_mem in
  let model_mem = Hashtbl.copy backing_mem in
  mem, model_mem
;;

let bits_of_hex hex = Bits.of_hex ~width:Memory.Iface.cpu_bus_width hex

let testbench ~mem ~model_mem ~write_events ~read0_events ~read1_events _ =
  let%bind.Step () = spawn_handlers ~mem in
  let%bind.Step writer = spawn_write_emitter ~model_mem ~events:write_events () in
  let%bind.Step reader0 = spawn_read0_emitter ~model_mem ~events:read0_events () in
  let%bind.Step reader1 = spawn_read1_emitter ~model_mem ~events:read1_events () in
  let%bind.Step () = Step.wait_for writer in
  let%bind.Step () = Step.wait_for reader0 in
  Step.wait_for reader1
;;

let%test_unit "read arbiter routes responses to both requestors" =
  let mem, model_mem =
    with_memories
      ~backing_mem:
        (Int.Table.of_alist_exn
           [ 0x100, bits_of_hex "0011223344556677"
           ; 0x108, bits_of_hex "8899aabbccddeeff"
           ; 0x110, bits_of_hex "0123456789abcdef"
           ; 0x118, bits_of_hex "fedcba9876543210"
           ; 0x200, bits_of_hex "0f1e2d3c4b5a6978"
           ; 0x208, bits_of_hex "8796a5b4c3d2e1f0"
           ; 0x210, bits_of_hex "13579bdf2468ace0"
           ; 0x218, bits_of_hex "deadbeefcafef00d"
           ])
      ()
  in
  run
  (* ~waves_config: *)
  (*   (Hardcaml_test_harness.Waves_config.to_file "/tmp/test.hardcaml_waveform.Z" *)
  (*    |> Hardcaml_test_harness.Waves_config.with_extra_cycles_after_test ~n:1) *)
    ~timeout:1000
    (testbench
       ~mem
       ~model_mem
       ~write_events:
         (Sequence.of_list
            [ Emitters.Write_through.Event.Delay 5
            ; Emitters.Write_through.Event.Store
                { addr = 0x10c; data = 0xfeedface; size = 2 }
            ])
       ~read0_events:
         (Sequence.of_list
            [ Emitters.Read_block.Event.Read { addr = 0x100 }
            ; Emitters.Read_block.Event.Read { addr = 0x100 }
            ])
       ~read1_events:
         (Sequence.of_list
            [ Emitters.Read_block.Event.Delay 1
            ; Emitters.Read_block.Event.Read { addr = 0x200 }
            ]))
;;

let access_generator ~writes ~reads0 ~reads1 =
  let open Quickcheck.Generator.Let_syntax in
  let%map write_events =
    Quickcheck.Generator.list_with_length
      writes
      (Emitters.Write_through.Event.quickcheck_generator ~max_set:0)
  and read0_events =
    Quickcheck.Generator.list_with_length
      reads0
      (Emitters.Read_block.Event.quickcheck_generator ~max_set:0)
  and read1_events =
    Quickcheck.Generator.list_with_length
      reads1
      (Emitters.Read_block.Event.quickcheck_generator ~max_set:0)
  in
  write_events, read0_events, read1_events
;;

let%test_unit "small random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "rd-arb-small")
    ~sexp_of:
      [%sexp_of:
        Emitters.Write_through.Event.t list
        * Emitters.Read_block.Event.t list
        * Emitters.Read_block.Event.t list]
    ~trials:100
    (access_generator ~writes:5 ~reads0:5 ~reads1:5)
    ~f:(fun (write_events, read0_events, read1_events) ->
      let mem, model_mem = with_memories () in
      run
        (* ~waves_config: *)
        (*   (Hardcaml_test_harness.Waves_config.to_file "/tmp/test.hardcaml_waveform.Z" *)
        (*    |> Hardcaml_test_harness.Waves_config.with_extra_cycles_after_test ~n:1) *)
        (testbench
           ~mem
           ~model_mem
           ~write_events:(Sequence.of_list write_events)
           ~read0_events:(Sequence.of_list read0_events)
           ~read1_events:(Sequence.of_list read1_events)))
;;

let%test_unit "larger random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "rd-arb-large")
    ~sexp_of:
      [%sexp_of:
        Emitters.Write_through.Event.t list
        * Emitters.Read_block.Event.t list
        * Emitters.Read_block.Event.t list]
    ~trials:10
    (access_generator ~writes:250 ~reads0:250 ~reads1:250)
    ~f:(fun (write_events, read0_events, read1_events) ->
      let mem, model_mem = with_memories () in
      run
        ~timeout:10_000
        (testbench
           ~mem
           ~model_mem
           ~write_events:(Sequence.of_list write_events)
           ~read0_events:(Sequence.of_list read0_events)
           ~read1_events:(Sequence.of_list read1_events)))
;;
