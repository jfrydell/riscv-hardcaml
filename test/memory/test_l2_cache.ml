open! Core
open! Hardcaml

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request0 : 'a Memory.Bus.To_mem.t
      ; request1 : 'a Memory.Bus.To_mem.t
      ; from_mem : 'a Memory.Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { response0 : 'a Memory.Bus.From_mem.t
      ; response1 : 'a Memory.Bus.From_mem.t
      ; to_mem : 'a Memory.Bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request0; request1; from_mem } : _ I.t) =
    let%hw.Memory.Bus.From_mem.Of_signal to_l1 = Memory.Bus.From_mem.Of_signal.wires () in
    let%tydi { up_resp = responses; dn_req = from_l1 } =
      Memory.Bus.Arbiter.Two.hierarchical
        ~scope
        { clocking; up_req = [ request0; request1 ]; dn_resp = to_l1 }
    in
    let response0, response1 =
      match responses with
      | [ response0; response1 ] -> response0, response1
      | _ -> raise_s [%message "arbiter returned unexpected number of response ports"]
    in
    let%hw.Memory.L2_cache.O.Of_signal l2 =
      Memory.L2_cache.hierarchical ~scope { clocking; from_l1; from_mem }
    in
    Memory.Bus.From_mem.Of_signal.assign to_l1 l2.to_l1;
    { O.response0; response1; to_mem = l2.to_mem }
  ;;
end

open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

let run = run ~create:Dut.create

let spawn_handlers ~mem =
  let%map.Step _ =
    Handlers.spawn
      ~mem
      ~delay_cycles:(fun () -> 0)
      ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
        { parent with from_mem = Handlers.merge_inputs ~parent:parent.from_mem ~child })
      ~outputs:(fun (p : _ Step.O.t) -> p.to_mem)
  in
  ()
;;

let spawn_write_emitter ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request0 = Emitters.merge_inputs ~parent:parent.request0 ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response0)
    ()
;;

let spawn_read_emitter ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request1 = Emitters.merge_inputs ~parent:parent.request1 ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response1)
    ()
;;

let bits_of_hex hex = Bits.of_hex ~width:Memory.Bus.cpu_bus_width hex

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
    (* ~waves_config: *)
    (*   (Hardcaml_test_harness.Waves_config.to_file "/tmp/test.hardcaml_waveform.Z" *)
    (*    |> Hardcaml_test_harness.Waves_config.with_extra_cycles_after_test ~n:1) *)
    (testbench
       ~mem
       ~model_mem
       ~write_events:
         (Sequence.of_list
            [ Emitters.Event.Write_through
                { addr = 0x104; data = bits_of_hex "00000000deadbeef"; size = 2 } ])
       ~read_events:
         (Sequence.of_list [ Emitters.Event.Delay 40; Read_block { addr = 0x104 } ]))
;;

let access_generator ~writes ~reads =
  let open Quickcheck.Generator.Let_syntax in
  let%map write_events =
    Quickcheck.Generator.list_with_length
      writes
      (Emitters.Event.write_through_generator ~max_set:4 ~io_accesses:true)
  and read_events =
    Quickcheck.Generator.list_with_length
      reads
      (Emitters.Event.read_generator ~max_set:4 ~io_accesses:true)
  in
  write_events, read_events
;;

let%test_unit "small random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "l2-test-small")
    ~sexp_of:[%sexp_of: Emitters.Event.t list * Emitters.Event.t list]
    ~trials:100
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
    ~sexp_of:[%sexp_of: Emitters.Event.t list * Emitters.Event.t list]
    ~trials:50
    (access_generator ~writes:500 ~reads:500)
    ~f:(fun (write_events, read_events) ->
      let mem, model_mem = with_memories () in
      run
        ~timeout:20_000
        (testbench
           ~mem
           ~model_mem
           ~write_events:(Sequence.of_list write_events)
           ~read_events:(Sequence.of_list read_events)))
;;
