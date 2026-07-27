open! Core
open! Hardcaml

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request0 : 'a Memory.Bus.To_mem.t
      ; request1 : 'a Memory.Bus.To_mem.t
      ; request2 : 'a Memory.Bus.To_mem.t
      ; from_mem : 'a Memory.Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { response0 : 'a Memory.Bus.From_mem.t
      ; response1 : 'a Memory.Bus.From_mem.t
      ; response2 : 'a Memory.Bus.From_mem.t
      ; to_mem : 'a Memory.Bus.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request0; request1; request2; from_mem } : _ I.t) =
    let result =
      Memory.Bus.Arbiter.Three.hierarchical
        ~scope
        { clocking; up_req = [ request0; request1; request2 ]; dn_resp = from_mem }
    in
    match result.up_resp with
    | [ response0; response1; response2 ] ->
      ({ response0; response1; response2; to_mem = result.dn_req } : _ O.t)
    | _ -> failwith "unreachable"
  ;;
end

open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

let run ?(timeout = 1000) = run ~timeout ~create:Dut.create

let spawn_handler ~mem =
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

let spawn_emitter0 ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request0 = Emitters.merge_inputs ~parent:parent.request0 ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response0)
    ()
;;

let spawn_emitter1 ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request1 = Emitters.merge_inputs ~parent:parent.request1 ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response1)
    ()
;;

let spawn_emitter2 ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with request2 = Emitters.merge_inputs ~parent:parent.request2 ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.response2)
    ()
;;

let with_memories ?(backing_mem = Int.Table.create ()) () =
  Hashtbl.copy backing_mem, Hashtbl.copy backing_mem
;;

let bits_of_hex hex = Bits.of_hex ~width:Memory.Bus.cpu_bus_width hex

let testbench ~mem ~model_mem ~events0 ~events1 ~events2 _ =
  let%bind.Step () = spawn_handler ~mem in
  let%bind.Step emitter0 = spawn_emitter0 ~model_mem ~events:events0 () in
  let%bind.Step emitter1 = spawn_emitter1 ~model_mem ~events:events1 () in
  let%bind.Step emitter2 = spawn_emitter2 ~model_mem ~events:events2 () in
  let%bind.Step () = Step.wait_for emitter0 in
  let%bind.Step () = Step.wait_for emitter1 in
  Step.wait_for emitter2
;;

let%test_unit "arbiter routes mixed accesses and responses to every requester" =
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
    ~timeout:1000
    (testbench
       ~mem
       ~model_mem
       ~events0:
         (Sequence.of_list
            [ Emitters.Event.Delay 5
            ; Write_through
                { addr = 0x10c; data = bits_of_hex "00000000feedface"; size = 2 }
            ])
       ~events1:
         (Sequence.of_list
            [ Emitters.Event.Read_block { addr = 0x100 }; Read_block { addr = 0x100 } ])
       ~events2:(Sequence.of_list [ Emitters.Event.Delay 1; Read_block { addr = 0x200 } ]))
;;

let access_generator ~writes ~reads0 ~reads1 =
  let open Quickcheck.Generator.Let_syntax in
  let%map events0 =
    Quickcheck.Generator.list_with_length
      writes
      (Emitters.Event.write_through_generator ~max_set:0)
  and events1 =
    Quickcheck.Generator.list_with_length
      reads0
      (Emitters.Event.read_block_generator ~max_set:0)
  and events2 =
    Quickcheck.Generator.list_with_length
      reads1
      (Emitters.Event.read_block_generator ~max_set:0)
  in
  events0, events1, events2
;;

let%test_unit "small random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "rd-arb-small")
    ~sexp_of:
      [%sexp_of: Emitters.Event.t list * Emitters.Event.t list * Emitters.Event.t list]
    ~trials:100
    (access_generator ~writes:5 ~reads0:5 ~reads1:5)
    ~f:(fun (events0, events1, events2) ->
      let mem, model_mem = with_memories () in
      run
        (testbench
           ~mem
           ~model_mem
           ~events0:(Sequence.of_list events0)
           ~events1:(Sequence.of_list events1)
           ~events2:(Sequence.of_list events2)))
;;

let%test_unit "larger random tests" =
  Quickcheck.test
    ~seed:(`Deterministic "rd-arb-large")
    ~sexp_of:
      [%sexp_of: Emitters.Event.t list * Emitters.Event.t list * Emitters.Event.t list]
    ~trials:20
    (access_generator ~writes:250 ~reads0:250 ~reads1:250)
    ~f:(fun (events0, events1, events2) ->
      let mem, model_mem = with_memories () in
      run
        ~timeout:10_000
        (testbench
           ~mem
           ~model_mem
           ~events0:(Sequence.of_list events0)
           ~events1:(Sequence.of_list events1)
           ~events2:(Sequence.of_list events2)))
;;
