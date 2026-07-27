open! Core
open! Hardcaml

module Pass_through = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; to_mem : 'a Memory.Bus.To_mem.t
      ; from_mem : 'a Memory.Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out_to_mem : 'a Memory.Bus.To_mem.t
      ; out_from_mem : 'a Memory.Bus.From_mem.t
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clocking = _; to_mem; from_mem } : _ I.t) : _ O.t =
    { out_to_mem = to_mem; out_from_mem = from_mem }
  ;;
end

module Sim = Cyclesim.With_interface (Pass_through.I) (Pass_through.O)

module Step =
  Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make
    (Pass_through.I)
    (Pass_through.O)

let create_simulator () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Pass_through.create scope)
;;

let run_testbench testbench =
  match
    Step.run_with_timeout
      ~input_default:Step.input_zero
      ~timeout:200
      ()
      ~simulator:(create_simulator ())
      ~testbench
  with
  | Some result -> result
  | None -> failwith "testbench timed out"
;;

let bits_of_hex hex = Bits.of_hex ~width:Memory.Bus.cpu_bus_width hex

let require_mem_word mem ~addr ~expected =
  let actual = Hashtbl.find_exn mem addr in
  if not (Bits.equal actual expected)
  then
    raise_s
      [%message
        "unexpected memory word"
          (addr : int)
          (actual : Bits.Hex.t)
          (expected : Bits.Hex.t)]
;;

let require_same_memory expected actual =
  let keys =
    Hash_set.to_list
      (Hash_set.of_list
         (module Int)
         (List.append (Hashtbl.keys expected) (Hashtbl.keys actual)))
    |> List.sort ~compare:Int.compare
  in
  List.iter keys ~f:(fun addr ->
    let expected =
      Hashtbl.find expected addr
      |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
    in
    let actual =
      Hashtbl.find actual addr
      |> Option.value ~default:(Bits.zero Memory.Bus.cpu_bus_width)
    in
    if not (Bits.equal expected actual)
    then
      raise_s
        [%message
          "memory mismatch" (addr : int) (expected : Bits.Hex.t) (actual : Bits.Hex.t)])
;;

let spawn_handler ~mem ~delay_cycles =
  Handlers.spawn
    ~mem
    ~delay_cycles
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with from_mem = Handlers.merge_inputs ~parent:parent.from_mem ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.out_to_mem)
;;

let spawn_emitter ?model_mem ~events () =
  Emitters.spawn
    ?model_mem
    ~events
    ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
      { parent with to_mem = Emitters.merge_inputs ~parent:parent.to_mem ~child })
    ~outputs:(fun (p : _ Step.O.t) -> p.out_from_mem)
    ()
;;

let%test_unit "write-through emitter reaches handler and updates memory" =
  let mem = Int.Table.create () in
  let model_mem = Int.Table.create () in
  Hashtbl.set mem ~key:0x100 ~data:(bits_of_hex "1122334455667788");
  Hashtbl.set model_mem ~key:0x100 ~data:(bits_of_hex "1122334455667788");
  run_testbench (fun _ ->
    let%bind.Step _ = spawn_handler ~mem ~delay_cycles:(fun () -> 2) in
    let%bind.Step writer =
      spawn_emitter
        ~model_mem
        ~events:
          (Sequence.of_list
             [ Emitters.Event.Write_through
                 { addr = 0x101; data = bits_of_hex "00000000000000aa"; size = 0 }
             ; Delay 1
             ; Write_through
                 { addr = 0x104; data = bits_of_hex "000000000000beef"; size = 1 }
             ; Write_through
                 { addr = 0x100; data = bits_of_hex "00000000deadbeef"; size = 2 }
             ])
        ()
    in
    Step.wait_for writer);
  require_mem_word mem ~addr:0x100 ~expected:(bits_of_hex "1122beefdeadbeef");
  require_same_memory model_mem mem
;;

let%test_unit "read-block emitter receives streamed block from handler" =
  let mem = Int.Table.create () in
  let model_mem = Int.Table.create () in
  let base_addr = 0x200 in
  let word_size = Memory.Bus.cpu_bus_width / 8 in
  let words =
    [ bits_of_hex "0123456789abcdef"
    ; bits_of_hex "1111222233334444"
    ; bits_of_hex "5555666677778888"
    ; bits_of_hex "9999aaaabbbbcccc"
    ]
  in
  List.iteri words ~f:(fun index data ->
    Hashtbl.set mem ~key:(base_addr + (index * word_size)) ~data);
  List.iteri words ~f:(fun index data ->
    Hashtbl.set model_mem ~key:(base_addr + (index * word_size)) ~data);
  run_testbench (fun _ ->
    let%bind.Step _ = spawn_handler ~mem ~delay_cycles:(fun () -> 1) in
    let%bind.Step reader =
      spawn_emitter
        ~model_mem
        ~events:(Sequence.of_list [ Emitters.Event.Read_block { addr = base_addr + 4 } ])
        ()
    in
    Step.wait_for reader)
;;

let%test_unit "unified emitter and handler support word reads and write-backs" =
  let mem = Int.Table.create () in
  let model_mem = Int.Table.create () in
  Hashtbl.set mem ~key:0x200 ~data:(bits_of_hex "1122334455667788");
  Hashtbl.set model_mem ~key:0x200 ~data:(bits_of_hex "1122334455667788");
  run_testbench (fun _ ->
    let%bind.Step _ = spawn_handler ~mem ~delay_cycles:(fun () -> 0) in
    let%bind.Step emitter =
      spawn_emitter
        ~model_mem
        ~events:
          (Sequence.of_list
             [ Emitters.Event.Write_back
                 { addr = 0x208; data = bits_of_hex "0123456789abcdef"; last = true }
             ; Read_word { addr = 0x204 }
             ])
        ()
    in
    Step.wait_for emitter);
  require_mem_word mem ~addr:0x208 ~expected:(bits_of_hex "0123456789abcdef");
  require_same_memory model_mem mem
;;
