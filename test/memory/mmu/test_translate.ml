open! Core
open! Hardcaml
module Dut = Mmu.Translate
open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

module Page_table_memory = struct
  module I = Mmu.Iface.Read_word.From_mem
  module O = Mmu.Iface.Read_word.To_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  let handle ~page_table ~delay_cycles () =
    let rec loop ?(inputs = I.Of_bits.zero ()) () =
      let%bind.Step outs = Step.cycle inputs in
      if Bits.to_bool outs.before_edge.load
      then (
        let%bind.Step () = Step.delay (I.Of_bits.zero ()) ~num_cycles:(delay_cycles ()) in
        let addr = Bits.to_int_trunc outs.before_edge.addr in
        let data = page_table addr in
        loop
          ~inputs:
            { data
            ; addr = Bits.of_int_trunc ~width:Mmu.Iface.addr_width addr
            ; valid = Bits.vdd
            }
          ())
      else loop ()
    in
    loop ()
  ;;

  let spawn ~page_table ~inputs ~outputs ~delay_cycles =
    Step.spawn_io ~inputs ~outputs (fun _ -> handle ~page_table ~delay_cycles ())
  ;;
end

let state ~root ~asid ~mode : Bits.t Mmu.State.t =
  { translation_mode = Mmu.State.Translation_mode.Binary.Of_bits.of_enum mode
  ; asid = Bits.of_int_trunc ~width:Mmu.State.asid_width asid
  ; page_table_root = Bits.of_int_trunc ~width:Mmu.State.addr_width root
  }
;;

let input ~state ~va ~valid : Bits.t Dut.I.t =
  { Step.input_hold with
    state
  ; access_type =
      Mmu.Translate.Access_type.Of_bits.of_enum Mmu.Translate.Access_type.Cases.Load
  ; va =
      { value = Bits.of_int_trunc ~width:Mmu.Iface.addr_width va
      ; valid = Bits.of_bool valid
      }
  }
;;

let check_pa ~mode ~va (output : Step.O_data.t) =
  let output = Step.O_data.before_edge output in
  if not (Bits.to_bool output.result.valid)
  then raise_s [%message "translation did not become valid" (va : int)];
  let actual = Bits.to_int_trunc output.result.pa in
  match mode with
  | Mmu.State.Translation_mode.Cases.Bare | Mmu.State.Translation_mode.Cases.Bare_debug ->
    if not (Int.equal actual va)
    then raise_s [%message "unexpected bare translation" (va : int) (actual : int)]
  | Mmu.State.Translation_mode.Cases.Sv32 ->
    Sample_page_table.check_translation ~va ~actual_pa:actual
;;

let issue_translation ~state ~mode ~va ~delay_cycles =
  let%bind.Step _ = Step.cycle (input ~state ~va ~valid:true) in
  let rec wait_for_result () =
    (* TODO: fails to test back-to-back requests. *)
    let%bind.Step output = Step.cycle (input ~state ~va ~valid:false) in
    if Bits.to_bool (Step.O_data.before_edge output).result.stall
    then wait_for_result ()
    else (
      check_pa ~mode ~va output;
      wait_for_held_result delay_cycles)
  (* Check that we still see result after some delay. *)
  and wait_for_held_result remaining_cycles =
    if Int.equal remaining_cycles 0
    then Step.return ()
    else (
      let%bind.Step output = Step.cycle (input ~state ~va ~valid:false) in
      check_pa ~mode ~va output;
      wait_for_held_result (remaining_cycles - 1))
  in
  wait_for_result ()
;;

let run_test ?(timeout = 200) ?(delay_cycles = fun () -> 0) ~page_table translations =
  run ~create:Dut.create ~timeout (fun () ->
    let%bind.Step _ =
      Page_table_memory.spawn
        ~page_table
        ~delay_cycles
        ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
          { parent with
            walker_from_mem =
              Page_table_memory.Step.merge_inputs ~parent:parent.walker_from_mem ~child
          })
        ~outputs:(fun (p : _ Step.O.t) -> p.walker_to_mem)
    in
    let rec issue_all = function
      | [] -> Step.return ()
      | (state, mode, va, delay_cycles) :: rest ->
        let%bind.Step () = issue_translation ~state ~mode ~va ~delay_cycles in
        issue_all rest
    in
    issue_all translations)
;;

let%test_unit "two-level page-table walk and TLB hit" =
  let vpn = 0x12345 in
  let page_offset0 = 0x34 in
  let page_offset1 = 0xff0 in
  run_test
    ~page_table:Sample_page_table.lookup
    [ ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn lsl 12) lor page_offset0
      , 3 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn lsl 12) lor page_offset1
      , 3 )
    ]
;;

let%test_unit "different ASIDs and direct-map conflicts refill the TLB" =
  let vpn0 = 0x00010 in
  let vpn1 = vpn0 + (1 lsl Mmu.Tlb.bits_index) in
  run_test
    ~page_table:Sample_page_table.lookup
    [ ( state
          ~root:Sample_page_table.root
          ~asid:1
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn0 lsl 12) lor 0x100
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:2
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn0 lsl 12) lor 0x200
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:2
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn1 lsl 12) lor 0x300
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:1
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.State.Translation_mode.Cases.Sv32
      , (vpn0 lsl 12) lor 0x400
      , 2 )
    ]
;;

type translation =
  { mode : Mmu.State.Translation_mode.Cases.t
  ; asid : int
  ; vpn : int
  ; page_offset : int
  ; delay_cycles : int
  }
[@@deriving sexp_of]

let translation_generator =
  let open Quickcheck.Generator.Let_syntax in
  let mode_generator =
    Quickcheck.Generator.weighted_union
      [ 8., Quickcheck.Generator.singleton Mmu.State.Translation_mode.Cases.Sv32
      ; 1., Quickcheck.Generator.singleton Mmu.State.Translation_mode.Cases.Bare
      ; 1., Quickcheck.Generator.singleton Mmu.State.Translation_mode.Cases.Bare_debug
      ]
  in
  let%map mode = mode_generator
  and asid = Int.gen_incl 0 15
  and vpn = Int.gen_incl 0 ((1 lsl Mmu.Iface.vpn_width) - 1)
  and page_offset = Int.gen_incl 0 ((1 lsl Mmu.Iface.page_offset_width) - 1)
  and delay_cycles = Int.gen_incl 0 12 in
  { mode; asid; vpn; page_offset; delay_cycles }
;;

let scenario_generator ~length =
  let open Quickcheck.Generator.Let_syntax in
  let%map translations =
    Quickcheck.Generator.list_with_length length translation_generator
  and delay_cycles = Quickcheck.Generator.list_with_length 20 (Int.gen_incl 0 12) in
  translations, delay_cycles
;;

let cycle_values values =
  let values = Array.of_list values in
  if Array.is_empty values then invalid_arg "cycle_values requires a non-empty list";
  let index = ref 0 in
  fun () ->
    let value = values.(!index) in
    index := (!index + 1) mod Array.length values;
    value
;;

let run_scenario (translations, delay_values) =
  let translations =
    List.map translations ~f:(fun { mode; asid; vpn; page_offset; delay_cycles } ->
      ( state ~root:Sample_page_table.root ~asid ~mode
      , mode
      , (vpn lsl Mmu.Iface.page_offset_width) lor page_offset
      , delay_cycles ))
  in
  run_test
    ~timeout:10_000
    ~delay_cycles:(cycle_values delay_values)
    ~page_table:Sample_page_table.lookup
    translations
;;

let%test_unit "randomized page-table walks with variable memory latency" =
  Quickcheck.test
    ~seed:(`Deterministic "mmu-tlb-walker")
    ~sexp_of:[%sexp_of: translation list * int list]
    ~trials:100
    (scenario_generator ~length:200)
    ~f:run_scenario
;;
