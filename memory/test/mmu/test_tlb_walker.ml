open! Core
open! Hardcaml

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; state : 'a Mmu.State.t
      ; va : 'a With_valid.t [@bits Mmu.Iface.addr_width]
      ; read_from_mem : 'a Mmu.Iface.Read_word.From_mem.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { pa : 'a With_valid.t [@bits Mmu.Iface.addr_width]
      ; read_to_mem : 'a Mmu.Iface.Read_word.To_mem.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; state; va; read_from_mem } : _ I.t) =
    let%hw.Mmu.Iface.Tlb_response.Of_signal walker_to_tlb =
      Mmu.Iface.Tlb_response.Of_signal.wires ()
    in
    let%hw.Mmu.Iface.Tlb_request.Of_signal tlb_to_walker =
      Mmu.Iface.Tlb_request.Of_signal.wires ()
    in
    let%hw.Mmu.Tlb.O.Of_signal tlb =
      Mmu.Tlb.hierarchical ~scope { clocking; state; va; from_walker = walker_to_tlb }
    in
    let%hw.Mmu.Walker.O.Of_signal walker =
      Mmu.Walker.hierarchical
        ~scope
        { clocking; state; from_tlb = tlb_to_walker; read_from_mem }
    in
    Mmu.Iface.Tlb_request.Of_signal.assign tlb_to_walker tlb.to_walker;
    Mmu.Iface.Tlb_response.Of_signal.assign walker_to_tlb walker.to_tlb;
    ({ pa = tlb.pa; read_to_mem = walker.read_to_mem } : _ O.t)
  ;;
end

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

let state ~root ~asid : Bits.t Mmu.State.t =
  { translation_mode =
      Mmu.State.Translation_mode.Binary.Of_bits.of_enum
        Mmu.State.Translation_mode.Cases.None
  ; asid = Bits.of_int_trunc ~width:Mmu.State.asid_width asid
  ; page_table_root = Bits.of_int_trunc ~width:Mmu.State.addr_width root
  }
;;

let input ~state ~va ~valid : Bits.t Dut.I.t =
  { Step.input_hold with
    state
  ; va =
      { value = Bits.of_int_trunc ~width:Mmu.Iface.addr_width va
      ; valid = Bits.of_bool valid
      }
  }
;;

let check_pa ~va (output : Step.O_data.t) =
  let output = Step.O_data.before_edge output in
  if not (Bits.to_bool output.pa.valid)
  then raise_s [%message "translation did not become valid" (va : int)];
  let actual = Bits.to_int_trunc output.pa.value in
  Sample_page_table.check_translation ~va ~actual_pa:actual
;;

let issue_translation ~state ~va =
  let%bind.Step _ = Step.cycle (input ~state ~va ~valid:true) in
  let rec wait_for_result () =
    let%bind.Step output = Step.cycle (input ~state ~va ~valid:false) in
    if Bits.to_bool (Step.O_data.before_edge output).pa.valid
    then (
      check_pa ~va output;
      Step.return ())
    else wait_for_result ()
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
            read_from_mem =
              Page_table_memory.Step.merge_inputs ~parent:parent.read_from_mem ~child
          })
        ~outputs:(fun (p : _ Step.O.t) -> p.read_to_mem)
    in
    let rec issue_all = function
      | [] -> Step.return ()
      | (state, va) :: rest ->
        let%bind.Step () = issue_translation ~state ~va in
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
    [ state ~root:Sample_page_table.root ~asid:7, (vpn lsl 12) lor page_offset0
    ; state ~root:Sample_page_table.root ~asid:7, (vpn lsl 12) lor page_offset1
    ]
;;

let%test_unit "different ASIDs and direct-map conflicts refill the TLB" =
  let vpn0 = 0x00010 in
  let vpn1 = vpn0 + (1 lsl Mmu.Tlb.bits_index) in
  run_test
    ~page_table:Sample_page_table.lookup
    [ state ~root:Sample_page_table.root ~asid:1, (vpn0 lsl 12) lor 0x100
    ; state ~root:Sample_page_table.root ~asid:2, (vpn0 lsl 12) lor 0x200
    ; state ~root:Sample_page_table.root ~asid:2, (vpn1 lsl 12) lor 0x300
    ; state ~root:Sample_page_table.root ~asid:1, (vpn0 lsl 12) lor 0x400
    ]
;;

type translation =
  { asid : int
  ; vpn : int
  ; page_offset : int
  }
[@@deriving sexp_of]

let translation_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map asid = Int.gen_incl 0 15
  and vpn = Int.gen_incl 0 ((1 lsl Mmu.Iface.vpn_width) - 1)
  and page_offset = Int.gen_incl 0 ((1 lsl Mmu.Iface.page_offset_width) - 1) in
  { asid; vpn; page_offset }
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
    List.map translations ~f:(fun { asid; vpn; page_offset } ->
      ( state ~root:Sample_page_table.root ~asid
      , (vpn lsl Mmu.Iface.page_offset_width) lor page_offset ))
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
