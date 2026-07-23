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

  let handle ~page_table () =
    let rec loop () =
      let%bind.Step outs = Step.cycle (I.Of_bits.zero ()) in
      if Bits.to_bool outs.before_edge.load
      then (
        let addr = Bits.to_int_trunc outs.before_edge.addr in
        let data = Hashtbl.find_exn page_table addr in
        let%bind.Step _ =
          Step.cycle
            { data
            ; addr = Bits.of_int_trunc ~width:Mmu.Iface.addr_width addr
            ; valid = Bits.vdd
            }
        in
        loop ())
      else loop ()
    in
    loop ()
  ;;

  let spawn ~page_table ~inputs ~outputs =
    Step.spawn_io ~inputs ~outputs (fun _ -> handle ~page_table ())
  ;;
end

let pte ~ppn ~read ~write ~execute ~user ~global =
  let flags =
    1
    lor (read lsl 1)
    lor (write lsl 2)
    lor (execute lsl 3)
    lor (user lsl 4)
    lor (global lsl 5)
  in
  Bits.of_int_trunc ~width:Mmu.Iface.addr_width ((ppn lsl 10) lor flags)
;;

let add_mapping ~page_table ~root ~vpn ~ppn ~user ~global =
  let root_index = vpn lsr 10 in
  let leaf_index = vpn land 0x3ff in
  let second_level_base = 0x2000 + (root_index lsl 12) in
  Hashtbl.set
    page_table
    ~key:(root + (root_index lsl 2))
    ~data:
      (pte ~ppn:(second_level_base lsr 12) ~read:0 ~write:0 ~execute:0 ~user:0 ~global:0);
  Hashtbl.set
    page_table
    ~key:(second_level_base + (leaf_index lsl 2))
    ~data:(pte ~ppn ~read:1 ~write:1 ~execute:1 ~user ~global)
;;

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

let check_pa ~va ~expected (output : Step.O_data.t) =
  let output = Step.O_data.before_edge output in
  if not (Bits.to_bool output.pa.valid)
  then raise_s [%message "translation did not become valid" (va : int)];
  let actual = Bits.to_int_trunc output.pa.value in
  if not (Int.equal actual expected)
  then
    raise_s [%message "unexpected translation" (va : int) (actual : int) (expected : int)]
;;

let issue_translation ~state ~va ~expected =
  let%bind.Step _ = Step.cycle (input ~state ~va ~valid:true) in
  let rec wait_for_result () =
    let%bind.Step output = Step.cycle (input ~state ~va ~valid:false) in
    if Bits.to_bool (Step.O_data.before_edge output).pa.valid
    then (
      check_pa ~va ~expected output;
      Step.return ())
    else wait_for_result ()
  in
  wait_for_result ()
;;

let run_test ~page_table translations =
  run ~create:Dut.create ~timeout:200 (fun () ->
    let%bind.Step _ =
      Page_table_memory.spawn
        ~page_table
        ~inputs:(fun ~(parent : _ Step.I.t) ~child ->
          { parent with
            read_from_mem =
              Page_table_memory.Step.merge_inputs ~parent:parent.read_from_mem ~child
          })
        ~outputs:(fun (p : _ Step.O.t) -> p.read_to_mem)
    in
    let rec issue_all = function
      | [] -> Step.return ()
      | (state, va, expected) :: rest ->
        let%bind.Step () = issue_translation ~state ~va ~expected in
        issue_all rest
    in
    issue_all translations)
;;

let%test_unit "two-level page-table walk and TLB hit" =
  let root = 0x1000 in
  let vpn = 0x12345 in
  let page_offset0 = 0x34 in
  let page_offset1 = 0xff0 in
  let physical_ppn = 0xabc in
  let page_table = Int.Table.create () in
  add_mapping ~page_table ~root ~vpn ~ppn:physical_ppn ~user:1 ~global:0;
  run_test
    ~page_table
    [ ( state ~root ~asid:7
      , (vpn lsl 12) lor page_offset0
      , (physical_ppn lsl 12) lor page_offset0 )
    ; ( state ~root ~asid:7
      , (vpn lsl 12) lor page_offset1
      , (physical_ppn lsl 12) lor page_offset1 )
    ]
;;

let%test_unit "different ASIDs and direct-map conflicts refill the TLB" =
  let root = 0x1000 in
  let vpn0 = 0x00010 in
  let vpn1 = vpn0 + (1 lsl Mmu.Tlb.bits_index) in
  let page_table = Int.Table.create () in
  add_mapping ~page_table ~root ~vpn:vpn0 ~ppn:0x321 ~user:0 ~global:0;
  add_mapping ~page_table ~root ~vpn:vpn1 ~ppn:0x654 ~user:0 ~global:0;
  run_test
    ~page_table
    [ state ~root ~asid:1, (vpn0 lsl 12) lor 0x100, (0x321 lsl 12) lor 0x100
    ; state ~root ~asid:2, (vpn0 lsl 12) lor 0x200, (0x321 lsl 12) lor 0x200
    ; state ~root ~asid:2, (vpn1 lsl 12) lor 0x300, (0x654 lsl 12) lor 0x300
    ; state ~root ~asid:1, (vpn0 lsl 12) lor 0x400, (0x321 lsl 12) lor 0x400
    ]
;;
