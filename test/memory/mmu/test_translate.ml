open! Core
open! Hardcaml
module Dut = Mmu.Translate
open Hardcaml_test_harness.Step_harness.Functional.Make_monadic (Dut.I) (Dut.O)

module Page_table_memory = struct
  module I = Memory.Bus.From_mem
  module O = Memory.Bus.To_mem
  module Step = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O)

  let handle ~page_table ~delay_cycles () =
    let rec loop ?(inputs = I.Of_bits.zero ()) () =
      let%bind.Step outs = Step.cycle inputs in
      if Bits.to_bool outs.before_edge.valid
         && Bits.to_bool outs.before_edge.access_type.read_word
      then (
        let%bind.Step () = Step.delay (I.Of_bits.zero ()) ~num_cycles:(delay_cycles ()) in
        let addr = Bits.to_unsigned_int outs.before_edge.addr in
        let data = page_table addr in
        loop
          ~inputs:
            { data = Bits.uresize data ~width:Memory.Bus.data_width
            ; addr = Bits.of_unsigned_int ~width:Mmu.Iface.addr_width addr
            ; valid = Bits.vdd
            ; last = Bits.vdd
            ; ready = Bits.vdd
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

let state ~root ~asid ~mode ~priv ~fetch_priv : Bits.t Mmu.State.t =
  { translation_mode = Mmu.State.Translation_mode.Binary.Of_bits.of_enum mode
  ; asid = Bits.of_unsigned_int ~width:Mmu.State.asid_width asid
  ; page_table_root = Bits.of_unsigned_int ~width:Mmu.State.addr_width root
  ; fetch_priv = Bits.of_unsigned_int ~width:2 fetch_priv
  ; load_store_priv = Bits.of_unsigned_int ~width:2 priv
  ; executable_readable = Bits.gnd
  ; supervisor_user_access = Bits.gnd
  }
;;

let input ~state ~access_type ~va ~valid : Bits.t Dut.I.t =
  { Step.input_hold with
    state
  ; access_type = Mmu.Translate.Access_type.Of_bits.of_enum access_type
  ; va =
      { value = Bits.of_unsigned_int ~width:Mmu.Iface.addr_width va
      ; valid = Bits.of_bool valid
      }
  }
;;

let effective_privilege (state : Bits.t Mmu.State.t) access_type =
  match access_type with
  | Mmu.Translate.Access_type.Cases.Instruction -> Bits.to_unsigned_int state.fetch_priv
  | Load | Store -> Bits.to_unsigned_int state.load_store_priv
;;

let check_result ~state ~access_type ~mode ~va (output : Step.O_data.t) =
  let output = Step.O_data.before_edge output in
  let actual_valid = Bits.to_bool output.result.valid in
  let actual_fault = Bits.to_bool output.result.fault in
  let expected_fault =
    match mode with
    | Mmu.State.Translation_mode.Cases.Bare | Bare_debug -> false
    | Sv32 ->
      Sample_page_table.access_fault
        ~va
        ~access_type
        ~effective_priv:(effective_privilege state access_type)
        ~supervisor_user_access:(Bits.to_bool state.supervisor_user_access)
  in
  if Bool.equal actual_fault expected_fault |> not
     || Bool.equal actual_valid (not expected_fault) |> not
  then
    raise_s
      [%message
        "unexpected translation status"
          (va : int)
          (access_type : Mmu.Translate.Access_type.Cases.t)
          (actual_valid : bool)
          (actual_fault : bool)
          (expected_fault : bool)];
  if not expected_fault
  then (
    let actual = Bits.to_unsigned_int output.result.pa in
    match mode with
    | Mmu.State.Translation_mode.Cases.Bare | Bare_debug ->
      if not (Int.equal actual va)
      then raise_s [%message "unexpected bare translation" (va : int) (actual : int)]
    | Sv32 -> Sample_page_table.check_translation ~va ~actual_pa:actual)
;;

let check_stalling_result (output : Step.O_data.t) =
  let result = (Step.O_data.before_edge output).result in
  if Bits.to_bool result.valid || Bits.to_bool result.fault
  then
    raise_s
      [%message
        "translation status was not mutually exclusive"
          (result.valid : Bits.t)
          (result.fault : Bits.t)
          (result.stall : Bits.t)]
;;

let issue_translation ~state ~access_type ~mode ~va ~delay_cycles =
  let%bind.Step _ = Step.cycle (input ~state ~access_type ~va ~valid:true) in
  let rec wait_for_result () =
    (* TODO: fails to test back-to-back requests. *)
    let%bind.Step output = Step.cycle (input ~state ~access_type ~va ~valid:false) in
    if Bits.to_bool (Step.O_data.before_edge output).result.stall
    then (
      check_stalling_result output;
      wait_for_result ())
    else (
      check_result ~state ~access_type ~mode ~va output;
      wait_for_held_result delay_cycles)
  (* Check that we still see result after some delay. *)
  and wait_for_held_result remaining_cycles =
    if Int.equal remaining_cycles 0
    then Step.return ()
    else (
      let%bind.Step output = Step.cycle (input ~state ~access_type ~va ~valid:false) in
      check_result ~state ~access_type ~mode ~va output;
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
      | (state, mode, access_type, va, delay_cycles) :: rest ->
        let%bind.Step () =
          issue_translation ~state ~access_type ~mode ~va ~delay_cycles
        in
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
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn lsl 12) lor page_offset0
      , 3 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn lsl 12) lor page_offset1
      , 3 )
    ]
;;

let%test_unit "effective privilege controls Sv32 translation" =
  let va = (0x12345 lsl 12) lor 0x34 in
  run_test
    ~page_table:Sample_page_table.lookup
    [ ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , va
      , 0 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:1
          ~fetch_priv:1
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , va
      , 0 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:3
          ~fetch_priv:3
      , Mmu.State.Translation_mode.Cases.Bare
      , Mmu.Translate.Access_type.Cases.Load
      , va
      , 0 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:3
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , va
      , 0 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:3
      , Mmu.State.Translation_mode.Cases.Bare
      , Mmu.Translate.Access_type.Cases.Instruction
      , va
      , 0 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:7
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:3
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Instruction
      , va
      , 0 )
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
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn0 lsl 12) lor 0x100
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:2
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn0 lsl 12) lor 0x200
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:2
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn1 lsl 12) lor 0x300
      , 2 )
    ; ( state
          ~root:Sample_page_table.root
          ~asid:1
          ~mode:Mmu.State.Translation_mode.Cases.Sv32
          ~priv:0
          ~fetch_priv:0
      , Mmu.State.Translation_mode.Cases.Sv32
      , Mmu.Translate.Access_type.Cases.Load
      , (vpn0 lsl 12) lor 0x400
      , 2 )
    ]
;;

type translation =
  { mode : Mmu.State.Translation_mode.Cases.t
  ; asid : int
  ; priv : int
  ; fetch_priv : int
  ; access_type : Mmu.Translate.Access_type.Cases.t
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
  and priv = Quickcheck.Generator.of_list [ 0; 1; 3 ]
  and fetch_priv = Quickcheck.Generator.of_list [ 0; 1; 3 ]
  and access_type =
    Quickcheck.Generator.of_list
      [ Mmu.Translate.Access_type.Cases.Load
      ; Mmu.Translate.Access_type.Cases.Store
      ; Mmu.Translate.Access_type.Cases.Instruction
      ]
  and vpn = Int.gen_incl 0 ((1 lsl Mmu.Iface.vpn_width) - 1)
  and page_offset = Int.gen_incl 0 ((1 lsl Mmu.Iface.page_offset_width) - 1)
  and delay_cycles = Int.gen_incl 0 12 in
  { mode; asid; priv; fetch_priv; access_type; vpn; page_offset; delay_cycles }
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
    List.map
      translations
      ~f:
        (fun
          { mode; asid; priv; fetch_priv; access_type; vpn; page_offset; delay_cycles } ->
        let effective_priv =
          match access_type with
          | Mmu.Translate.Access_type.Cases.Instruction -> fetch_priv
          | Load | Store -> priv
        in
        let expected_mode =
          match mode with
          | Mmu.State.Translation_mode.Cases.Sv32 when effective_priv land 2 <> 0 ->
            Mmu.State.Translation_mode.Cases.Bare
          | mode -> mode
        in
        ( state ~root:Sample_page_table.root ~asid ~mode ~priv ~fetch_priv
        , expected_mode
        , access_type
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
  let normal_permission_coverage = Int.Hash_set.create () in
  let superpage_permission_coverage = Int.Hash_set.create () in
  let saw_invalid_nonleaf_encoding = ref false in
  let saw_invalid_leaf_encoding = ref false in
  let saw_forbidden_write = ref false in
  let record_coverage
    { mode; priv; fetch_priv; access_type; vpn; asid = _; page_offset = _; delay_cycles = _ }
    =
    let effective_priv =
      match access_type with
      | Mmu.Translate.Access_type.Cases.Instruction -> fetch_priv
      | Load | Store -> priv
    in
    if Mmu.State.Translation_mode.Cases.compare mode Sv32 = 0 && effective_priv land 2 = 0
    then (
      let permissions = Sample_page_table.permissions vpn in
      let permission_bits = Sample_page_table.permission_bits permissions in
      Hash_set.add
        (if Sample_page_table.is_superpage vpn
         then superpage_permission_coverage
         else normal_permission_coverage)
        permission_bits;
      if not permissions.valid
      then (
        let rwx = permission_bits land 7 in
        saw_invalid_nonleaf_encoding := !saw_invalid_nonleaf_encoding || rwx = 0;
        saw_invalid_leaf_encoding := !saw_invalid_leaf_encoding || rwx = 7);
      saw_forbidden_write
      := !saw_forbidden_write || (permissions.write && not permissions.read))
  in
  Quickcheck.test
    ~seed:(`Deterministic "mmu-tlb-walker")
    ~sexp_of:[%sexp_of: translation list * int list]
    ~trials:100
    (scenario_generator ~length:200)
    ~f:(fun ((translations, _) as scenario) ->
      List.iter translations ~f:record_coverage;
      run_scenario scenario);
  let require_permissions name coverage expected =
    let missing = List.filter expected ~f:(Fn.non (Hash_set.mem coverage)) in
    if not (List.is_empty missing)
    then raise_s [%message "randomized MMU permission coverage incomplete" name (missing : int list)]
  in
  require_permissions "normal pages" normal_permission_coverage (List.init 16 ~f:Fn.id);
  require_permissions
    "superpages"
    superpage_permission_coverage
    (List.filter (List.init 16 ~f:Fn.id) ~f:(fun permission -> permission land 7 <> 0));
  if not (!saw_invalid_nonleaf_encoding && !saw_invalid_leaf_encoding)
  then
    raise_s
      [%message
        "randomized MMU invalid-PTE coverage incomplete"
          (!saw_invalid_nonleaf_encoding : bool)
          (!saw_invalid_leaf_encoding : bool)];
  if not !saw_forbidden_write
  then raise_s [%message "randomized MMU did not cover W=1,R=0"]
;;
