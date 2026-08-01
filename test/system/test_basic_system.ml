open! Core
open! Hardcaml

module System = Riscv_system.System.Make (struct
    module Cpu = struct
      let caches = Riscv_system.Cpu.Cache_config.L2
      let disable_address_translation = true
    end
  end)

module Dut = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; request_interrupt : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { commit_pc : 'a With_valid.t [@bits 32]
      ; register_value : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  let create scope ({ clocking; request_interrupt } : _ I.t) =
    let system = System.create ~scope ~clocking in
    Signal.(System.interrupt system <-- request_interrupt);
    System.attach_bram_memory ~size_bytes:0x8000 system;
    let mmio = System.attach_mmio_register ~addr:0x80000000 system in
    let register_value =
      Types.Clocking.reg clocking ~enable:mmio.write.valid mmio.write.value
    in
    Signal.(mmio.read_value <-- register_value);
    System.complete system;
    let cpu = System.cpu system in
    ({ commit_pc = cpu.commit_pc; register_value } : _ O.t)
  ;;
end

module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

let word_bits value = Bits.of_int32_trunc ~width:32 (Int32.of_string value)

let program =
  Riscvemulate.
    [ Lui { rd = 1; imm = Int32.of_string "0x80000000" }
    ; Lui { rd = 2; imm = Int32.of_string "0x12345000" }
    ; IntImm (Add, { rd = 2; rs1 = 2; imm = Int32.of_string "0x678" })
    ; Store (Word, { rs1 = 1; rs2 = 2; imm = Int32.zero })
    ; Load (Word, Unsigned, { rd = 3; rs1 = 1; imm = Int32.zero })
    ; Lui { rd = 2; imm = Int32.of_string "0xdeadc000" }
    ; IntImm (Add, { rd = 2; rs1 = 2; imm = Int32.of_string "-0x111" })
    ; Store (Word, { rs1 = 1; rs2 = 2; imm = Int32.zero })
    ; Load (Word, Unsigned, { rd = 4; rs1 = 1; imm = Int32.zero })
    ]
;;

let preload_program sim memory =
  Hashtbl.iter_keys memory ~f:(fun addr ->
    if Int32.(addr < zero || addr >= of_int_exn 0x8000)
    then failwithf "program address outside BRAM: 0x%lx" addr ());
  let words = Int.Table.create () in
  Hashtbl.iteri memory ~f:(fun ~key:addr ~data:byte ->
    let addr = Int32.to_int_exn addr in
    let word_address = addr / 8 in
    let shift = 8 * (addr % 8) in
    Hashtbl.update words word_address ~f:(fun current ->
      let current = Option.value current ~default:0L in
      Int64.(current lor shift_left (of_int byte) shift)));
  let main_memory =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn
  in
  Hashtbl.iteri words ~f:(fun ~key:address ~data ->
    Cyclesim.Memory.of_bits main_memory ~address (Bits.of_int64_trunc ~width:64 data))
;;

let read_registers sim =
  Cyclesim.lookup_mem_by_name sim "regfile"
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int32_trunc
;;

let run_program (sim : Sim.t) ~insn_count =
  let commits = ref 0 in
  let rec loop cycles =
    if !commits = insn_count
    then ()
    else if cycles = 0
    then failwithf "system program timed out after %d cycles" 5_000 ()
    else (
      let outputs = Cyclesim.outputs sim in
      if Bits.to_bool !(outputs.commit_pc.valid) then Int.incr commits;
      Cyclesim.cycle sim;
      loop (cycles - 1))
  in
  loop 5_000
;;

let%test_unit "system routes BRAM and MMIO accesses" =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:(Cyclesim.Config.trace `All_named) (Dut.create scope) in
  let inputs = Cyclesim.inputs sim in
  inputs.request_interrupt := Bits.gnd;
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  let memory = Riscvemulate.init ~insns:program ~addr:Int32.zero |> Riscvemulate.memory in
  preload_program sim memory;
  run_program sim ~insn_count:(List.length program);
  let registers = read_registers sim in
  let outputs = Cyclesim.outputs sim in
  if not (Int32.equal registers.(3) (Int32.of_string "0x12345678"))
  then failwithf "first MMIO read returned 0x%lx" registers.(3) ();
  if not (Int32.equal registers.(4) (Int32.of_string "0xdeadbeef"))
  then failwithf "second MMIO read returned 0x%lx" registers.(4) ();
  if not (Bits.equal !(outputs.register_value) (word_bits "0xdeadbeef"))
  then
    failwithf
      "MMIO register ended at 0x%lx"
      (Bits.to_int32_trunc !(outputs.register_value))
      ()
;;
