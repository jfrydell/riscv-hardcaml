open! Core
open! Hardcaml

module Test = Test_definitions.Fencei

let emulator = Riscvemulate.State.init ~insns:Test.program ~addr:Int32.zero
let sim = Sim.Cpu.create ~memory:(Hashtbl.copy emulator.memory) No_waves

let () =
  let regfile = Cyclesim.lookup_mem_by_name sim.sim "regfile" |> Option.value_exn in
  List.iter Test.initial_registers ~f:(fun (address, value) ->
    Cyclesim.Memory.of_bits
      regfile
      ~address
      (Bits.of_int32_trunc ~width:32 value))
;;

let committed_pcs = ref []

let rec run cycles =
  if List.length !committed_pcs = 3
  then ()
  else if cycles = 0
  then failwith "Fence.i test timed out"
  else (
    Option.iter (Sim.Cpu.commit_pc sim) ~f:(fun pc -> committed_pcs := pc :: !committed_pcs);
    Sim.Cpu.cycle sim;
    run (cycles - 1))
;;

let () =
  run 5_000;
  let committed_pcs = List.rev !committed_pcs in
  if not (List.equal Int32.equal committed_pcs [ Int32.zero; Int32.of_int_exn 4; Int32.of_int_exn 8 ])
  then raise_s [%message "Fence.i committed unexpected PCs" (committed_pcs : int32 list)];
  let regs = Sim.Cpu.regs sim in
  if not (Int32.equal regs.(3) (Int32.of_int_exn 42))
  then failwithf "Fence.i did not execute the updated instruction: x3 = %lx" regs.(3) ();
  Stdio.print_string "Fence.i test: good\n"
;;
