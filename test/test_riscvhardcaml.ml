
open! Base
open! Hardcaml

(* Run a basic program *)
let program = Riscvemulate.[
  IntImm (Add, {rd = 1; rs1 = 0; imm = Int32.of_int_exn 7});
  IntReg (Add, {rd = 2; rs1 = 1; rs2 = 1});
]
let emulator = Riscvemulate.init ~insns:program ~addr:Int32.zero
let sim_cpu = Sim.Cpu.create ()
let sim_mem = Hashtbl.copy emulator.memory

(* Run for 2 cycles and check regs *)
let regs = Array.create ~len:32 Int32.zero
let _ = Sim.Cpu.cycle_insn sim_cpu sim_mem; regs.(1) <- Int32.of_int_exn 7
let _ = if Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) regs
          then Stdio.print_string "Basic program: cycle 1 good\n"
          else failwith "Basic cycle 1 failed"
let _ = Sim.Cpu.cycle_insn sim_cpu sim_mem; regs.(2) <- Int32.of_int_exn 14
let _ = if Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) regs
          then Stdio.print_string "Basic program: cycle 2 good\n"
          else failwith "Basic cycle 2 failed"


(* Compare a random execution to the emulator for the given number of instructions.
Generates instructions on-the-fly to account for randomly branching around. *)
let compare_emulator ~insn_count ~mem_range ~reg_max =
  (* Init *)
  let emulator = Riscvemulate.blank in
  let sim_cpu = Sim.Cpu.create () in
  let sim_mem = Hashtbl.copy emulator.memory in

  (* Run *)
  for i = 0 to insn_count do
    Stdio.printf "\n\n== CYCLE %d ==\n" i;
    (* Step simulator and emulator *)
    (try
      (* Each cycle, inject instruction at simulator's PC (due to pipelining, simulator PC runs ahead of emulation) *)
      Sim.Cpu.cycle_insn ~f:(fun _ ->
        let new_insn = Riscvemulate.Random.instruction ~mem_range ~reg_max () in
        Stdio.printf "writing "; Stdio.print_s (Riscvemulate.sexp_of_insn new_insn);
        Stdio.printf "(binary: %08x) to PC %d\n" (Riscvemulate.to_int32 new_insn |> Int32.to_int_exn) (Int32.to_int_exn (Sim.Cpu.pc sim_cpu));
        (* Injection happens for both simulator and emulator, while simulator fetch runs ahead. Fine as long as
        no code modifies instructions in near (couple cycle) future, which is definitely possible so TODO fix this. *)
        List.iter [sim_mem; emulator.memory] ~f:(fun memory ->
          Riscvemulate.store ~memory ~addr:(Sim.Cpu.pc sim_cpu) ~size:4 ~value:(Riscvemulate.to_int32 new_insn)
        );
      ) sim_cpu sim_mem;
      (* And step the emulator forward an instruction as well *)
      Riscvemulate.step emulator
    with
      | err -> Stdio.print_s (Riscvemulate.sexp_of_insn (Riscvemulate.current_pc_insn emulator)); raise err
    );

    if not (Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) emulator.regs) then (
      Stdio.printf "Mismatch on cycle %d:\n" i;
      Stdio.printf "Insn: "; Stdio.print_s (Riscvemulate.sexp_of_insn (Riscvemulate.current_pc_insn emulator));
      Stdio.printf "Emulator state: "; Stdio.print_s (Riscvemulate.sexp_of_state emulator);
      Stdio.printf "HW regs: "; Stdio.print_s (sexp_of_array sexp_of_int32 (Sim.Cpu.regs sim_cpu));
      Stdio.printf "HW mem: "; Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int sim_mem);
      failwith "mismatch between emulator and simulation"
    );
  done;
  (* TODO: flush remaining insns in sim? actually no, issue is in other direction *)
  if not (Hashtbl.is_empty (Hashtbl.merge emulator.memory sim_mem ~f:(fun ~key -> function
    | `Both (a,b) -> if a <> b then Some key else None
    | `Left a -> if a <> 0 then Some key else None
    | `Right b -> if 0 <> b then Some key else None
  ))) then
    failwith "memory different at end"

(* Test! *)
let _ = Random.init 1
(* mem_range being small risks stores overwriting instructions, which causes emulator mismatch if within pipeline already *)
let _ = compare_emulator ~insn_count:1000 ~mem_range:Int32.(of_int_exn 1000, of_int_exn 1024) ~reg_max:8
