
open! Base
open! Hardcaml

(* Compare a random execution to the emulator for the given number of instructions.
Generates instructions on-the-fly to account for randomly branching around. *)
let compare_emulator ~insn_count ~mem_range ~reg_max =
  (* Init *)
  let emulator = Riscvemulate.blank () in
  let sim_cpu = Sim.Cpu.create () in
  let sim_mem = Hashtbl.copy emulator.memory in
  let debug = false in

  (* Run *)
  for i = 1 to insn_count do
    if debug then (Stdio.printf "\n\n== CYCLE %d ==\n" i);

    (* Run simulator, injecting instructions as it fetches (due to pipelining, simulator PC runs ahead of emulation) *)
    Sim.Cpu.cycle_insn ~f:(fun _ ->
      (* If branch to near current PC, we may overwrite instruction before it commits and emulator executes it *)
      let new_insn = Riscvemulate.Random.instruction ~mem_range ~reg_max
                      ~branch_cond:(fun offset -> Int32.(abs offset > of_int_exn 20)) () in
      if debug then (
        Stdio.printf "Writing "; Stdio.print_s (Riscvemulate.sexp_of_insn new_insn);
        Stdio.printf "(binary: %08x) to PC %d (fetched by sim)\n"
          (Riscvemulate.to_int32 new_insn |> Int32.to_int_exn) (Int32.to_int_exn (Sim.Cpu.pc sim_cpu));
      );
      (* Injection happens for both simulator and emulator, while simulator fetch runs ahead. Fine as long as no
      code modifies instructions in near (couple cycle) future, which is unlikely if for random programs are not
      near zero (would need reg to collide with PC - mem offset); this is why we bias mem offset away from 0. *)
      List.iter [sim_mem; emulator.memory] ~f:(fun memory ->
        Riscvemulate.store ~memory ~addr:(Sim.Cpu.pc sim_cpu) ~size:4 ~value:(Riscvemulate.to_int32 new_insn)
      );
    ) sim_cpu sim_mem;

    (* Output instruction we are executing this cycle for debugging *)
    if debug then (
      let pc = !(emulator.pc) in
      let insn = Riscvemulate.load ~memory:emulator.memory ~addr:pc ~size:4 ~extend:Unsigned in
      Stdio.printf "\nStepping emulator through PC %d = insn %08x\n" (Int32.to_int_exn pc) (Int32.to_int_exn insn);
      Stdio.print_s (Riscvemulate.sexp_of_insn (Riscvemulate.current_pc_insn emulator));
    );

    (* And step the emulator forward an instruction as well *)
    Riscvemulate.step emulator;

    (* Compare results *)
    if not (Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) emulator.regs) then (
      Stdio.printf "Mismatch on cycle %d:\n" i;
      Stdio.printf "Emulator state: "; Stdio.print_s (Riscvemulate.sexp_of_state emulator);
      Stdio.printf "HW regs: "; Stdio.print_s (sexp_of_array sexp_of_int32 (Sim.Cpu.regs sim_cpu));
      Stdio.printf "HW mem: "; Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int sim_mem);
      failwith "mismatch between emulator and simulation"
    );
  done;
  (* HACK: if next instruction in emulator is a store, execute it, as CPU will already have done it because pipelining. *)
  let bonus_insn = Riscvemulate.load ~memory:emulator.memory ~addr:!(emulator.pc) ~size:4 ~extend:Unsigned in
  match Riscvemulate.of_int32_exn bonus_insn with
  | Store _ ->
      if debug then (Stdio.printf "\nExecuting bonus store because it's in the pipeline\n");
      Riscvemulate.step emulator;
  | _ -> ();
  (* Compare memory contents *)
  let mem_diff = Hashtbl.merge emulator.memory sim_mem ~f:(fun ~key -> function
    | `Both (a,b) -> if a <> b then Some (key, a, b) else None
    | `Left a -> if a <> 0 then Some (key, a, 0) else None
    | `Right b -> if 0 <> b then Some (key, 0, b) else None
  ) in
  if not (Hashtbl.is_empty mem_diff) then (
    Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 [%sexp_of: int32 * int * int] mem_diff);
    failwith "memory different at end"
  )

(* Run some random tests. Doing many more tests or instruction leads to cases where hacky approach of instruction
injection breaks :( *)
let _ = for i = 1 to 1000 do
  (* Stdio.printf "\n\n\n= Running small test %d =" i; *)
  Random.init i;
  (* mem_range being small risks stores overwriting instructions, which causes emulator mismatch if within pipeline already *)
  compare_emulator ~insn_count:10 ~mem_range:Int32.(of_int_exn 10000, of_int_exn 10024) ~reg_max:8
done
