
open! Base

(* Get a stream of random instructions with the given seed *)
let random_stream ~mem_range ?(reg_max = 32) ~seed () =
  Random.init seed;
  Sequence.unfold ~init:() ~f:(fun _ ->
    Some (Riscvemulate.Random.instruction ~mem_range ~reg_max (), ())
  )

(* Generate a full program with the given length and other constraints.
We avoid the issues of the initial `hacky_fuzzing` attempt
with the following constraints on the generated program:
- Each instruction executed by the program is never accessed as data memory prior to running
as an instruction. This allows us to generate instructions on-the-fly without affecting earlier
execution, but also avoids self-modifying-code hazards.
- Within the given number of instructions + 1, a "halt" instruction branching to itself will be
executed. In other words, executing insn_count+N instructions ends in the same state for any N>=0.

Returns the program as a memory and as a trace of executed instructions.
*)
let generate_program ~insn_count ~insn_stream =
  (* Mutable insn_stream interface for getting random instructions *)
  let insn_stream = ref insn_stream in
  let get_insn () =
    let insn, stream = Sequence.next !insn_stream |> Option.value_exn in
    insn_stream := stream; insn in
  (* Memory to store our program and emulator to figure out where to fetch insns from *)
  let insn_mem = Hashtbl.create (module Int32)
  and emulator = Riscvemulate.blank ()
  and insn_trace = ref [] in
  (* A candidate instruction is invalid if the program ends up at already-accessed memory or
  is not aligned to 4 bytes (this restriction is strict, but required anyway for base ISA and
  helpful here to avoid partial overlap with previous instructions).
  Also forbid instructions that overwrite the instruction that follows them (not detected as
  already-accessed yet because we haven't run the instruction).
  NOTE: For simplicity, currently forbid reexecuting instructions. Fixing requires making this validity
  check recursive because need to check the result of the existing instruction in new state. Would also
  need separate array to track loads/stores, as emulator memory includes instruction injections. Likely
  not worth the complexity (would need duplicate emulator state for multi-cycle speculation).
  But maybe should update fuzzer to generate aligned branches to avoid undersampling? *)
  let candidate_valid insn =
    let newpc = Riscvemulate.next_pc ~regs:emulator.regs ~pc:!(emulator.pc) ~insn in
    let clobber = Riscvemulate.next_clobber ~regs:emulator.regs ~insn in
    Int32.((newpc land of_int_exn 3) = zero) && (
      Sequence.init 4 ~f:(fun i -> Int32.(newpc + of_int_exn i))
      |> Sequence.for_all ~f:(fun addr -> not (Hashtbl.mem emulator.memory addr || clobber addr))
    )
  in
  for _ = 1 to insn_count do
    (* Generate an instruction, rejecting those which would violate the PC constraint on the
    next cycle (usually branches, but occasionally may need branch if PC+4 is invalid). *)
    let insn = Riscvemulate.Random.(resample ~f:get_insn ~cond:candidate_valid) in
    (* Inject into memory (NOTE: can't overwrite existing, see `candidate_valid` note) *)
    List.iter [insn_mem; emulator.memory] ~f:(fun memory ->
      Riscvemulate.store ~memory ~addr:!(emulator.pc) ~value:(Riscvemulate.to_int32 insn) ~size:4);
    insn_trace := insn :: !insn_trace;
    (* Step emulation *)
    Riscvemulate.step emulator
  done;
  (* Add a halt instruction after the program ends *)
  let halt = Riscvemulate.Branch (Eq, {rs1=0; rs2=0; imm=Int32.zero}) in
  Riscvemulate.store ~memory:insn_mem ~addr:!(emulator.pc) ~size:4
    ~value:Riscvemulate.(to_int32 (halt));
  insn_mem, (List.rev !insn_trace)

(* Run a program on the emulator and on the simulator hardware, returning `None` if end state is the same
and `Some (correct_regs, sim_regs, mem_diff)` otherwise, where `mem_diff` maps addresses to `(correct_byte,
sim_byte)` pairs. *)
let test_program ~program ~insn_count =
  let sim_cpu = Sim.Cpu.create ()
  and sim_mem = Hashtbl.copy program in
  let emulator = Riscvemulate.with_mem program in
  for _ = 0 to insn_count do
    Sim.Cpu.cycle_insn sim_cpu sim_mem;
    (* Stdio.printf "currently at pc=%d\n" (Int32.to_int_exn !(emulator.pc));
    Stdio.print_s (sexp_of_array sexp_of_int32 emulator.regs);
    Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int emulator.memory); *)
    Riscvemulate.step emulator;
  done;
  let mem_diff = Hashtbl.merge emulator.memory sim_mem ~f:(fun ~key:_ -> function
    | `Both (a,b) -> if a <> b then Some (a, b) else None
    | `Left a -> if a <> 0 then Some (a, 0) else None
    | `Right b -> if 0 <> b then Some (0, b) else None
  ) in
  if Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) emulator.regs && Hashtbl.is_empty mem_diff
  then None
  else Some (emulator.regs, Sim.Cpu.regs sim_cpu, mem_diff)

(* Run programs with limited reg/memory to cause hazards *)
let insn_count = 3
let _ = for i = 1 to 100 do
  let insn_stream = random_stream ~mem_range:Int32.(zero, of_int_exn 16) ~reg_max:8 ~seed:i () in
  let (program, trace) = generate_program ~insn_count ~insn_stream:insn_stream in
  (* Stdio.printf "\n\nRunning program %d\n" i;
  List.iter trace ~f:(fun i -> Stdio.print_s (Riscvemulate.sexp_of_insn i));
  Stdio.printf "\nInitial memory contents:\n";
  Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int program); *)
  let result = test_program ~program ~insn_count:(insn_count + 5) in
  match result with
  | Some (correct_regs, sim_regs, mem_diff) -> (
    Stdio.printf "Error in program %d\n" i;
    List.iter trace ~f:(fun i -> Stdio.print_s (Riscvemulate.sexp_of_insn i));
    Stdio.printf "\nCorrect regs:\n";
    Stdio.print_s (sexp_of_array sexp_of_int32 correct_regs);
    Stdio.printf "\nActual regs:\n";
    Stdio.print_s (sexp_of_array sexp_of_int32 sim_regs);
    Stdio.printf "\nMem diff:\n";
    Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 [%sexp_of: int * int] mem_diff);
    Stdio.printf "\nInitial memory contents:\n";
    Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int program);
    failwith ("error in program " ^ Int.to_string i)
  )
  | None -> ()
done
