
open! Base

(* Generate a full program with the given length and other constraints, returning an initial
memory containing the program. We avoid the issues of the initial `hacky_fuzzing` attempt
with the following constraints on the generated program:
- Each instruction executed by the program is never accessed as data memory prior to running
as an instruction. This allows us to generate instructions on-the-fly without affecting earlier
execution, but also avoids self-modifying-code hazards.
- Within the given number of instructions + 1, a "halt" instruction branching to itself will be
executed. In other words, executing insn_count+N instructions ends in the same state for any N>=0.
*)
let generate_program ~insn_count ~mem_range ?(reg_max = 32) () =
  (* Memory to store our program and emulator to figure out where to fetch insns from *)
  let insn_mem = Hashtbl.create (module Int32)
  and emulator = Riscvemulate.blank () in
  (* A candidate instruction is invalid if the program ends up at already-accessed memory or
  is not aligned to 4 bytes (this restriction is strict, but required anyway for base ISA and
  helpful here to avoid partial overlap with previous instructions).
  NOTE: For simplicity, currently forbid reexecuting instructions. Fixing requires making this validity
  check recursive because need to check the result of the existing instruction in new state. Would also
  need separate array to track loads/stores, as emulator memory includes instruction injections. Likely
  not worth the complexity (would need duplicate emulator state for multi-cycle speculation). *)
  let candidate_valid insn =
    let newpc = Riscvemulate.next_pc ~regs:emulator.regs ~pc:!(emulator.pc) ~insn in
    Int32.((newpc land of_int_exn 3) = zero) && (
      Sequence.init 4 ~f:(fun i -> Int32.(newpc + of_int_exn i))
      |> Sequence.for_all ~f:(fun addr -> not (Hashtbl.mem emulator.memory addr))
    )
  in
  for _ = 1 to insn_count do
    (* Generate an instruction, rejecting those which would violate the PC constraint on the
    next cycle (usually branches, but occasionally may need branch if PC+4 is invalid). *)
    let insn = Riscvemulate.Random.(resample ~f:(instruction ~mem_range ~reg_max) ~cond:candidate_valid) in
    (* Inject into memory (NOTE: can't overwrite existing, see `candidate_valid` note) *)
    List.iter [insn_mem; emulator.memory] ~f:(fun memory ->
      Riscvemulate.store ~memory ~addr:!(emulator.pc) ~value:(Riscvemulate.to_int32 insn) ~size:4);
    (* Step emulation *)
    Riscvemulate.step emulator
  done;
  (* Add a halt instruction after the program ends *)
  Riscvemulate.store ~memory:insn_mem ~addr:!(emulator.pc) ~size:4
    ~value:Riscvemulate.(to_int32 (Branch (Eq, {rs1=0; rs2=0; imm=Int32.zero})));
  insn_mem

(* Run a program on the emulator and on the simulator hardware, returning `true` if end state is the same *)
let test_program ~program ~insn_count =
  let sim_cpu = Sim.Cpu.create ()
  and sim_mem = Hashtbl.copy program in
  let emulator = Riscvemulate.with_mem program in
  for _ = 0 to insn_count do
    Sim.Cpu.cycle_insn sim_cpu sim_mem;
    Riscvemulate.step emulator;
  done;
  Array.equal Int32.equal (Sim.Cpu.regs sim_cpu) emulator.regs
  && Hashtbl.is_empty (
    Hashtbl.merge emulator.memory sim_mem ~f:(fun ~key:_ -> function
      | `Both (a,b) -> if a <> b then Some (a, b) else None
      | `Left a -> if a <> 0 then Some (a, 0) else None
      | `Right b -> if 0 <> b then Some (0, b) else None
    )
  )

(* Run programs with limited reg/memory to cause hazards *)
let _ = for i = 1 to 100 do
  Random.init i;
  let program = generate_program ~insn_count:3 ~mem_range:Int32.(zero, of_int_exn 16) ~reg_max:8 () in
  if not (test_program ~program ~insn_count:10) then failwith ("error in program " ^ Int.to_string i)
done
