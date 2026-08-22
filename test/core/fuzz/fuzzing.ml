open! Core
module Insn = Riscv_isa.Insn
module State = Riscvemulate.State
module Unpriv = Riscvemulate.Unpriv

(* Instruction generator using the derived [@@deriving quickcheck] from isa/insn.ml.
   Filters system instructions and normalizes through encode/decode roundtrip
   to ensure only hardware-representable instructions are produced. *)
let insn_generator ~reg_max =
  let reg_ok r = r < reg_max in
  Quickcheck.Generator.filter_map Insn.quickcheck_generator_insn ~f:(fun insn ->
    let open Insn in
    match insn with
    | Ecall | Ebreak | Mret | Sret | Fencei | Csr _ -> None
    | _ ->
      let regs_valid =
        match insn with
        | IntReg (_, { rd; rs1; rs2 }) -> reg_ok rd && reg_ok rs1 && reg_ok rs2
        | IntImm (_, { rd; rs1; _ }) | Load (_, _, { rd; rs1; _ }) | Jalr { rd; rs1; _ }
          -> reg_ok rd && reg_ok rs1
        | Store (_, { rs1; rs2; _ }) | Branch (_, { rs1; rs2; _ }) ->
          reg_ok rs1 && reg_ok rs2
        | Jal { rd; _ } | Lui { rd; _ } | AuiPc { rd; _ } -> reg_ok rd
        | Ecall | Ebreak | Mret | Sret | Fencei | Csr _ -> false
      in
      if not regs_valid
      then None
      else (
        try Some (of_int32_exn (to_int32 insn)) with
        | _ -> None))
;;

(* Sample an instruction stream from the QuickCheck generator *)
let insn_stream ~reg_max ~seed =
  let random = Splittable_random.of_int seed in
  Sequence.unfold ~init:() ~f:(fun () ->
    let insn = Quickcheck.Generator.generate (insn_generator ~reg_max) ~size:30 ~random in
    Some (insn, ()))
;;

(* Rejection sampling: draw from f until cond is satisfied *)
let rec resample ~f ~cond =
  let x = f () in
  if cond x then x else resample ~f ~cond
;;

(* Generate a full program with the given length and other constraints.
   We avoid issues with the following constraints on the generated program:
   - Each instruction executed by the program is never accessed as data memory prior to running
   as an instruction. This allows us to generate instructions on-the-fly without affecting earlier
   execution, but also avoids self-modifying-code hazards.
   - Within the given number of instructions + 1, a "halt" instruction branching to itself will be
   executed. In other words, executing insn_count+N instructions ends in the same state for any N>=0.

   Returns the program as a memory and as a trace of executed instructions. *)
let generate_program ~insn_count ~insn_stream ~filter =
  let insn_stream = ref insn_stream in
  let get_insn () =
    let insn, stream = Sequence.next !insn_stream |> Option.value_exn in
    insn_stream := stream;
    insn
  in
  let insn_mem = Hashtbl.create (module Int32)
  and emulator = State.blank ()
  and insn_trace = ref [] in
  (* A candidate instruction is invalid if the program ends up at already-accessed memory or
     is not aligned to 4 bytes. Also forbid instructions that access the instruction that follows
     them, and forbid reexecuting instructions. *)
  let candidate_valid insn =
    filter insn
    && (not @@ Unpriv.is_unaligned_access ~regs:emulator.regs ~insn)
    &&
    let newpc = Unpriv.next_pc ~regs:emulator.regs ~pc:!(emulator.pc) ~insn in
    let clobber = Unpriv.next_access ~regs:emulator.regs ~insn in
    Int32.(newpc land of_int_exn 3 = zero)
    (* Keep instruction fetches below the temporary I/O address boundary. *)
    && Int32.(newpc >= zero)
    && Int32.(newpc <> !(emulator.pc))
    && Sequence.init 4 ~f:(fun i -> Int32.(newpc + of_int_exn i))
       |> Sequence.for_all ~f:(fun addr ->
         not (Hashtbl.mem emulator.memory addr || clobber addr))
  in
  for _ = 1 to insn_count do
    let insn = resample ~f:get_insn ~cond:candidate_valid in
    List.iter [ insn_mem; emulator.memory ] ~f:(fun memory ->
      State.store ~memory ~addr:!(emulator.pc) ~value:(Insn.to_int32 insn) ~size:4);
    insn_trace := insn :: !insn_trace;
    Unpriv.step emulator
  done;
  let halt = Insn.Branch (Insn.Eq, { rs1 = 0; rs2 = 0; imm = Int32.zero }) in
  State.store ~memory:insn_mem ~addr:!(emulator.pc) ~size:4 ~value:(Insn.to_int32 halt);
  insn_mem, List.rev !insn_trace
;;

(* Run a program on the emulator and on the simulated hardware, returning None if end state is the
   same and Some with diagnostics otherwise. *)
let test_program ~cycle_fn ~program ~insn_count =
  let sim = Sim.Cpu.create ~memory:(Hashtbl.copy program) No_waves in
  let emulator = State.with_mem program in
  for _ = 0 to insn_count do
    Sim.Cpu.cycle_insn ?cycle_fn sim;
    Unpriv.step emulator
  done;
  Sim.Cpu.flush sim;
  let mem_diff =
    Hashtbl.merge emulator.memory (Sim.Cpu.memory sim) ~f:(fun ~key:_ -> function
      | `Both (a, b) -> if a <> b then Some (a, b) else None
      | `Left a -> if a <> 0 then Some (a, 0) else None
      | `Right b -> if 0 <> b then Some (0, b) else None)
  in
  if Array.equal Int32.equal (Sim.Cpu.regs sim) emulator.regs && Hashtbl.is_empty mem_diff
  then None
  else Some (emulator.regs, Sim.Cpu.regs sim, mem_diff)
;;

(* QuickCheck configuration for a single fuzz test *)
type fuzz_config =
  { seed : int
  ; insn_count : int
  ; working_insn_count : int
  (** For shrinking with binary search, the max instruction count known to pass the test. *)
  }
[@@deriving sexp_of]

(* Shrinker that binary-searches for the minimum insn_count reproducing a failure.
   Seed is kept fixed since changing it produces a completely different program. *)
let fuzz_config_shrinker =
  Quickcheck.Shrinker.create (fun t ->
    let next_test = (t.insn_count + t.working_insn_count) / 2 in
    [ { t with insn_count = next_test }
    ; { t with insn_count = t.insn_count - 1; working_insn_count = next_test }
    ]
    |> Sequence.of_list)
;;

(* Run a fuzz test: generate program from config, compare emulator vs hardware.
   Diagnostics are embedded in the error sexp so shrinking is quiet. *)
let check_equivalence ?(filter = Fn.const true) ?cycle_fn ~reg_max { seed; insn_count; _ }
  =
  let insn_stream = insn_stream ~reg_max ~seed in
  let program, trace = generate_program ~insn_count ~insn_stream ~filter in
  match test_program ~cycle_fn ~program ~insn_count with
  | None -> ()
  | Some (correct_regs, sim_regs, mem_diff) ->
    let mem_diff =
      if Hashtbl.length mem_diff > 50
      then [%message "" ~length:(Hashtbl.length mem_diff : int)]
      else [%message "" ~em_then_sim:(mem_diff : (int * int) Int32.Table.t)]
    in
    Error.raise_s
      [%message
        "hardware/emulator mismatch"
          (seed : int)
          (insn_count : int)
          (trace : Insn.insn list)
          (correct_regs : int32 array)
          (sim_regs : int32 array)
          (mem_diff : Sexp.t)]
;;
