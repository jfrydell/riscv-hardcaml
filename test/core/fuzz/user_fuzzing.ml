(** Generates random programs which run in user context, with address translation,
    triggering execptions which are handled by a lightweight supervisor. *)

open! Core
module Insn = Riscv_isa.Insn
open Insn

(* Page table and trap handlers are defined starting at 0x40000000. *)
let supervisor_base = Int32.of_string "0x40000000"
let root_page_table = supervisor_base
let trap_handler_pa = Int32.of_string "0x40001000"
let trap_handler_va = supervisor_base
let invalid_base = Int32.of_string "0x40101000"
let invalid_page_pa = Int32.of_string "0x4000f000"
let user_start = Int32.of_string "0x00100000"
let page_size = 0x1000
let page_mask = Int32.of_int_exn (page_size - 1)
let addi ~rd ~rs1 imm = Insn.IntImm (Add, { rd; rs1; imm = Int32.of_int_exn imm })
let csrw csr rs1 = Insn.Csr { op = Csrrw; rd = 0; src = Reg rs1; csr }
let csrr rd csr = Insn.Csr { op = Csrrs; rd; src = Reg 0; csr }
let csrrw rd csr rs1 = Insn.Csr { op = Csrrw; rd; src = Reg rs1; csr }

let bootstrap =
  let open Insn.Csr_address in
  [ Insn.Lui { rd = 1; imm = trap_handler_va }
  ; csrw stvec 1
  ; Insn.Lui { rd = 1; imm = Int32.of_int_exn 0xb000 }
  ; addi ~rd:1 ~rs1:1 0x15d
  ; csrw medeleg 1
  ; Insn.Lui { rd = 1; imm = Int32.of_string "0x80040000" }
  ; csrw 0x180 1
  ; Insn.Lui { rd = 30; imm = Int32.of_string "0xc0000000" }
  ; Insn.Lui { rd = 31; imm = invalid_base }
  ; Insn.Lui { rd = 1; imm = user_start }
  ; csrw mepc 1
  ; csrw mstatus 0
  ; Insn.Mret
  ]
;;

(* x31 is the handler scratch register, preserved through sscratch.  x30 holds the
   constant used to fold a bad fetch PC back into the user quarter. *)
let trap_handler =
  let open Insn.Csr_address in
  [ csrrw 31 sscratch 31
  ; csrr 31 scause
  ; addi ~rd:31 ~rs1:31 (-12)
  ; Insn.Branch (Eq, { rs1 = 31; rs2 = 0; imm = Int32.of_int_exn 24 })
  ; (* Return to next instruction. *)
    csrr 31 sepc
  ; addi ~rd:31 ~rs1:31 4
  ; csrw sepc 31
  ; csrrw 31 sscratch 31
  ; Insn.Sret
  ; (* Instruction access fault; add $30 to EPC. *)
    csrr 31 sepc
  ; Insn.IntReg (Add, { rd = 31; rs1 = 31; rs2 = 30 })
  ; csrw sepc 31
  ; csrrw 31 sscratch 31
  ; Insn.Sret
  ]
;;

type permission =
  | Read
  | Write
  | Execute

let permission_bit = function
  | Read -> 0x2
  | Write -> 0x6
  | Execute -> 0x8
;;

type mapping =
  { ppn : int
  ; user : bool
  ; mutable permissions : int
  }

type exception_kind =
  | Load_page_fault
  | Store_page_fault
  | Unaligned_load
  | Unaligned_store
  | Unaligned_branch
  | Illegal_instruction
  | User_ecall
  | Breakpoint
  | Invalid_pc
[@@deriving sexp_of]

let random_exception_kind random =
  let exception_kinds =
    [| Load_page_fault
     ; Store_page_fault
     ; Unaligned_load
     ; Unaligned_store
     ; Unaligned_branch
     ; Illegal_instruction
     ; User_ecall
     ; Breakpoint
     ; Invalid_pc
    |]
  in
  exception_kinds.(Splittable_random.int
                     random
                     ~lo:0
                     ~hi:(Array.length exception_kinds - 1))
;;

(** An instruciotn that was executed, or that caused an exception instead. *)
type trace_entry =
  | Normal of int32 * Insn.insn
  | Exception of int32 * exception_kind * Insn.insn
[@@deriving sexp_of]

type generated_program =
  { memory : int Int32.Table.t
  ; expected_memory : int Int32.Table.t
  ; expected_regs : int32 array
  ; commit_count : int
  ; trace : trace_entry list
  }

(** Write a list of instructions to memory at the given start address. *)
let store_program memory ~addr insns =
  List.iteri insns ~f:(fun index insn ->
    Riscvemulate.State.store
      ~memory
      ~addr:Int32.(addr + of_int_exn Int.(4 * index))
      ~size:4
      ~value:(Insn.to_int32 insn))
;;

let page_number addr = Int32.(to_int_exn (addr lsr 12))

let materialize_page_table memories mappings =
  let next_table = ref Int32.(root_page_table + of_int_exn Int.(2 * page_size)) in
  let second_level = Int.Table.create () in
  let table_for vpn1 =
    Hashtbl.find_or_add second_level vpn1 ~default:(fun () ->
      let addr = !next_table in
      (next_table := Int32.(!next_table + of_int_exn page_size));
      List.iter memories ~f:(fun memory ->
        Riscvemulate.State.store
          ~memory
          ~addr:Int32.(root_page_table + of_int_exn Int.(4 * vpn1))
          ~size:4
          ~value:Int32.((of_int_exn (page_number addr) lsl 10) lor one));
      addr)
  in
  Hashtbl.iteri mappings ~f:(fun ~key:vpn ~data:{ ppn; user; permissions } ->
    let vpn1 = vpn lsr 10 in
    let vpn0 = vpn land 0x3ff in
    let table = table_for vpn1 in
    let flags = 1 lor permissions lor if user then 0x10 else 0 in
    List.iter memories ~f:(fun memory ->
      Riscvemulate.State.store
        ~memory
        ~addr:Int32.(table + of_int_exn Int.(4 * vpn0))
        ~size:4
        ~value:Int32.((of_int_exn ppn lsl 10) lor of_int_exn flags)))
;;

let generate_program ~seed ~insn_count ~exception_rate =
  if Float.(exception_rate < 0. || exception_rate > 1.)
  then invalid_arg "exception_rate must be between zero and one";
  let memory = Int32.Table.create () in
  let expected_memory = Int32.Table.create () in
  List.iter [ memory; expected_memory ] ~f:(fun memory ->
    store_program memory ~addr:Int32.zero bootstrap;
    store_program memory ~addr:trap_handler_pa trap_handler);
  (* Store list of VPN mappings for emulator address translation and later constructing actual page table. *)
  let mappings = Int.Table.create () in
  let next_user_ppn = ref 1 in
  (* Map a page to phyiscal memory, or set new permission bit already mapped. *)
  let map_page ~user va permission =
    let vpn = page_number va in
    let mapping =
      Hashtbl.find_or_add mappings vpn ~default:(fun () ->
        let ppn = !next_user_ppn in
        Int.incr next_user_ppn;
        { ppn; user; permissions = 0 })
    in
    if not (Bool.equal mapping.user user)
    then failwith "page mapped at two privilege levels";
    mapping.permissions <- mapping.permissions lor permission_bit permission
  in
  Hashtbl.set
    mappings
    ~key:(page_number trap_handler_va)
    ~data:
      { ppn = page_number trap_handler_pa
      ; user = false
      ; permissions = permission_bit Read lor permission_bit Execute
      };
  Hashtbl.set
    mappings
    ~key:(page_number invalid_base)
    ~data:
      { ppn = page_number invalid_page_pa
      ; user = false
      ; permissions =
          permission_bit Read lor permission_bit Write lor permission_bit Execute
      };
  (* Use existing mappings to translate address for emulator. *)
  let translate va =
    let mapping = Hashtbl.find_exn mappings (page_number va) in
    Int32.((of_int_exn mapping.ppn lsl 12) lor (va land page_mask))
  in
  (* Initialize emulator with state after bootstrap. *)
  let emulator =
    Riscvemulate.State.create ~address_translation:translate expected_memory
  in
  emulator.pc := user_start;
  emulator.regs.(1) <- user_start;
  emulator.regs.(30) <- Int32.of_string "0xc0000000";
  emulator.regs.(31) <- invalid_base;
  (* Track virtual addresses we've used, ensuring newly-generated instructions
     don't reuse already-seen data. *)
  let used_virtual = Int32.Table.create () in
  let mark_used addr size =
    for offset = 0 to size - 1 do
      Hashtbl.set used_virtual ~key:Int32.(addr + of_int_exn offset) ~data:()
    done
  in
  let target_unused addr =
    Int32.(addr >= user_start && addr < supervisor_base && addr land of_int_exn 3 = zero)
    && List.for_all (List.init 4 ~f:Fn.id) ~f:(fun offset ->
      not (Hashtbl.mem used_virtual Int32.(addr + of_int_exn offset)))
  in
  let normal_stream = ref (Fuzzing.insn_stream ~reg_max:30 ~seed) in
  let next_normal () =
    let insn, rest = Sequence.next !normal_stream |> Option.value_exn in
    normal_stream := rest;
    insn
  in
  let random = Splittable_random.of_int seed in
  let random_reg () = Splittable_random.int random ~lo:1 ~hi:30 in
  let access_valid insn =
    match Riscvemulate.Unpriv.access_addr_and_size ~regs:emulator.regs ~insn with
    | None -> true
    | Some (addr, size) ->
      let last = Int32.(addr + of_int_exn Int.(size - 1)) in
      Int32.(addr >= zero && last >= addr && last < supervisor_base)
      && page_number addr = page_number last
  in
  let candidate_valid insn =
    (not (Riscvemulate.Unpriv.is_unaligned_access ~regs:emulator.regs ~insn))
    && access_valid insn
    &&
    let next_pc =
      Riscvemulate.Unpriv.next_pc ~regs:emulator.regs ~pc:!(emulator.pc) ~insn
    in
    let clobber = Riscvemulate.Unpriv.next_access ~regs:emulator.regs ~insn in
    target_unused next_pc
    && (not (Int32.equal next_pc !(emulator.pc)))
    && List.for_all (List.init 4 ~f:Fn.id) ~f:(fun offset ->
      not (clobber Int32.(next_pc + of_int_exn offset)))
  in
  let rec choose_normal () =
    let insn = next_normal () in
    if candidate_valid insn then insn else choose_normal ()
  in
  let exception_insn kind =
    match kind with
    | Load_page_fault ->
      Insn.Load (Word, Signed, { rd = random_reg (); rs1 = 31; imm = Int32.zero })
    | Store_page_fault ->
      Insn.Store (Word, { rs1 = 31; rs2 = random_reg (); imm = Int32.zero })
    | Unaligned_load ->
      Insn.Load (Word, Signed, { rd = random_reg (); rs1 = 0; imm = Int32.of_int_exn 2 })
    | Unaligned_store ->
      Insn.Store (Word, { rs1 = 0; rs2 = random_reg (); imm = Int32.of_int_exn 2 })
    | Unaligned_branch -> Insn.Branch (Eq, { rs1 = 0; rs2 = 0; imm = Int32.of_int_exn 2 })
    | Illegal_instruction -> Insn.Mret
    | User_ecall -> Insn.Ecall
    | Breakpoint -> Insn.Ebreak
    | Invalid_pc -> assert false
  in
  let rec choose_exception () =
    let kind = random_exception_kind random in
    match kind with
    | Invalid_pc ->
      (* Supervisor will end up returning us to location base on jump PC; make sure unused. *)
      let offset = Int32.of_int_exn (4 * Splittable_random.int ~lo:0 ~hi:511 random) in
      let target = Int32.((invalid_base + offset) land (supervisor_base - one)) in
      if target_unused target && not (Int32.equal target !(emulator.pc))
      then kind, Insn.Jalr { rd = 0; rs1 = 31; imm = offset }, target
      else choose_exception ()
    | _ ->
      let next_pc = Int32.(!(emulator.pc) + of_int_exn 4) in
      if target_unused next_pc
      then kind, exception_insn kind, next_pc
      else choose_exception ()
  in
  let trace = ref [] in
  let handler_commits = ref 0 in
  let legal_commits = ref 0 in
  for _ = 1 to insn_count do
    let pc = !(emulator.pc) in
    let exception_ = Float.(Splittable_random.unit_float random < exception_rate) in
    let insn, exception_kind, next_pc =
      if exception_
      then (
        let kind, insn, next_pc = choose_exception () in
        insn, Some kind, next_pc)
      else (
        let insn = choose_normal () in
        insn, None, Riscvemulate.Unpriv.next_pc ~regs:emulator.regs ~pc ~insn)
    in
    (* Put instruction into memory, mapping page and marking used. *)
    map_page ~user:true pc Execute;
    List.iter [ memory; expected_memory ] ~f:(fun memory ->
      Riscvemulate.State.store
        ~memory
        ~addr:(translate pc)
        ~size:4
        ~value:(Insn.to_int32 insn));
    mark_used pc 4;
    (* Execute instruction, emulating effect of handler for exceptions. *)
    match exception_kind with
    | Some kind ->
      (match kind with
       | Unaligned_load -> map_page ~user:true (Int32.of_int_exn 2) Read
       | Unaligned_store -> map_page ~user:true (Int32.of_int_exn 2) Write
       | _ -> ());
      emulator.pc := next_pc;
      handler_commits := !handler_commits + 9;
      (match kind with
       | Invalid_pc -> Int.incr legal_commits
       | _ -> ());
      trace := Exception (pc, kind, insn) :: !trace
    | None ->
      (match insn with
       | Load (_, _, { rs1; imm; _ }) ->
         map_page ~user:true Int32.(emulator.regs.(rs1) + imm) Read
       | Store (_, { rs1; imm; _ }) ->
         map_page ~user:true Int32.(emulator.regs.(rs1) + imm) Write
       | _ -> ());
      Option.iter
        (Riscvemulate.Unpriv.access_addr_and_size ~regs:emulator.regs ~insn)
        ~f:(fun (addr, size) -> mark_used addr size);
      Riscvemulate.Unpriv.step emulator;
      Int.incr legal_commits;
      trace := Normal (pc, insn) :: !trace
  done;
  let halt = Insn.Branch (Eq, { rs1 = 0; rs2 = 0; imm = Int32.zero }) in
  let halt_pc = !(emulator.pc) in
  map_page ~user:true halt_pc Execute;
  List.iter [ memory; expected_memory ] ~f:(fun memory ->
    Riscvemulate.State.store
      ~memory
      ~addr:(translate halt_pc)
      ~size:4
      ~value:(Insn.to_int32 halt));
  materialize_page_table [ memory; expected_memory ] mappings;
  { memory
  ; expected_memory
  ; expected_regs = Array.copy emulator.regs
  ; commit_count = List.length bootstrap + !legal_commits + !handler_commits + 1
  ; trace = List.rev !trace
  }
;;

let memory_diff expected actual =
  Hashtbl.merge expected actual ~f:(fun ~key:_ -> function
    | `Both (a, b) -> if a = b then None else Some (a, b)
    | `Left a -> if a = 0 then None else Some (a, 0)
    | `Right b -> if b = 0 then None else Some (0, b))
;;

let run_and_check ?cycle_fn ~seed ~insn_count ~exception_rate () =
  let generated = generate_program ~seed ~insn_count ~exception_rate in
  let sim = Sim.Cpu.create ~memory:(Hashtbl.copy generated.memory) No_waves in
  for _ = 1 to generated.commit_count do
    Sim.Cpu.cycle_insn ?cycle_fn sim
  done;
  Sim.Cpu.flush sim;
  let hardware_regs = Sim.Cpu.regs sim in
  let diff = memory_diff generated.expected_memory (Sim.Cpu.memory sim) in
  if (not (Array.equal Int32.equal generated.expected_regs hardware_regs))
     || not (Hashtbl.is_empty diff)
  then
    raise_s
      [%message
        "user-mode hardware/model mismatch"
          (seed : int)
          (insn_count : int)
          (exception_rate : float)
          (generated.trace : trace_entry list)
          (generated.expected_regs : int32 array)
          (hardware_regs : int32 array)
          (diff : (int * int) Int32.Table.t)]
;;
