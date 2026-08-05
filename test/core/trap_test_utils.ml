open! Core
open Riscv_isa.Insn
open Riscvemulate.State
open Riscvemulate.Unpriv

let int = Int32.of_int_exn
let addi ~rd ~rs1 imm = IntImm (Add, { rd; rs1; imm = int imm })
let csrw csr rs1 = Csr { op = Csrrw; rd = 0; src = Reg rs1; csr }
let csrr rd csr = Csr { op = Csrrs; rd; src = Reg 0; csr }
let halt = Branch (Eq, { rs1 = 0; rs2 = 0; imm = Int32.zero })

let store_program memory ~addr insns =
  List.iteri insns ~f:(fun index insn ->
    store ~memory ~addr:Int32.(addr + int Int.(4 * index)) ~size:4 ~value:(to_int32 insn))
;;

let program sections =
  let memory = Int32.Table.create () in
  List.iter sections ~f:(fun (addr, insns) -> store_program memory ~addr:(int addr) insns);
  memory
;;

let run_hardware
  ?(max_cycles = 5_000)
  ?(before_cycle = fun (_sim : Sim.Cpu.t) -> ())
  ~done_
  memory
  =
  let sim = Sim.Cpu.create ~memory No_waves in
  let commits = ref [] in
  let rec loop cycles =
    if cycles = 0
    then
      raise_s
        [%message
          "Trap test timed out"
            (Sim.Cpu.regs sim : int32 array)
            (List.rev (List.take !commits 100) : int32 list)]
    else if done_ sim
    then ()
    else (
      before_cycle sim;
      Option.iter (Sim.Cpu.commit_pc sim) ~f:(fun pc -> commits := pc :: !commits);
      Sim.Cpu.cycle sim;
      loop (cycles - 1))
  in
  loop max_cycles;
  sim, List.rev !commits
;;

let run_emulator ?(max_steps = 1_000) ~done_ memory =
  let emulator = with_mem memory in
  let rec loop steps =
    if steps = 0
    then raise_s [%message "Trap emulator test timed out" (emulator : state)]
    else if done_ emulator
    then ()
    else (
      step emulator;
      loop (steps - 1))
  in
  loop max_steps;
  emulator
;;

let check_reg regs register expected =
  let actual = regs.(register) in
  let expected = int expected in
  if not (Int32.equal actual expected)
  then
    raise_s
      [%message
        "Unexpected register value" (register : int) (actual : int32) (expected : int32)]
;;

let exception_handler =
  let open Csr_address in
  [ csrr 20 mcause
  ; csrr 21 mepc
  ; csrr 22 mtval
  ; csrr 23 mstatus
  ; addi ~rd:24 ~rs1:24 1
  ; IntImm (Sll, { rd = 25; rs1 = 25; imm = int 4 })
  ; IntReg (Add, { rd = 25; rs1 = 25; rs2 = 20 })
  ; addi ~rd:21 ~rs1:21 4
  ; csrw mepc 21
  ; Mret
  ]
;;

let interrupt_handler =
  let open Csr_address in
  [ csrr 20 mcause
  ; csrr 21 mepc
  ; csrr 22 mstatus
  ; addi ~rd:24 ~rs1:24 1
  ; addi ~rd:30 ~rs1:0 1
  ; nop
  ; nop
  ; Mret
  ]
;;

let supervisor_exception_handler =
  let open Csr_address in
  [ csrr 20 scause
  ; csrr 21 sepc
  ; csrr 22 stval
  ; csrr 23 sstatus
  ; addi ~rd:24 ~rs1:24 1
  ; IntImm (Sll, { rd = 25; rs1 = 25; imm = int 4 })
  ; IntReg (Add, { rd = 25; rs1 = 25; rs2 = 20 })
  ; addi ~rd:21 ~rs1:21 4
  ; csrw sepc 21
  ; Sret
  ]
;;

let supervisor_interrupt_handler =
  let open Csr_address in
  [ csrr 20 scause
  ; csrr 21 sepc
  ; csrr 22 sstatus
  ; csrr 26 sip
  ; addi ~rd:24 ~rs1:24 1
  ; addi ~rd:30 ~rs1:0 1
  ; nop
  ; nop
  ; Sret
  ]
;;
