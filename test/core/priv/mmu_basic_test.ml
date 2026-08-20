open! Core
open Riscv_isa.Insn
open Riscvemulate.State
open Trap_test_utils

let user_code_ppn = 0
let code_page_offset = 0x100
let code_virtual_address = 0x01407000 + code_page_offset
let load_virtual_address = 0x01408000
let store_virtual_address = 0x01409000
let invalid_virtual_address = 0x0140a000
let load_value = 0x12345678
let trap_vector = 0x80

let trap_handler =
  let open Csr_address in
  [ csrr 20 mcause
  ; csrr 21 mepc
  ; csrr 22 mtval
  ; csrr 23 mstatus
  ; addi ~rd:31 ~rs1:0 42
  ; halt
  ]
;;

let setup_page_table memory =
  (* VPN[1] = 5 selects a non-leaf PTE pointing at the page table at PPN 2. *)
  store ~memory ~addr:(int 0x1014) ~size:4 ~value:(int ((2 lsl 10) lor 1));
  (* VPN[0] = 7 selects an executable user page mapped to [user_code_ppn]. *)
  store ~memory ~addr:(int 0x201c) ~size:4 ~value:(int ((user_code_ppn lsl 10) lor 0x19));
  (* The next two virtual pages map to readable PPN 4 and read/write PPN 5. *)
  store ~memory ~addr:(int 0x2020) ~size:4 ~value:(int ((4 lsl 10) lor 0x13));
  store ~memory ~addr:(int 0x2024) ~size:4 ~value:(int ((5 lsl 10) lor 0x17))
;;

let memory_with_user_program user_program =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ addi ~rd:1 ~rs1:0 trap_vector
          ; csrw mtvec 1
          ; Lui { rd = 1; imm = Int32.min_value }
          ; addi ~rd:1 ~rs1:1 1
          ; csrw Privileged.Csrs.addresses.satp 1
          ; Lui { rd = 2; imm = int 0x01407000 }
          ; addi ~rd:2 ~rs1:2 code_page_offset
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; (user_code_ppn lsl 12) + code_page_offset, user_program
      ; trap_vector, trap_handler
      ]
  in
  setup_page_table memory;
  memory
;;

let check_trap_handler_regs ~regs ~mcause ~mepc ~mtval ~mstatus =
  check_reg regs 31 42;
  check_reg regs 20 mcause;
  check_reg regs 21 mepc;
  check_reg regs 22 mtval;
  check_reg regs 23 mstatus
;;

let check_faulting_pc_not_committed commits faulting_pc =
  if List.mem commits (int faulting_pc) ~equal:Int32.equal
  then
    raise_s
      [%message
        "Faulting instruction incorrectly committed"
          (faulting_pc : int)
          (commits : int32 list)]
;;

let run_fault_test
  ?(prepare_memory = fun (_ : int Int32.Table.t) -> ())
  ~user_program
  ~mcause
  ~mepc
  ~mtval
  ()
  =
  let memory = memory_with_user_program user_program in
  prepare_memory memory;
  let sim, commits =
    run_hardware ~max_cycles:1_000 memory ~done_:(fun sim ->
      Int32.equal (Sim.Cpu.regs sim).(31) (int 42))
  in
  check_trap_handler_regs ~regs:(Sim.Cpu.regs sim) ~mcause ~mepc ~mtval ~mstatus:0;
  check_faulting_pc_not_committed commits mepc;
  sim
;;

let test_load_page_fault () =
  let faulting_pc = code_virtual_address + 0x18 in
  let sim =
    run_fault_test
      ~prepare_memory:(fun memory ->
        store ~memory ~addr:(int 0x4000) ~size:4 ~value:(int load_value))
      ~user_program:
        [ Lui { rd = 3; imm = int load_virtual_address }
        ; Load (Word, Signed, { rd = 4; rs1 = 3; imm = Int32.zero })
        ; Lui { rd = 5; imm = int store_virtual_address }
        ; Store (Word, { rs1 = 5; rs2 = 4; imm = Int32.zero })
        ; Load (Word, Unsigned, { rd = 6; rs1 = 5; imm = Int32.zero })
        ; Lui { rd = 7; imm = int invalid_virtual_address }
        ; Load (Word, Unsigned, { rd = 8; rs1 = 7; imm = Int32.zero })
        ]
      ~mcause:13
      ~mepc:faulting_pc
      ~mtval:invalid_virtual_address
      ()
  in
  let regs = Sim.Cpu.regs sim in
  check_reg regs 4 load_value;
  check_reg regs 6 load_value;
  let stored_value =
    load ~memory:(Sim.Cpu.memory sim) ~addr:(int 0x5000) ~size:4 ~extend:Unsigned
  in
  if not (Int32.equal stored_value (int load_value))
  then raise_s [%message "Translated store did not update memory" (stored_value : int32)]
;;

let test_store_page_fault () =
  let faulting_pc = code_virtual_address + 8 in
  run_fault_test
    ~user_program:
      [ Lui { rd = 7; imm = int invalid_virtual_address }
      ; addi ~rd:8 ~rs1:0 42
      ; Store (Word, { rs1 = 7; rs2 = 8; imm = Int32.zero })
      ]
    ~mcause:15
    ~mepc:faulting_pc
    ~mtval:invalid_virtual_address
    ()
  |> ignore
;;

let test_instruction_page_fault () =
  let faulting_pc = load_virtual_address in
  run_fault_test
    ~user_program:
      [ Branch
          ( Eq
          , { rs1 = 0
            ; rs2 = 0
            ; imm = int (faulting_pc - code_virtual_address)
            } )
      ]
    ~mcause:12
    ~mepc:faulting_pc
    ~mtval:faulting_pc
    ()
  |> ignore
;;

let test_unaligned_load () =
  let faulting_pc = code_virtual_address + 4 in
  run_fault_test
    ~user_program:
      [ Lui { rd = 7; imm = int load_virtual_address }
      ; Load (Word, Unsigned, { rd = 8; rs1 = 7; imm = int 2 })
      ]
    ~mcause:4
    ~mepc:faulting_pc
    ~mtval:(load_virtual_address + 2)
    ()
  |> ignore
;;

let test_unaligned_store () =
  let faulting_pc = code_virtual_address + 8 in
  run_fault_test
    ~user_program:
      [ Lui { rd = 7; imm = int store_virtual_address }
      ; addi ~rd:8 ~rs1:0 42
      ; Store (Word, { rs1 = 7; rs2 = 8; imm = int 2 })
      ]
    ~mcause:6
    ~mepc:faulting_pc
    ~mtval:(store_virtual_address + 2)
    ()
  |> ignore
;;

let test_unaligned_branch () =
  let faulting_pc = code_virtual_address in
  run_fault_test
    ~user_program:[ Branch (Eq, { rs1 = 0; rs2 = 0; imm = int 2 }) ]
    ~mcause:0
    ~mepc:faulting_pc
    ~mtval:(faulting_pc + 2)
    ()
  |> ignore
;;

let () =
  test_load_page_fault ();
  test_store_page_fault ();
  test_instruction_page_fault ();
  test_unaligned_load ();
  test_unaligned_store ();
  test_unaligned_branch ();
  Stdio.print_endline "Sv32 page faults and address-alignment traps: good"
;;
