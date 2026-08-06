open! Core
open Riscv_isa.Insn
open Riscvemulate.State
open Trap_test_utils

let user_code_ppn = 0
let code_page_offset = 0x100
let code_virtual_address = 0x01407000 + code_page_offset
let load_virtual_address = 0x01408000
let store_virtual_address = 0x01409000
let load_value = 0x12345678

let test_instruction_fetch_through_sv32 () =
  let open Csr_address in
  let memory =
    program
      [ ( 0
        , [ Lui { rd = 1; imm = Int32.min_value }
          ; addi ~rd:1 ~rs1:1 1
          ; csrw Privileged.Csrs.addresses.satp 1
          ; Lui { rd = 2; imm = int 0x01407000 }
          ; addi ~rd:2 ~rs1:2 code_page_offset
          ; csrw mepc 2
          ; csrw mstatus 0
          ; Mret
          ] )
      ; ( (user_code_ppn lsl 12) + code_page_offset
        , [ Lui { rd = 3; imm = int load_virtual_address }
          ; Load (Word, Signed, { rd = 4; rs1 = 3; imm = Int32.zero })
          ; Lui { rd = 5; imm = int store_virtual_address }
          ; Store (Word, { rs1 = 5; rs2 = 4; imm = Int32.zero })
          ; Load (Word, Unsigned, { rd = 6; rs1 = 5; imm = Int32.zero })
          ; addi ~rd:31 ~rs1:0 42
          ; halt
          ] )
      ]
  in
  (* VPN[1] = 5 selects a non-leaf PTE pointing at the page table at PPN 2. *)
  store ~memory ~addr:(int 0x1014) ~size:4 ~value:(int ((2 lsl 10) lor 1));
  (* VPN[0] = 7 selects an executable user page mapped to PPN 3. *)
  store ~memory ~addr:(int 0x201c) ~size:4 ~value:(int ((user_code_ppn lsl 10) lor 0x19));
  (* The next two virtual pages map to readable PPN 4 and read/write PPN 5. *)
  store ~memory ~addr:(int 0x2020) ~size:4 ~value:(int ((4 lsl 10) lor 0x13));
  store ~memory ~addr:(int 0x2024) ~size:4 ~value:(int ((5 lsl 10) lor 0x17));
  store ~memory ~addr:(int 0x4000) ~size:4 ~value:(int load_value);
  let sim, commits =
    run_hardware ~max_cycles:1_000 memory ~done_:(fun sim ->
      Int32.equal (Sim.Cpu.regs sim).(31) (int 42))
  in
  let regs = Sim.Cpu.regs sim in
  check_reg regs 31 42;
  check_reg regs 4 load_value;
  check_reg regs 6 load_value;
  let stored_value =
    load ~memory:(Sim.Cpu.memory sim) ~addr:(int 0x5000) ~size:4 ~extend:Unsigned
  in
  if not (Int32.equal stored_value (int load_value))
  then raise_s [%message "Translated store did not update memory" (stored_value : int32)];
  if not (List.mem commits (int code_virtual_address) ~equal:Int32.equal)
  then
    raise_s
      [%message
        "Translated user instruction did not commit"
          (code_virtual_address : int)
          (commits : int32 list)]
;;

let () =
  test_instruction_fetch_through_sv32 ();
  Stdio.print_endline "Sv32 instruction fetch, load, and store after mret: good"
;;
