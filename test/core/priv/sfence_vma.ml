open! Core
open Riscv_isa.Insn
open Riscvemulate.State
open Trap_test_utils

let root_page_table = 0x1000
let leaf_page_table = 0x2000
let old_code_page = 0x3000
let new_code_page = 0x4000
let old_data_page = 0x5000
let new_data_page = 0x6000
let trap_vector = 0x800

let code_update_va = 0x3004
let data_update_va = 0x3008
let code_va = 0x1000
let data_va = 0x2000
let page_table_va = 0x3000
let old_data_value = 0x11111111
let new_data_value = 42

let slli ~rd ~rs1 imm = IntImm (Sll, { rd; rs1; imm = Int32.of_int_exn imm })

let setup_page_table memory =
  (* The root maps VPN[1] = 0 to the leaf table. *)
  store
    ~memory
    ~addr:(Int32.of_int_trunc root_page_table)
    ~size:4
    ~value:(Int32.of_int_exn (((leaf_page_table lsr 12) lsl 10) lor 1));
  (* VA 0 is the S-mode code page, VA 0x1000 is initially old code, VA 0x2000
     is initially read-only data, and VA 0x3000 maps the leaf table so S mode
     can update the PTEs. *)
  List.iter
    [ 0, 0, 0x0b
    ; 1, 3, 0x0b
    ; 2, 5, 0x03
    ; 3, leaf_page_table lsr 12, 0x07
    ]
    ~f:(fun (vpn, ppn, permissions) ->
      store
        ~memory
        ~addr:(Int32.of_int_trunc (leaf_page_table + (vpn * 4)))
        ~size:4
        ~value:(Int32.of_int_exn ((ppn lsl 10) lor permissions)))
;;

let m_mode_program =
  let open Csr_address in
  [ (* satp = Sv32, with the root page table at PPN 1. *)
    addi ~rd:1 ~rs1:0 1
  ; addi ~rd:2 ~rs1:0 1
  ; slli ~rd:2 ~rs1:2 31
  ; IntReg (Add, { rd = 1; rs1 = 1; rs2 = 2 })
  ; csrw Privileged.Csrs.addresses.satp 1
    (* mtvec = 0x800, and mstatus.MPP = S. *)
  ; addi ~rd:2 ~rs1:0 1
  ; slli ~rd:2 ~rs1:2 11
  ; csrw mtvec 2
  ; addi ~rd:3 ~rs1:0 0x100
  ; csrw mepc 3
  ; csrw mstatus 2
  ; Mret
  ]
;;

let s_mode_prefix =
  [ (* x3 = the virtual address through which the leaf table is updated. *)
    addi ~rd:3 ~rs1:0 (page_table_va lsr 12)
  ; slli ~rd:3 ~rs1:3 12
    (* x6 = the data VA and x7 = the value written by the remapped code. *)
  ; addi ~rd:6 ~rs1:0 (data_va lsr 13)
  ; slli ~rd:6 ~rs1:6 13
  ; addi ~rd:7 ~rs1:0 new_data_value
    (* x4 = the new executable code PTE: PPN 4, R|X|V. *)
  ; addi ~rd:4 ~rs1:0 1
  ; slli ~rd:4 ~rs1:4 12
  ; addi ~rd:4 ~rs1:4 0x0b
    (* x5 = the new writable data PTE: PPN 6, R|W|V. *)
  ; addi ~rd:5 ~rs1:0 3
  ; slli ~rd:5 ~rs1:5 11
  ; addi ~rd:5 ~rs1:5 7
    (* Visit the old code mapping before changing its PTE. *)
  ; Jal { rd = 0; imm = Int32.of_int_exn (code_va - 0x12c) }
  ]
;;

let post_old_mapping =
  [ (* Visit the old read-only data mapping before changing its PTE. *)
    Load (Word, Signed, { rd = 8; rs1 = 6; imm = Int32.zero })
  ; Store
      ( Word
      , { rs1 = 3
        ; rs2 = 4
        ; imm = Int32.of_int_exn (code_update_va - page_table_va)
        } )
  ; Store
      ( Word
      , { rs1 = 3
        ; rs2 = 5
        ; imm = Int32.of_int_exn (data_update_va - page_table_va)
        } )
    (* The fence is deliberately at VA 0xffc, so its sequential PC is the
       first instruction in the remapped code page. *)
  ; Jal { rd = 0; imm = Int32.of_int_exn (0xffc - 0x20c) }
  ]
  @ List.init ((0xff8 - 0x210) / 4 + 1) ~f:(fun _ -> nop)
  @ [ SfenceVma { rs1 = 0; rs2 = 0 } ]
;;

let trap_handler = [ csrr 31 Csr_address.mcause; halt ]

let memory =
  let memory =
    program
      [ 0, m_mode_program
      ; 0x100, s_mode_prefix
      ; 0x200, post_old_mapping
      ; old_code_page, [ Jal { rd = 0; imm = Int32.of_int_exn (-0xe00) } ]
      ; new_code_page,
        [ Store (Word, { rs1 = 6; rs2 = 7; imm = Int32.zero })
        ; addi ~rd:30 ~rs1:0 new_data_value
        ; halt
        ]
      ; trap_vector, trap_handler
      ]
  in
  setup_page_table memory;
  store
    ~memory
    ~addr:(Int32.of_int_exn old_data_page)
    ~size:4
    ~value:(Int32.of_int_exn old_data_value);
  memory
;;

let () =
  let sim, _commits =
    run_hardware
      ~max_cycles:10_000
      ~done_:(fun sim -> Int32.equal (Sim.Cpu.regs sim).(30) (int new_data_value))
      memory
  in
  Sim.Cpu.flush sim;
  let regs = Sim.Cpu.regs sim in
  check_reg regs 8 old_data_value;
  check_reg regs 30 new_data_value;
  check_reg regs 31 0;
  let stored_value =
    load
      ~memory:(Sim.Cpu.memory sim)
      ~addr:(int new_data_page)
      ~size:4
      ~extend:Unsigned
  in
  if not (Int32.equal stored_value (int new_data_value))
  then
    let expected = int new_data_value in
    raise_s
      [%message
        "SFENCE.VMA did not use the remapped data page"
          (stored_value : int32)
          (expected : int32)]
  else Stdio.print_endline "SFENCE.VMA test: good"
;;
