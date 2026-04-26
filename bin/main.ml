open! Core
open Hardcaml

let scope = Scope.create ~flatten_design:false ()

let circuit =
  let open Riscvhardcaml in
  let module Circuit = Circuit.With_interface (Cpu.I) (Cpu.O) in
  Circuit.create_exn ~name:"cpu" (Cpu.create scope)
;;

let database = Scope.circuit_database scope
let rtl = Rtl.create ~database Verilog [ circuit ]
let rtl_string = Rope.to_string (Rtl.full_hierarchy rtl)
let () = Out_channel.write_all "_output/cpu.v" ~data:rtl_string

(* Debug CPU with waveform *)
let program =
  Riscvemulate.(
    (* [ IntImm (Add, { rd = 1; rs1 = 0; imm = Int32.of_int_exn 7 }) *)
    (* ; IntReg (Add, { rd = 2; rs1 = 1; rs2 = 1 }) *)
    (* ; Store (Half, { rs1 = 2; rs2 = 1; imm = Int32.of_int_exn (-14) }) *)
    (* ; Load (Half, Unsigned, { rd = 3; rs1 = 2; imm = Int32.of_int_exn (-14) }) *)
    [ AuiPc { rd = 3; imm = Int32.zero }
    ; AuiPc { rd = 1; imm = Int32.of_int_exn 61440 }
    ; Lui { rd = 3; imm = Int32.zero }
    ; IntImm (Xor, { rd = 2; rs1 = 2; imm = Int32.of_int_exn 301 })
    ; Load (Byte, Unsigned, { rd = 0; rs1 = 0; imm = Int32.of_int_exn 382 })
    ; Jalr { rd = 1; rs1 = 2; imm = Int32.of_int_exn (-1) }
    ])
;;

let memory = (Riscvemulate.init ~insns:program ~addr:Int32.zero).memory

(* Reproduce fuzz test. *)
let memory, trace =
  ignore (program, memory);
  let insn_stream = Fuzz.insn_stream ~reg_max:4 ~seed:91691 in
  Fuzz.generate_program ~insn_count:10 ~insn_stream ~filter:(function
    | IntImm (Add, _) -> true
    | Load _ | Store _ | Branch _ -> true
    | _ -> false)
;;

let () = print_s [%message (trace : Riscvemulate.insn list)]
let emulator = Riscvemulate.with_mem memory

let () =
  List.iter trace ~f:(fun i ->
    print_s
      [%message
        "Instruction vs emulator"
          (i : Riscvemulate.insn)
          (Riscvemulate.current_pc_insn emulator : Riscvemulate.insn)];
    print_s [%message "PC" (!(emulator.pc) : int32)];
    Riscvemulate.step emulator;
    print_s [%message "Reg" (emulator.regs.(3) : int32)];
    print_endline "")
;;

let sim, waves = Sim.Cpu.create ~memory ~config:(Cyclesim.Config.trace `Everything) Waves

let _ =
  for _ = 1 to 30 do
    Sim.Cpu.cycle_external sim;
    Cyclesim.cycle sim.sim;
    (* Print outputs *)
    Stdio.print_s
      (Riscvhardcaml.Cpu.O.sexp_of_t
         (fun b -> sexp_of_int (Bits.to_int_trunc !b))
         (Cyclesim.outputs sim.sim));
    (* Print reg *)
    Stdio.printf "%d\n\n" (Int32.to_int_exn (Sim.Cpu.regs sim).(3))
  done
;;

(* let _ = Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int mem)
let _ = Stdio.printf "mem b0: %d\n" (Riscvemulate.load ~memory:mem ~addr:Int32.zero ~size:2 ~extend:Riscvemulate.Unsigned |> Int32.to_int_exn) *)
let term = true
let _ = if term then Hardcaml_waveterm_interactive.run waves
