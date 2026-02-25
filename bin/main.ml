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
  Riscvemulate.
    [ IntImm (Add, { rd = 1; rs1 = 0; imm = Int32.of_int_trunc 100 })
    ; IntImm (Add, { rd = 6; rs1 = 0; imm = Int32.of_int_trunc 1000 })
    ; IntReg (Sub, { rd = 1; rs1 = 6; rs2 = 1 })
    ; nop
    ; nop
    ; nop
    ; nop
    ; nop
    ; nop
    ]
;;

let mem = (Riscvemulate.init ~insns:program ~addr:Int32.zero).memory

(* (* Reproduce fuzz test. *) *)
(* let mem, trace = *)
(*   ignore (program, mem); *)
(*   let insn_stream = Fuzz.insn_stream ~reg_max:8 ~seed:38501 in *)
(*   Fuzz.generate_program ~insn_count:140 ~insn_stream *)
(* ;; *)
(**)
(* let emulator = Riscvemulate.with_mem mem *)
(**)
(* let () = *)
(*   List.iter trace ~f:(fun i -> *)
(*     print_s *)
(*       [%message *)
(*         (i : Riscvemulate.insn) *)
(*           (Riscvemulate.current_pc_insn emulator : Riscvemulate.insn)]; *)
(*     print_s [%message (!(emulator.pc) : int32)]; *)
(*     Riscvemulate.step emulator) *)
(* ;; *)

let sim, waves = Sim.Cpu.create ~config:(Cyclesim.Config.trace `Everything) Waves

let _ =
  for _ = 1 to 10 do
    Sim.Cpu.cycle_external sim mem;
    Cyclesim.cycle sim;
    (* Print outputs *)
    Stdio.print_s
      (Riscvhardcaml.Cpu.O.sexp_of_t
         (fun b -> sexp_of_int (Bits.to_int_trunc !b))
         (Cyclesim.outputs sim));
    (* Print reg *)
    Stdio.printf "%d\n" (Int32.to_int_exn (Sim.Cpu.regs sim).(7))
  done
;;

(* let _ = Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int mem)
let _ = Stdio.printf "mem b0: %d\n" (Riscvemulate.load ~memory:mem ~addr:Int32.zero ~size:2 ~extend:Riscvemulate.Unsigned |> Int32.to_int_exn) *)
let term = false
let _ = if term then Hardcaml_waveterm_interactive.run waves
