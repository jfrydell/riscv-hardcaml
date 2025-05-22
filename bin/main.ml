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
    [ Lui { rd = 5; imm = Int32.of_int_trunc (-816635904) }
    ; AuiPc { rd = 1; imm = Int32.of_int_trunc 328196096 }
    ; Store (Word, { rs1 = 0; rs2 = 5; imm = Int32.of_int_exn 1023 })
    ; nop
    ; nop
    ; nop
    ; nop
    ]
;;

let mem = (Riscvemulate.init ~insns:program ~addr:Int32.zero).memory
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
    Stdio.printf "%d\n" (Int32.to_int_exn (Sim.Cpu.regs sim).(5))
  done
;;

(* let _ = Stdio.print_s (Hashtbl.sexp_of_t sexp_of_int32 sexp_of_int mem)
let _ = Stdio.printf "mem b0: %d\n" (Riscvemulate.load ~memory:mem ~addr:Int32.zero ~size:2 ~extend:Riscvemulate.Unsigned |> Int32.to_int_exn) *)
let term = true
let _ = if term then Hardcaml_waveterm_interactive.run waves
