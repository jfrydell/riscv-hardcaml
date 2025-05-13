open Hardcaml

let circuit =
  let open Riscvhardcaml in
  Circuit.create_with_interface (module Cpu.I) (module Cpu.O) ~name:"cpu" Cpu.cpu

let () = Rtl.print Verilog circuit
