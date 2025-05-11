open Hardcaml

let circuit =
  let open Signal in
  let clock = input "clock" 1
  and reset = input "reset" 1
  and insn_in = input "insn" 32 in
  let pc_out = Riscvhardcaml.Cpu.cpu ~clock ~reset ~insn_in in
  let pc_out = output "pc" pc_out in
  Circuit.create_exn ~name:"cpu" [pc_out]

let () = Rtl.print Verilog circuit
