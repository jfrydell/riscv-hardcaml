open Hardcaml

let circuit =
  let open Signal in
  let clock = input "clock" 1
  and reset = input "reset" 1
  and insn_in = input "insn" 32
  and data_in = input "mem_data" 32 in
  let pc_out, mem_addr, mem_access, mem_size, mem_data = Riscvhardcaml.Cpu.cpu ~clock ~reset ~insn_in ~data_in in
  let pc_out = output "pc" pc_out in
  let mem_addr = output "mem_addr" mem_addr in
  let mem_access = output "mem_access" mem_access in
  let mem_size = output "mem_size" mem_size in
  let mem_data = output "store_data" mem_data in
  Circuit.create_exn ~name:"cpu" [pc_out; mem_addr; mem_access; mem_size; mem_data]

let () = Rtl.print Verilog circuit
