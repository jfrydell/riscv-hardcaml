open! Base

include Riscv
include Emulate_user
module Random = Fuzz

(* Create an empty processor state, with memory, PC, and regs initialized to 0 *)
let blank = {
    regs = Array.create ~len:32 Int32.zero;
    pc = ref Int32.zero;
    memory = Hashtbl.create (module Int32);
  }

(* Create an initial processor state with the given program to run starting at the given address *)
let init ~insns ~addr =
  if Int32.to_int_exn addr % 4 <> 0 then failwith "unaligned instruction address";
  let memory = Hashtbl.create (module Int32) in
  List.iteri ~f:(fun i insn ->
    store ~memory ~addr:(Int32.(+) addr (Int32.of_int_exn (4*i))) ~size:4 ~value:(to_int32 insn)
  ) insns;
  {
    regs = Array.create ~len:32 Int32.zero;
    pc = ref Int32.zero;
    memory;
  }
