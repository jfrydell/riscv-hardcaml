open! Core
open Hardcaml

module Update_circuit = struct
  module I = struct
    type 'a t =
      { update : 'a Privileged.Trap_csr.Update.t
      ; old_values : 'a Privileged.Csrs.t
      }
    [@@deriving hardcaml]
  end

  module O = Privileged.Csrs

  let create _scope ({ update; old_values } : _ I.t) =
    Privileged.Trap_csr.update ~update ~old_values
  ;;
end

module Update_sim = Cyclesim.With_interface (Update_circuit.I) (Update_circuit.O)

module Csr_bank_sim =
  Cyclesim.With_interface (Privileged.Csr_bank.I) (Privileged.Csr_bank.O)

module Decode_trap_sim =
  Cyclesim.With_interface (Privileged.Decode_trap.I) (Privileged.Decode_trap.O)

let bits value = Bits.of_int_trunc ~width:32 value
let bit value = Bits.of_bool value

let csrs ?(mstatus = 0) ?(privilege = 0) () =
  { (Privileged.Csrs.Of_bits.zero ()) with
    mstatus = bits mstatus
  ; privilege = bits privilege
  }
;;

let update
  ?(epc = 0)
  ?(trap_value = 0)
  ?(cause = 0)
  ?(trap = true)
  ?(ret = false)
  ?(higher_priv_s = false)
  ()
  : Bits.t Privileged.Trap_csr.Update.t
  =
  { epc = bits epc
  ; trap_value = bits trap_value
  ; cause = bits cause
  ; trap = bit trap
  ; ret = bit ret
  ; higher_priv_s = bit higher_priv_s
  }
;;

let evaluate update old_values =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Update_sim.create (Update_circuit.create scope) in
  Update_circuit.I.iter2
    (Cyclesim.inputs sim)
    { update; old_values }
    ~f:(fun input value -> input := value);
  Cyclesim.cycle sim;
  Privileged.Csrs.map (Cyclesim.outputs sim) ~f:(fun output -> !output)
;;

let check name actual expected =
  let actual = Bits.to_int32_trunc actual in
  let expected = Int32.of_int_exn expected in
  if not (Int32.equal actual expected)
  then raise_s [%message "Unexpected trap CSR value" (name : string) (actual : int32)]
;;

let decode_csrs
  ?(privilege = 0)
  ?(mstatus = 0)
  ?(medeleg = 0)
  ?(mideleg = 0)
  ?(mie = 0)
  ?(mtvec = 0)
  ?(stvec = 0)
  ?(mepc = 0)
  ?(sepc = 0)
  ()
  =
  { (Privileged.Csrs.Of_bits.zero ()) with
    privilege = bits privilege
  ; mstatus = bits mstatus
  ; medeleg = bits medeleg
  ; mideleg = bits mideleg
  ; mie = bits mie
  ; mtvec = bits mtvec
  ; stvec = bits stvec
  ; mepc = bits mepc
  ; sepc = bits sepc
  }
;;

let evaluate_decode
  ?(epc = 0)
  ?(trap_value = 0)
  ?(exception_cause = 0)
  ?(interrupt_cause = 11)
  ?(trap = true)
  ?(interrupt = false)
  ?(mret = false)
  ?(sret = false)
  csrs
  =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Decode_trap_sim.create (Privileged.Decode_trap.create scope) in
  Privileged.Decode_trap.I.iter2
    (Cyclesim.inputs sim)
    { epc = bits epc
    ; trap_value = bits trap_value
    ; exception_cause = bits exception_cause
    ; interrupt_cause = Bits.of_int_trunc ~width:4 interrupt_cause
    ; csrs
    ; trap = bit trap
    ; interrupt = bit interrupt
    ; mret = bit mret
    ; sret = bit sret
    }
    ~f:(fun input value -> input := value);
  Cyclesim.cycle sim;
  Privileged.Decode_trap.O.map (Cyclesim.outputs sim) ~f:(fun output -> !output)
;;

let test_decode_trap () =
  let delegated_exception =
    evaluate_decode
      ~epc:0x44
      ~exception_cause:8
      (decode_csrs ~privilege:0 ~medeleg:(1 lsl 8) ~mtvec:0x100 ~stvec:0x201 ())
  in
  if not (Bits.to_bool delegated_exception.update.higher_priv_s)
  then raise_s [%message "Delegated exception did not target supervisor mode"];
  check "delegated exception handler" delegated_exception.handler_pc 0x200;
  let machine_exception =
    evaluate_decode
      ~exception_cause:8
      (decode_csrs ~privilege:3 ~medeleg:(1 lsl 8) ~mtvec:0x100 ~stvec:0x200 ())
  in
  if Bits.to_bool machine_exception.update.higher_priv_s
  then raise_s [%message "Machine-mode exception was incorrectly delegated"];
  check "machine exception handler" machine_exception.handler_pc 0x100;
  let delegated_interrupt =
    evaluate_decode
      ~interrupt:true
      (decode_csrs
         ~privilege:1
         ~mstatus:(1 lsl 1)
         ~mideleg:(1 lsl 11)
         ~mie:(1 lsl 11)
         ~stvec:0x2f1
         ())
  in
  if not (Bits.to_bool delegated_interrupt.interrupt_enabled)
  then raise_s [%message "Enabled delegated interrupt was masked"];
  if not
       (Int32.equal
          (Bits.to_int32_trunc delegated_interrupt.update.cause)
          (Int32.of_string "0x8000000b"))
  then raise_s [%message "Interrupt cause was not constructed by trap decoding"];
  if not (Bits.to_bool delegated_interrupt.update.higher_priv_s)
  then raise_s [%message "Delegated interrupt did not target supervisor mode"];
  check "delegated vectored handler" delegated_interrupt.handler_pc 0x31c;
  let masked_in_machine =
    evaluate_decode
      ~interrupt:true
      (decode_csrs ~privilege:3 ~mstatus:(1 lsl 3) ~mideleg:(1 lsl 11) ~mie:(1 lsl 11) ())
  in
  if Bits.to_bool masked_in_machine.interrupt_enabled
  then raise_s [%message "Delegated interrupt was not masked in machine mode"];
  let supervisor_return =
    evaluate_decode
      ~trap:false
      ~sret:true
      (decode_csrs ~privilege:1 ~mepc:0x100 ~sepc:0x204 ())
  in
  check "supervisor return PC" supervisor_return.handler_pc 0x204
;;

let test_machine_trap () =
  let preserved_fields =
    (1 lsl 1) (* SIE *)
    lor (1 lsl 5) (* SPIE *)
    lor (1 lsl 8) (* SPP *)
    lor (1 lsl 17) (* MPRV *)
    lor (1 lsl 18) (* SUM *)
    lor (1 lsl 19) (* MXR *)
    lor (1 lsl 20) (* TVM *)
    lor (1 lsl 21) (* TW *)
    lor (1 lsl 22)
    (* TSR *)
  in
  let mstatus = preserved_fields lor (1 lsl 3) (* MIE *) in
  let result =
    evaluate
      (update ~epc:0x105 ~trap_value:0x1234 ~cause:7 ())
      (csrs ~mstatus ~privilege:1 ())
  in
  check
    "machine trap mstatus"
    result.mstatus
    (preserved_fields lor (1 lsl 7) lor (1 lsl 11));
  check "machine trap mepc" result.mepc 0x104;
  check "machine trap mcause" result.mcause 7;
  check "machine trap mtval" result.mtval 0x1234;
  check "machine trap privilege" result.privilege 3
;;

let test_supervisor_trap () =
  let mstatus = (1 lsl 1) lor (1 lsl 17) in
  let result =
    evaluate
      (update ~epc:0x209 ~trap_value:0x5678 ~cause:9 ~higher_priv_s:true ())
      (csrs ~mstatus ~privilege:1 ())
  in
  check "supervisor trap mstatus" result.mstatus ((1 lsl 5) lor (1 lsl 8) lor (1 lsl 17));
  check "supervisor trap sepc" result.sepc 0x208;
  check "supervisor trap scause" result.scause 9;
  check "supervisor trap stval" result.stval 0x5678;
  check "supervisor trap privilege" result.privilege 1
;;

let test_machine_return () =
  let mstatus =
    (1 lsl 7) (* MPIE *) lor (1 lsl 11) (* MPP = S *) lor (1 lsl 17)
    (* MPRV, cleared when returning below M *)
  in
  let result =
    evaluate (update ~trap:false ~ret:true ()) (csrs ~mstatus ~privilege:3 ())
  in
  check "machine return mstatus" result.mstatus ((1 lsl 3) lor (1 lsl 7));
  check "machine return privilege" result.privilege 1
;;

let test_supervisor_return () =
  let mstatus = (1 lsl 5) (* SPIE *) lor (1 lsl 17) (* MPRV *) in
  let result =
    evaluate
      (update ~trap:false ~ret:true ~higher_priv_s:true ())
      (csrs ~mstatus ~privilege:1 ())
  in
  check "supervisor return mstatus" result.mstatus ((1 lsl 1) lor (1 lsl 5));
  check "supervisor return privilege" result.privilege 0
;;

let test_csr_bank_trap_write () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Csr_bank_sim.create (Privileged.Csr_bank.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  Cyclesim.cycle sim;
  check "initial privilege" !(outputs.csrs.privilege) 3;
  inputs.trap_write.epc := bits 0x305;
  inputs.trap_write.trap_value := bits 0x4321;
  inputs.trap_write.cause := bits 11;
  inputs.trap_write.trap := Bits.vdd;
  Cyclesim.cycle sim;
  if not (Bits.to_bool !(outputs.write_done))
  then raise_s [%message "Trap CSR write did not signal completion"];
  check "bank mepc" !(outputs.csrs.mepc) 0x304;
  check "bank mcause" !(outputs.csrs.mcause) 11;
  check "bank mtval" !(outputs.csrs.mtval) 0x4321;
  check "bank privilege" !(outputs.csrs.privilege) 3;
  inputs.trap_write.trap := Bits.gnd;
  inputs.trap_write.ret := Bits.vdd;
  Cyclesim.cycle sim;
  if not (Bits.to_bool !(outputs.write_done))
  then raise_s [%message "Trap return CSR write did not signal completion"];
  check "bank mret mstatus" !(outputs.csrs.mstatus) (1 lsl 7);
  check "bank mret privilege" !(outputs.csrs.privilege) 3
;;

let () =
  test_decode_trap ();
  test_machine_trap ();
  test_supervisor_trap ();
  test_machine_return ();
  test_supervisor_return ();
  test_csr_bank_trap_write ();
  Stdio.print_string "Trap CSR updates: all modes good\n"
;;
