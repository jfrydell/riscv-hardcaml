(* Mutable state and memory operations for the RISC-V emulator. *)

open! Base
module Insn = Riscv_isa.Insn

type state =
  { regs : int32 Array.t
  ; csrs : int32 Array.t
  ; privilege : int ref
  ; pc : int32 ref
  ; memory : (int32, int) Hashtbl.t
  (** Byte-addressed physical memory (range 0-255), defaulting to zero. *)
  ; address_translation : int32 -> int32 [@sexp.opaque]
  (** Translation applied to instruction and data addresses before accessing [memory]. *)
  }
[@@deriving sexp_of]

let regs { regs; _ } = regs
let csrs { csrs; _ } = csrs
let privilege { privilege; _ } = !privilege
let pc { pc; _ } = !pc
let memory { memory; _ } = memory

(* Load a value from memory, extending as necessary to 32 bits. *)
let load ~memory ~addr ~size ~extend =
  let load_byte addr = Hashtbl.find memory addr |> Option.value ~default:0 in
  let bytes = List.init size ~f:(fun n -> load_byte Int32.(addr + of_int_exn n)) in
  let value =
    List.fold_right bytes ~init:0 ~f:(fun b v -> (256 * v) + b) |> Int32.of_int_trunc
  in
  match extend with
  | Insn.Signed ->
    Int32.shift_right (Int32.shift_left value (32 - (8 * size))) (32 - (8 * size))
  | Insn.Unsigned -> value
;;

(* Make implicitly-zero addresses visible in the memory table after a load. *)
let touch ~memory ~addr ~size =
  let touch_byte addr =
    Hashtbl.update memory addr ~f:(function
      | Some b -> b
      | None -> 0)
  in
  let _ = List.init size ~f:(fun n -> touch_byte Int32.(addr + of_int_exn n)) in
  ()
;;

(* Store a value to memory. *)
let store ~memory ~addr ~value ~size =
  let value = Int32.to_int_exn value in
  for b = 0 to size - 1 do
    Hashtbl.set
      memory
      ~key:Int32.(addr + of_int_exn b)
      ~data:((value lsr (8 * b)) land 255)
  done
;;

let create ?(address_translation = Fn.id) memory =
  let csrs = Array.create ~len:4096 Int32.zero in
  csrs.(Insn.Csr_address.misa) <- Int32.of_string "0x40140100";
  { regs = Array.create ~len:32 Int32.zero
  ; csrs
  ; privilege = ref 3
  ; pc = ref Int32.zero
  ; memory
  ; address_translation
  }
;;

(* Create a processor state with the given initial memory contents (copied). *)
let with_mem ?address_translation memory =
  create ?address_translation (Hashtbl.copy memory)
;;

(* Create an empty processor state, with memory, PC, and registers initialized to 0. *)
let blank () = with_mem (Hashtbl.create (module Int32))

(* Create an initial processor state with the given program to run at [addr]. *)
let init ~insns ~addr =
  if Int32.to_int_exn addr % 4 <> 0 then failwith "unaligned instruction address";
  let memory = Hashtbl.create (module Int32) in
  List.iteri
    ~f:(fun i insn ->
      store
        ~memory
        ~addr:(Int32.( + ) addr (Int32.of_int_exn (4 * i)))
        ~size:4
        ~value:(Insn.to_int32 insn))
    insns;
  create memory
;;
