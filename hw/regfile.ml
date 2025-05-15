(* Simple 32-register (1 zero), 2 read port, 1 write port register file.
Written on rising edge (intended to be aligned with pipeline latches for easier simulation),
writes are forwarded to read to give write-before-read semantics anyway. *)

open! Base
open Hardcaml

module I = struct
  type 'a t =
    { rd: 'a [@bits 5]
    ; rdval: 'a [@bits 32]
    ; rs: 'a array [@bits 5] [@length 2]
    ; clock: 'a
    ; reset: 'a
  }
  [@@deriving hardcaml]
end
module O = struct
  type 'a t = { rsval: 'a [@bits 32] }
  [@@deriving hardcaml]
end

(* TODO: reset? need to replace `multiport_memory` with set of registers *)
let create I.{rd; rdval; rs; clock; reset=_reset} =
  let open Signal in
  let write_port = Write_port.{
    write_clock = clock;
    write_address = rd -- "regfileRD";
    write_data = rdval -- "regfileRDVAL";
    write_enable = rd <>: zero 5;
  } in
  let rsval_raw = multiport_memory ~name:"regfile" ~write_ports:[|write_port|] ~read_addresses:rs 32 in
  let rsval_fwd = Array.map2_exn rs rsval_raw ~f:(fun rs regval -> mux2 ((rs ==: rd) &: (rd <>: zero 5)) rdval regval) in
  O.{
    rsval = rsval_fwd
  }
