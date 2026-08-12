open Core
open Hardcaml

let bram_start = Int32.of_int_exn 0x40000000
let bram_size = 0x8000

let preload_program sim memory =
  Hashtbl.iter_keys memory ~f:(fun addr ->
    if Int32.(addr < bram_start || addr >= bram_start + of_int_exn bram_size)
    then failwithf "interrupt test program outside BRAM: 0x%lx" addr ());
  let words = Int.Table.create () in
  Hashtbl.iteri memory ~f:(fun ~key:addr ~data:byte ->
    let addr = Int32.to_int_exn Int32.(addr - bram_start) in
    let bytes_per_word = Memory.Bus.data_width / 8 in
    let word_address = addr / bytes_per_word in
    let shift = 8 * (addr % bytes_per_word) in
    Hashtbl.update words word_address ~f:(fun current ->
      let current = Option.value current ~default:0L in
      Int64.(current lor shift_left (of_int byte) shift)));
  let main_memory =
    match Cyclesim.lookup_mem_by_name sim "main_memory_bram" with
    | Some memory -> memory
    | None ->
      let names =
        (Cyclesim.traced sim).internal_signals
        |> List.concat_map ~f:(fun signal -> signal.mangled_names)
        |> List.filter ~f:(fun name ->
          String.is_substring name ~substring:"main"
          || String.is_substring name ~substring:"bram")
      in
      raise_s [%message "Could not find test BRAM" (names : string list)]
  in
  Hashtbl.iteri words ~f:(fun ~key:address ~data ->
    Cyclesim.Memory.of_bits
      main_memory
      ~address
      (Bits.of_int64_trunc ~width:Memory.Bus.data_width data))
;;
