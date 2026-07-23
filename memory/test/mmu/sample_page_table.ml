open! Core
open! Hardcaml

let root = 0x1000
let page_offset_mask = (1 lsl Mmu.Iface.page_offset_width) - 1
let ppn_mask = (1 lsl (Mmu.Iface.addr_width - Mmu.Iface.page_offset_width)) - 1
let second_level_first = 0x1000000
let second_level_stride = 0x4000
let entries_per_page_table = 1 lsl 10
let second_level_count = entries_per_page_table

let pte ~ppn ~read ~write ~execute ~user ~global =
  let flags =
    1
    lor (read lsl 1)
    lor (write lsl 2)
    lor (execute lsl 3)
    lor (user lsl 4)
    lor (global lsl 5)
  in
  Bits.of_int_trunc ~width:Mmu.Iface.addr_width ((ppn lsl 10) lor flags)
;;

let second_level_base root_index = second_level_first + (root_index * second_level_stride)

let leaf_pte vpn =
  pte ~ppn:(17 * vpn land ppn_mask) ~read:1 ~write:1 ~execute:1 ~user:0 ~global:0
;;

let invalid_lookup address =
  raise_s [%message "invalid sample page-table lookup" (address : int)]
;;

let lookup address =
  let root_offset = address - root in
  if root_offset >= 0 && root_offset < 0x1000 && root_offset land 3 = 0
  then (
    let root_index = root_offset lsr 2 in
    pte
      ~ppn:(second_level_base root_index lsr Mmu.Iface.page_offset_width)
      ~read:0
      ~write:0
      ~execute:0
      ~user:0
      ~global:0)
  else if address >= second_level_first
  then (
    let relative = address - second_level_first in
    let root_index = relative / second_level_stride in
    let table_offset = relative mod second_level_stride in
    if root_index < second_level_count && table_offset < 0x1000 && table_offset land 3 = 0
    then (
      let leaf_index = table_offset lsr 2 in
      let vpn = (root_index lsl 10) lor leaf_index in
      leaf_pte vpn)
    else invalid_lookup address)
  else invalid_lookup address
;;

let physical_ppn vpn = 17 * vpn land ppn_mask

let physical_address va =
  (physical_ppn (va lsr Mmu.Iface.page_offset_width) lsl Mmu.Iface.page_offset_width)
  lor (va land page_offset_mask)
;;

let check_translation ~va ~actual_pa =
  let expected_pa = physical_address va in
  if not (Int.equal actual_pa expected_pa)
  then
    raise_s
      [%message "unexpected translation" (va : int) (actual_pa : int) (expected_pa : int)]
;;
