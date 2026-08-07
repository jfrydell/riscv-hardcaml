open! Core
open! Hardcaml

let root = 0x1000
let page_offset_mask = (1 lsl Mmu.Iface.page_offset_width) - 1
let ppn_mask = (1 lsl (Mmu.Iface.addr_width - Mmu.Iface.page_offset_width)) - 1
let second_level_first = 0x1000000
let second_level_stride = 0x4000
let entries_per_page_table = 1 lsl 10
let superpage_first_root_index = entries_per_page_table / 2
let second_level_count = superpage_first_root_index

type permissions =
  { valid : bool
  ; read : bool
  ; write : bool
  ; execute : bool
  ; user : bool
  }
[@@deriving sexp_of]

let pte ~ppn ({ valid; read; write; execute; user } : permissions) ~global =
  let flags =
    Bool.to_int valid
    lor (Bool.to_int read lsl 1)
    lor (Bool.to_int write lsl 2)
    lor (Bool.to_int execute lsl 3)
    lor (Bool.to_int user lsl 4)
    lor (global lsl 5)
  in
  Bits.of_unsigned_int ~width:Mmu.Iface.addr_width ((ppn lsl 10) lor flags)
;;

let second_level_base root_index = second_level_first + (root_index * second_level_stride)

let permissions_from_key key =
  { valid = true
  ; read = key land 1 <> 0
  ; write = key land 2 <> 0
  ; execute = key land 4 <> 0
  ; user = key land 8 <> 0
  }
;;

let normal_page_permissions vpn =
  let permissions = permissions_from_key vpn in
  if permissions.read || permissions.write || permissions.execute
  then permissions
  else if vpn land 0x10 = 0
  then { permissions with valid = false }
  else { permissions with valid = false; read = true; write = true; execute = true }
;;

let superpage_permissions root_index =
  let permissions = permissions_from_key root_index in
  (* [V=1,R=W=X=0] denotes a pointer at level one. Keep this range entirely terminal
     while retaining all other permission combinations, including forbidden W=1,R=0. *)
  if permissions.read || permissions.write || permissions.execute
  then permissions
  else { permissions with execute = true }
;;

let is_superpage vpn = vpn lsr Mmu.Iface.vpn_part_width >= superpage_first_root_index

let permissions vpn =
  if is_superpage vpn
  then superpage_permissions (vpn lsr Mmu.Iface.vpn_part_width)
  else normal_page_permissions vpn
;;

let permission_bits ({ read; write; execute; user; _ } : permissions) =
  Bool.to_int read
  lor (Bool.to_int write lsl 1)
  lor (Bool.to_int execute lsl 2)
  lor (Bool.to_int user lsl 3)
;;

let normal_page_ppn vpn = 17 * vpn land ppn_mask

let superpage_base_ppn root_index =
  (17 * root_index land (entries_per_page_table - 1)) lsl Mmu.Iface.vpn_part_width
;;

let physical_ppn vpn =
  if is_superpage vpn
  then
    superpage_base_ppn (vpn lsr Mmu.Iface.vpn_part_width)
    lor (vpn land (entries_per_page_table - 1))
  else normal_page_ppn vpn
;;

let leaf_pte vpn = pte ~ppn:(normal_page_ppn vpn) (permissions vpn) ~global:0

let superpage_pte root_index =
  pte ~ppn:(superpage_base_ppn root_index) (superpage_permissions root_index) ~global:0
;;

let invalid_lookup address =
  raise_s [%message "invalid sample page-table lookup" (address : int)]
;;

let lookup address =
  let root_offset = address - root in
  if root_offset >= 0 && root_offset < 0x1000 && root_offset land 3 = 0
  then (
    let root_index = root_offset lsr 2 in
    if root_index >= superpage_first_root_index
    then superpage_pte root_index
    else
      pte
        ~ppn:(second_level_base root_index lsr Mmu.Iface.page_offset_width)
        { valid = true; read = false; write = false; execute = false; user = false }
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

let physical_address va =
  (physical_ppn (va lsr Mmu.Iface.page_offset_width) lsl Mmu.Iface.page_offset_width)
  lor (va land page_offset_mask)
;;

let access_fault ~va ~access_type ~effective_priv ~supervisor_user_access =
  let permissions = permissions (va lsr Mmu.Iface.page_offset_width) in
  let valid = permissions.valid && (permissions.read || not permissions.write) in
  let access_allowed =
    match access_type with
    | Mmu.Translate.Access_type.Cases.Load -> permissions.read
    | Store -> permissions.write
    | Instruction -> permissions.execute
  in
  let privilege_allowed =
    match effective_priv with
    | 0 -> permissions.user
    | 1 -> (not permissions.user) || supervisor_user_access
    | _ -> true
  in
  not (valid && access_allowed && privilege_allowed)
;;

let check_translation ~va ~actual_pa =
  let expected_pa = physical_address va in
  if not (Int.equal actual_pa expected_pa)
  then
    raise_s
      [%message "unexpected translation" (va : int) (actual_pa : int) (expected_pa : int)]
;;
