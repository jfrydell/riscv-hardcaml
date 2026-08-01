open! Core
open! Hardcaml
open Signal

(* TODO: change physical addr width to 34? *)
let addr_width = 32
let asid_width = 9

module Translation_mode = struct
  module Cases = struct
    type t =
      | Bare
      | Bare_debug
      | Sv32
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  module Binary = Enum.Make_binary (Cases)
  module One_hot = Enum.Make_one_hot (Cases)
end

type 'a t =
  { translation_mode : 'a Translation_mode.Binary.t
  ; asid : 'a [@bits asid_width]
  ; page_table_root : 'a [@bits addr_width]
  ; fetch_priv : 'a [@bits 2] (** Effective privilege level for instruction fetches. *)
  ; load_store_priv : 'a [@bits 2]
  (** Effective privilege level for loads and stores (not fetches). *)
  ; executable_readable : 'a (** MXR bit, allowing reads to access execute-only pages. *)
  ; supervisor_user_access : 'a
  (** SUM bit, allowing (effective) S-mode loads and stores (but not fetches) to access
      U-mode pages. *)
  }
[@@deriving hardcaml]

let of_csrs (csrs : _ Privileged.Csrs.t) =
  let sv32 = csrs.satp.:(31) in
  let asid = csrs.satp.:[30, 22] in
  let page_table_root = csrs.satp.:[19, 0] @: zero 12 in
  (* Instruction fetches always use current privilege level. *)
  let fetch_priv = csrs.privilege.:[1, 0] in
  (* If MPRV is set, then loads/stores use MPP privilege. *)
  let mstatus = Privileged.Csrs.Mstatus.Fields.of_register csrs.mstatus in
  let load_store_priv = mux2 mstatus.mprv mstatus.mpp fetch_priv in
  { translation_mode =
      Translation_mode.Binary.Of_signal.(mux2 sv32 (of_enum Sv32) (of_enum Bare))
  ; asid
  ; page_table_root
  ; fetch_priv
  ; load_store_priv
  ; executable_readable = mstatus.mxr
  ; supervisor_user_access = mstatus.sum
  }
;;
