open! Core
open! Hardcaml

let addr_width = 32
let asid_width = 16

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
  }
[@@deriving hardcaml]
