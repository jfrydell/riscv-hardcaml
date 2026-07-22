open! Core
open! Hardcaml

module Translation_mode = struct
  module Cases = struct
    type t =
      | None
      | None_debug
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  module Binary = Enum.Make_binary (Cases)
  module One_hot = Enum.Make_one_hot (Cases)
end

type 'a t =
  { translation_mode : 'a Translation_mode.Binary.t
  }
[@@deriving hardcaml]
