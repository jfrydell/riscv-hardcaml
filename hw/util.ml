
open Base
open Hardcaml

(* Matches a scrutinee against a number of cases, each of which provides a list
of wires and an output value. If the scrutinee matches one of the wires in a
case, that case's output is taken.
If the scutinee does not match exactly one case, the output is undefined. *)
let muxmatch ~scrutinee ~cases =
  let open Signal in
  List.map ~f:(fun (patterns, result) ->
    With_valid.{
      value = result;
      valid =
        List.map ~f:(fun p -> scrutinee ==: p) patterns
        |> tree ~arity:2 ~f:(reduce ~f:(|:))
    }
  ) cases
  |> onehot_select
