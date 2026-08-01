open! Core
open! Hardcaml
open Signal

let addr_width = 32

module Access_type = struct
  module Cases = struct
    type t =
      | Instruction
      | Load
      | Store
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  include Enum.Make_binary (Cases)

  (** Privilege level to use for address translation. Translation mode only applies to S
      and U (01 and 00). *)
  let effective_priv ~(state : _ State.t) access =
    Of_signal.match_
      access
      [ Instruction, state.fetch_priv
      ; Load, state.load_store_priv
      ; Store, state.load_store_priv
      ]
  ;;
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; state : 'a State.t
    ; va : 'a With_valid.t [@bits addr_width]
    (** Request a new address to be translated. Once a request is received, this valid bit
        is ignored until [pa.valid] goes high (but a request arriving the cycle [pa.valid]
        is high will be processed). *)
    ; access_type : 'a Access_type.t
    ; walker_from_mem : 'a Memory_bus.From_mem.t
    (** Response from memory to page table walker. *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a Iface.Translation.t
    (** A translated address. Held until the cycle after a new request comes in on
        [va.valid]. *)
    ; walker_to_mem : 'a Memory_bus.To_mem.t (** Request from PT walker to read PTE. *)
    }
  [@@deriving hardcaml]
end

(** No-translation mode, simply buffering the input VA into the output PA. For debugging,
    can insert a varying stall before output. *)
let bare scope ({ clocking; va; state; _ } : _ I.t) =
  let scope = Scope.sub_scope scope "bare" in
  let%hw stall_cycles = wire 2 in
  (* Don't accept new request while stalling previous (or if it's not valid). *)
  let%hw stall = stall_cycles <>:. 0 in
  let%hw accept = ~:stall &&: va.valid in
  let%hw.Iface.Translation.Of_signal result =
    let%hw pa = Types.Clocking.reg clocking ~enable:accept va.value in
    { pa = mux2 stall (zero addr_width) pa
    ; io = pa.:(addr_width - 1)
    ; valid = vdd
    ; stall
    }
  in
  (* Cycle through 4 values of stall amount if we're in none_debug mode. *)
  let%hw phase =
    Types.Clocking.reg_fb ~width:2 ~enable:accept clocking ~f:(fun p -> p +:. 1)
  in
  let%hw stall_amount =
    mux phase @@ List.map ~f:(of_unsigned_int ~width:2) [ 1; 2; 0; 0 ]
    |> mux2
         (State.Translation_mode.Binary.Of_signal.is state.translation_mode Bare)
         (zero 2)
  in
  (* Set cycle count when we accept request, then decrement each cycle. *)
  stall_cycles
  <-- Types.Clocking.reg
        clocking
        (mux2 accept stall_amount @@ mux2 stall (stall_cycles -:. 1) (zero 2));
  (* Output zeros when stalling for testing. *)
  let%hw pa = mux2 stall (zero 32) result.pa in
  ({ pa; io = pa.:(addr_width - 1); valid = ~:stall &&: result.valid; stall }
   : _ Iface.Translation.t)
;;

let create scope ({ clocking; va; state; access_type; walker_from_mem } : _ I.t) =
  (* Track whether the currently-arriving VA should be translated; held through
     transaction if it takes more than a cycle. (Note that the rest of [state]
     is registered within the TLB module). *)
  let%hw translating = wire 1 in
  let%hw.Iface.Translation.Of_signal bare_translation =
    bare
      scope
      { clocking
      ; va = { va with valid = va.valid &&: ~:translating }
      ; state
      ; access_type
      ; walker_from_mem
      }
  in
  let%hw.Iface.Tlb_response.Of_signal tlb_from_walker =
    Iface.Tlb_response.Of_signal.wires ()
  in
  let%hw.Tlb.O.Of_signal tlb_out =
    Tlb.hierarchical
      ~scope
      { clocking
      ; state
      ; va = { va with valid = va.valid &&: translating }
      ; from_walker = tlb_from_walker
      }
  in
  let%hw.Walker.O.Of_signal walker_out =
    Walker.hierarchical
      ~scope
      { clocking; state; from_tlb = tlb_out.to_walker; read_from_mem = walker_from_mem }
  in
  Iface.Tlb_response.Of_signal.assign tlb_from_walker walker_out.to_tlb;
  (* While a request is stalled, hold translation state constant. *)
  let%hw stall = tlb_out.result.stall ||: bare_translation.stall in
  (* Change request routing only when not stalled to avoid having two requests
     in-flight at once. *)
  let%hw translating_unlatched =
    State.Translation_mode.Binary.Of_signal.is state.translation_mode Sv32
    &&: ~:(Access_type.effective_priv ~state access_type).:(1)
  in
  translating
  <-- Types.Clocking.cut_through_reg ~enable:~:stall clocking translating_unlatched;
  (* While [translating] changes as soon as we aren't stalled, so a new request is
     routed correctly, the output still comes from the previously-selected
     module until we actually get a new valid request. *)
  let%hw translating_output = Types.Clocking.reg clocking ~enable:va.valid translating in
  let%hw.Iface.Translation.Of_signal result =
    let%hw.Iface.Translation.Of_signal selected_result =
      Iface.Translation.Of_signal.mux2 translating_output tlb_out.result bare_translation
    in
    { selected_result with io = selected_result.pa.:(addr_width - 1); stall }
  in
  ({ result; walker_to_mem = walker_out.read_to_mem } : _ O.t)
;;

let hierarchical =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical create
;;
