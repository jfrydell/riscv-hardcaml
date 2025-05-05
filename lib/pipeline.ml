open Base
open Hardcaml

(* Globally unique identifier for a signal *)
module Id = struct
  type t = int

  (* Can get separate sources of IDs for reproducibility or something... *)
  let id_generator () =
    let curr = ref 0 in
    (fun () -> curr := 1 + !curr; !curr)

  (* ...but would rather just have the one program-wide id lol *)
  let new_id = id_generator ()
end

(* TODO: not sure how best to represent possible registers. Need to be able to get value in register at each stage
as well as specifier for register for bypass logic, with guarantee that they can be computed at decode. For now, I'm
just hardcoding two specifiers and assuming I'll figure out details once I implement actual pipeline construction. *)
type reg =
  | Rs1
  | Rs2
  | Rd

type stage = int
(* Standard 5-stage *)
let sF = 1
let sD = 2
let sX = 3
let sM = 4
let sW = 5

(* Specifies how a wire used in implementing a single stage of a single instruction should be hooked up to the pipeline. *)
type pipe_hook =
  | Read of Id.t * int (* Signal being read and its width for easier wire creation in helpers *)
  | Write of Id.t
  (* | WriteGuarded of Id.t * Signal.t (* Writes conditionally (in addition to full stage guard?) *) *)
  | ReadReg of reg
  (* TODO: maybe separate reading/writing regs? it's only for bypassing, and maybe could define in way independent from main functionality.
  Maybe could read/write values to register file through usual `Read` and `Write` hooks and have `RegRead` give one of those to use as an
  alternative if bypass doesn't match? I guess `RegRead` is similar to `Read` itself then. RegWrite should likely be independent though.
  It's really just a notation to indicate that an instruction has updated a value and the value is available for bypassing. I guess similar
  to Write then. This comment is really just saying read/write reg hooks should only be used for bypassing, not integrating into actual
  register file anywhere. At least for `WriteReg`. Still not sure actually, as need to identify register ID consistently in all (?; at least many)
  stages for bypassing purposes. Leaving for now. *)
  | WriteReg of reg

class pipeline guard = object (self)
  val mutable hooks: (Signal.t * stage * pipe_hook) list = []
  (* Guard gives a wire at each stage determining whether this pipeline is "active."
  Takes in a `pipeline` argument with the pipeline containing this one so that signals can be
  read from it (can't use this pipeline to determine whether itself is active, as theoretically
  signals could be invalid). *)
  val guard: pipeline -> stage -> Signal.t = guard

  (* Adds a wire to the pipeline at the given stage with the given hook *)
  method inject w stage hook = hooks <- (w, stage, hook) :: hooks

  (* Helper to read a value from the pipeline with the given hook, producing a wire. *)
  method take hook stage =
    let width = match hook with
      | Read (_, width) -> width
      | ReadReg _ -> 32
      | Write _ | WriteReg _ -> failwith "Tried to take a write signal from the pipeline"
    in
    let w = Signal.wire width in
    self#inject w stage hook; w

  (* Helper to create a new pipeline signal, write a value to it, and return a hook to read it with *)
  method put_pipe w stage = let i = Id.new_id() in
    self#inject w stage (Write i);
    Read (i, Signal.width w)

  (* Helper to create a new pipeline signal, read a value from it, and return a hook to write it with *)
  method pull_pipe w stage = let i = Id.new_id() in
    self#inject w stage (Read (i, Signal.width w));
    Write i

  (* Integrates several sub-pipelines into this one, assuming only one can be valid at a time.
  TODO: one at a time or all at once? *)
  method integrate (_subs: pipeline list) = () (* TODO *)

end

(* NOTE: originally had separate hooks for global and insn-specific values being read/written. Now think
better to just figure that out when generating pipeline logic based on which pipeline writes value. *)
(* TODO: big problem to solve is how to implement guard logic / muxes when sending signals between pipelines.
One possibility: specify guard with special cross-pipeline write (opposite from above where I got rid of
special cross-pipeline, but I think good because this is other direction (toward local)). Kind of like because
whether something is being written may need complex logic based on arbitrary data from stage.
Probably still want full-pipeline guard though? Seems simpler to reason about. *)
