(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(********************************************************************)
(* $Id$ *)

(** Run-time representations of Focal values *)

(** The type of run-time representations *)
type t = 
    N of Name.t                 (** names *)
  | L of (V.t, V.t) Lens.t      (** lenses *)      
  | T of Type.t                 (** types *)
  | V of V.t                    (** views *)
  | F of (t -> t)               (** functions *)

val string_of_t : t -> string
(** [string_of_t v] returns a formatted string representing [v] *)

val dummy : ?msg:string -> Syntax.sort -> t
(** [dummy s] returns a dummy value of sort [s]. For sorts, see [Syntax.sort].
  @param the first optional argument is used as an error message for a dummy lens;
  its default is "". *)

val memoize : t -> t
(** Nate, could you fill in that one please ? I'm not sure of what it does *)
