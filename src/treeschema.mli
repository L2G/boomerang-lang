(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id$ *)

(* Schemas for trees.  Loosely based on tree logic -- 
   see "A Logic You Can Count On", POPL '04. *)

type t 
val hash : t -> int
val format_t : t -> unit

(* -------------- compilation state ------------------- *)

(* These functions are called from compiler.ml when tree
   schemas are being compiled from abstract syntax trees
   into Treeschema.t's. *)

(* Mark some variables that we are in the process of defining *)
val mark_tvars : (string * Info.t) list -> unit

(* Finalize the current definition *)
val finalize : unit -> unit

(* [update x t] sets the definition of the schema named [x] 
   in the (single global) environment to [t]. *)
val update : string -> t -> unit

(* -------------- constructors --------------- *)
val mk_any       : t
val mk_empty     : t
val mk_atom      : Name.t -> t -> t
val mk_atom_cats : Name.t list -> t -> t
val mk_atom_alts : Name.t list -> t -> t
val mk_cat       : t list -> t
val mk_union     : t list -> t
val mk_var       : string -> t
val mk_wild      : Name.Set.t -> int -> bool -> t -> t
val mk_neg       : t -> t
val mk_isect     : t list -> t
val mk_diff      : t -> t -> t 
val mk_nil       : t
val mk_cons      : t -> t -> t

(* Generate a schema for lists of the given schema. *)
val mk_list  : Info.t -> t -> t

(* Generate a singleton schema describing exactly the given tree *)
val t_of_tree : Tree.t -> t 

(* -------------- operations --------------- *)
val is_empty : t -> bool
val equivalent : t -> t -> bool
val subschema : t -> t -> bool
val member : Tree.t -> t -> bool

(* NATE: Few more comments... *)
val project : Name.t -> t -> t 
val project_all : t -> t option
val inject : t -> t -> t
val inject_map : t -> t Name.Map.t -> t
val restrict : Name.Set.t -> t -> t*t

(* [dom_member d t] is true iff the set [d] is the domain
   of some tree belonging to [t]. *)
val dom_member : Name.Set.t -> t -> bool

(* [is_list t] yields true if [t] is equivalent to List(s) for some s *)
val is_list  : t -> bool

val total_member : int ref
