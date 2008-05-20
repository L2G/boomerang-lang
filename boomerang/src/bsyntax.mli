(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/bsyntax.mli                                                  *)
(* Boomerang abstract syntax interface                                         *)
(* $Id$ *)
(*******************************************************************************)

(** {2 Boomerang Abstract Syntax} *)

(** {2 Identifiers} *)
module Id : sig
  type t = Info.t * string
  (** the type of identifiers; parsing info and a string *)
  
  val mk : Info.t -> string -> t
  (** [mk i s] returns the identifier representing [s] with parsing info [i]. *)

  val info_of_t : t -> Info.t
  (** [info_of_t x] returns the parsing info from [x]. *)

  val string_of_t : t -> string 
  (** [string_of_t x] returns the string that [x] represents. *)
 
  val compare : t -> t -> int
  (** [compare x y] compares [x] and [y] using the standard comparison operator. *)

  val equal : t -> t -> bool
  (** [equal x y] returns [true] iff [x] and [y] represent the same
      string (ignoring parsing information.) *)

  val wild : t
  (** [wild] is a constant representing the "don't care" string "_" *)

  module Set : Set.S with type elt = t
  (** Sets with Id.ts as elements *)
end

(** {2 Qualified Identifiers } *)
module Qid : sig 
  type t = Id.t list * Id.t
  (** the type of identifiers: a list of qualifiers and a
      base identifier *)

  val mk : Id.t list -> Id.t -> t
  (** [mk qs x] returns the qualified identifier with qualifiers [qs]
      and base identifier [x]. *)

  val t_of_id : Id.t -> t
  (** [t_of_id q] returns a qualified identifier with base identifier
      [x] and no qualifiers. *)

  val info_of_t : t -> Info.t
  (** [info_of_t q] returns the parsing info associated with [q]. *)

  val qs_of_t : t -> Id.t list
  (** [qs_of_t q] returns the qualifiers associated with [q]. *)

  val id_of_t : t -> Id.t
  (** [id_of_t q] returns the base identifier associated with [q]. *)

  val string_of_t : t -> string 
  (** [string_of_t q] formats prints [q] as a string. *)
 
  val compare : t -> t -> int
  (** [compare q1 q2] comparse [q1] and [q2] using a dictionary
      ordering on the underlying list of identifiers. *)

  val equal : t -> t -> bool
  (** [equal q1 q2] returns [true] iff [q1] and [q2] represent the same
      qualified identifier (ignoring parsing information). *)

  val id_dot : Id.t -> t -> t
  (** [id_dot x1 q1] returns the qualified identifier representing [x1.q1]. *)

  val splice_id_dot : Id.t -> t -> t
  (** [splice_id_dot x1 q1], where [q1] represents [q11.x12],
      returns the qualified identifier representing [q11.x1.x12] *)

  val t_dot_id : t -> Id.t -> t
  (** [t_dot_id q x] returns the qualified identifier representing
      [q.x]. *)

  val t_dot_t : t -> t -> t
  (** [t_dot_t q1 q2] returns the qualified identifier representing
      [q1.q2]. *)

  val id_prefix : t -> Id.t list -> bool
  (** [id_prefix q xl] returns [true] iff [q] is a prefix of [xl]. *)

  val mk_mod_t : string list -> string -> t
  (** [mk_mod_t ss s] constructs the qualified identifier representing
      [ss] with dummy parsing info. *)

  val mk_native_prelude_t : string -> t
  (** [mk_native_prelude_t s] constructs the qualified identifier representing
      [Native.Prelude.s] with dummy parsing info. *)

  val mk_prelude_t : string -> t
  (** [mk_prelude_t s] constructs the qualified identifier representing
      [Prelude.s] with dummy parsing info. *)

  val mk_core_t : string -> t
  (** [mk_prelude_core_t s] constructs the qualified identifier representing
      [Prelude.Core.s] with dummy parsing info. *)

  val mk_list_t : string -> t
  (** [mk_list_t s] constructs the qualified identifier representing
      [List.s] with dummy parsing info. *) 

  module Env : Env.S with type key = t
  (** Environments with Qid.ts as keys *)

  module Set : Set.S with type elt = t
  (** Sets with Qid.ts as elements *)
end

type blame = Blame of Info.t * bool 
(* blame *)

val mk_blame : Info.t -> blame 
(** [mk_blame i] constructs blame associated with parsing info [i]. *)

val invert_blame : blame -> blame
(** ??? *)

val string_of_blame : blame -> string

type sort = 
    (* base sorts *)
    | SUnit                           (* unit *)
    | SBool                           (* booleans *)
    | SInteger                        (* integers *)
    | SChar                           (* characters *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)

    (* products and datatypes (sums) *)
    | SProduct of sort * sort         (* products *)
    | SData of sort list * Qid.t      (* data types *)

    (* dependent and refinement sorts *)
    | SFunction of Id.t * sort * sort (* dependent functions *)
    | SRefine of Id.t * sort * exp    (* refinements *)

    (* variables and universals *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)

and param = Param of Info.t * Id.t * sort
(** The type of formal parameters: parsing info, a pattern, and a
    sort. *)

and binding = Bind of Info.t * pat * sort option * exp 

and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Qid.t 
    | EOver of Info.t * op * exp list 
    | EFun  of Info.t * param * sort option * exp 
    | ELet  of Info.t * binding * exp 

    (* err... System F rather *)
    | ETyFun of Info.t * Id.t * exp 
    | ETyApp of Info.t * exp * sort

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list * sort

    (* coercions *)
    | ECast of Info.t * sort * sort * blame * exp 
        
    (* unit, strings, ints, characters, character sets *)
    | EUnit    of Info.t  
    | EBoolean of Info.t * bool
    | EInteger of Info.t * int    
    | EChar    of Info.t * Bstring.sym
    | EString  of Info.t * Bstring.t 
    | ECSet    of Info.t * bool * (Bstring.sym * Bstring.sym) list 
(** The type of expression ASTs. *)

and op = 
  | OIter of int * int 
  | ODot
  | OTilde
  | OMinus
  | OBar
  | OAmp
  | OBarBar
  | OAmpAmp
  | ODarrow
  | ODeqarrow
  | OEqual
  | OLt
  | OLeq
  | OGt
  | OGeq
(** The type of overloaded operators. *)

and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * sort option 
  | PVnt of Info.t * Qid.t * pat option 
  | PPar of Info.t * pat * pat
(** The type of pattern ASTs. *)
    
type test_result =
    | TestError
    | TestPrint
    | TestEqual of exp
    | TestSortPrint of sort option
    | TestSortEqual of sort
(** The type of unit test results. *)

type decl = 
    | DLet  of Info.t * binding
    | DType of Info.t * Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod  of Info.t * Id.t * decl list 
    | DTest of Info.t * exp * test_result
(** The type of declaration ASTs. *)

type modl = Mod of Info.t * Id.t * Qid.t list * decl list
(** The type of module ASTs: the name of the module, a list of "open"
    modules, and a list of declarations. *)
          
val (^>) : sort -> sort -> sort
(** [s1 ^> s2] is the function sort from [s1] to [s2]. *)

val info_of_exp : exp -> Info.t
(** [info_of_exp e] returns the parsing info associated to expression [e]. *)
      
val info_of_pat : pat -> Info.t
(** [info_of_pat p] returns the parsing info associated to pattern [p]. *)

val info_of_module : modl -> Info.t
(** [info_of_module m] returns the parsing info associated to module [m]. *)

val id_of_module : modl -> Id.t
(** [id_of_module m] returns the name of module [m]. *)

val sort_of_param : param -> sort
(** [sort_of_param p] returns the sort declared with parameter [p]. *)

val id_of_param : param -> Id.t
(** [sort_of_param p] returns the name of parameter [p]. *)

val pat_of_binding : binding -> pat
(** [pat_of_binding b] returns the name of the variable bound in [b]. *)

val exp_of_binding : binding -> exp
(** [exp_op_binding p] returns the expression of binding [b]. *)

val subst_sort : (Id.t * sort) list -> sort -> sort

(* 
val subst_exp : (Qid.t * exp) list -> exp -> exp 
*)

val subst_exp_in_sort : (Qid.t * exp) list -> sort -> sort

val erase_sort : sort -> sort 

val free_sort_vars : sort -> Id.Set.t

val free_exp_vars_in_sort : sort -> Qid.Set.t
