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

  val mk_list_t : string -> t
  (** [mk_list_t s] constructs the qualified identifier representing
      [List.s] with dummy parsing info. *) 

  module Env : Env.S with type key = t
  (** Environments with Qid.ts as keys *)

  module Set : Set.S with type elt = t
  (** Sets with Qid.ts as elements *)
end

type sort = 
    (* base types *)
    | SUnit                           (* unit *)
    | SBool                           (* booleans *)
    | SInteger                        (* integers *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)
    (* dependent types *)
    | SFunction of Id.t * sort * sort (* dependent functions *)
    | SData of sort list * Qid.t      (* data types *)
    | SProduct of sort * sort         (* products *)
    | SRefine of Id.t * sort * exp    (* refinements *)
    (* polymorphism *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)

and param_desc = Param of Id.t * sort
(** The type of formal parameters: parsing info, an identifier, and a
    sort. *)

and binding_desc = Bind of Id.t * sort option * exp 

and exp_desc = 
    (* lambda calculus *)
    | EApp of exp * exp 
    | EVar of Qid.t 
    | EOver of op * exp list
    | EFun of param * sort option * exp 
    | ELet of binding * exp 

    (* err... System F rather *)
    | ETyFun of Id.t * exp 
    | ETyApp of exp * sort

    (* with products, case *)
    | EPair of exp * exp 
    | ECase of exp * (pat * exp) list 
        
    (* unit, strings, ints, character sets *)
    | EUnit  
    | EBoolean of bool
    | EInteger of int    
    | EString of Bstring.t 
    | ECSet of bool * (Bstring.sym * Bstring.sym) list 
(** The type of expression "descriptions". *)

and op = 
  | OIter of int * int 
  | ODot
  | OBar
  | OTilde

and pat_desc = 
  | PWld 
  | PUnt 
  | PBol of bool
  | PInt of int
  | PStr of string
  | PVar of Id.t 
  | PVnt of Qid.t * pat option 
  | PPar of pat * pat
(** The type of pattern "descriptions". *)

and ('a,'b) syntax = 
    { info: Info.t;
      desc: 'a;
      mutable annot: 'b }
(** A generic type for abstract syntax: parsing info, description, and
    mutable annotation data to be filled in by the checker. This type
    is loosely based on similar types in Galax. *)

and exp = (exp_desc, sort option) syntax
(** The type of expressions: an expression description annotated with
    an optional sort and a list of sort variables. *)

and pat = (pat_desc,sort option) syntax          
(** The type of patterns: an pattern description annotated with
    an optional sort. *)

and param = (param_desc, unit) syntax

and binding = (binding_desc,unit) syntax
(** The type of bindings: a binding description annotated with
    an optional sort. *)
    
val mk_exp : Info.t -> exp_desc -> exp
(** [mk_exp i d] constructs an [exp] with parsing info [i] and
    description [d]. *)

val mk_annot_exp : Info.t -> exp_desc -> sort -> exp
(** [mk_annot_exp i e s] constructs an [exp] with parsing info [i] and
    description [e], annotated with [s]. *)

val mk_pat : Info.t -> pat_desc -> pat
(** [mk_pat i p] constructs a [pat] with parsing info [i] and
    description [p]. *)

val mk_annot_pat : Info.t -> pat_desc -> sort -> pat
(** [mk_annot_pat i p s] constructs a [pat] with parsing info [i] and
    description [p], annotated with [s]. *)

val mk_param : Info.t -> param_desc -> param
(** [mk_param i p] constructs a [param] with parsing info [i] and
    description [p]. *)

val mk_binding : Info.t -> binding_desc -> binding
(** [mk_binding i b] constructs a [binding] with parsing info [i] and
    description [b]. *)

val mk_annot_pat : Info.t -> pat_desc -> sort -> pat
(** [mk_checked_pat i p s] constructs a [pat] with parsing info [i],
    description [p], and annotated with sort [s]. *)

type test_result =
    | TestValue of exp
    | TestError
    | TestShow
    | TestLensType of (exp option * exp option)
(** The datatype representing unit test results. *)

type decl_desc = 
    | DLet of binding  
    | DType of Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod of Id.t * decl list 
    | DTest of exp * test_result
(** The type of declaration "descriptions" *)

and decl = (decl_desc,unit) syntax 
(** The type of declarations: a description and an optional sort. *)
          
val mk_decl : Info.t -> decl_desc -> decl
(** [mk_decl i d] constructs the [decl] with parsing info [i] and
    description [d]. *)
          
type modl_desc = Mod of Id.t * Id.t list * decl list
(** The type of module "descriptions": the name of the module, a list
    of "open" modules, and a list of declarations. *)

and modl = (modl_desc,unit) syntax
(** The type of modules: a module description with no annotation. *)
          
val mk_mod : Info.t -> modl_desc -> modl
(** [mk_mod i m] constructs a [modl] with parsing info [i] and
description [m]. *)
          
val (^>) : sort -> sort -> sort
(** [s1 ^> s2] is the function sort from [s1] to [s2]. *)

val free_sort_vars : sort -> Id.Set.t
(** [free_sort_vars s] returns the set of free sort variables in [s]. *)

val free_vars : exp -> Qid.Set.t 
  (** [free_vars e] returns the set of free variables in expression [e]. *)

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

val id_of_binding : binding -> Id.t
(** [sort_of_param p] returns the name of the variable bound in [b]. *)

val exp_of_binding : binding -> exp
(** [sort_of_param p] returns the expression of binding [b]. *)

