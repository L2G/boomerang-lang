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
type blame = Blame of Info.t  

val mk_blame : Info.t -> blame 
(** [mk_blame i] constructs blame associated with parsing info [i]. *)

val invert_blame : blame -> blame
(** ??? *)

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
    | SData of sort list * Bident.Qid.t      (* data types *)

    (* dependent and refinement sorts *)
    | SFunction of Bident.Id.t * sort * (int * exp) list * sort (* dependent functions, with allocation *)
    | SRefine of Bident.Id.t * sort * exp                       (* refinements *)

    (* variables and universals *)
    | SVar of Bident.Id.t                    (* variables *)
    | SForall of Bident.Id.t * sort          (* universals *)

and param = Param of Info.t * Bident.Id.t * sort
(** The type of formal parameters: parsing info, a pattern, and a
    sort. *)

and binding = Bind of Info.t * pat * sort option * exp 

and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Bident.Qid.t 
    | EOver of Info.t * op * exp list 
    | EFun  of Info.t * param * sort option * exp 
    | ELet  of Info.t * binding * exp 

    (* err... System F rather *)
    | ETyFun of Info.t * Bident.Id.t * exp 
    | ETyApp of Info.t * exp * sort

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list * sort

    (* coercion and holes! *)
    | ECast    of Info.t * sort * sort * blame * exp 
    | ELoc     of Info.t * int
    | EAlloc   of Info.t * (int * exp) list * exp

    (* unit, strings, ints, characters, character sets *)
    | EUnit    of Info.t  
    | EBoolean of Info.t * string option (* None = true ; Some s = false with cex s *)
    | EInteger of Info.t * int    
    | EChar    of Info.t * char
    | EString  of Info.t * string
    | ECSet    of Info.t * bool * (char * char) list 
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
  | PVar of Info.t * Bident.Id.t * sort option 
  | PVnt of Info.t * Bident.Qid.t * pat option 
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
    | DType of Info.t * Bident.Id.t list * Bident.Qid.t * (Bident.Id.t * sort option) list 
    | DMod  of Info.t * Bident.Id.t * decl list 
    | DTest of Info.t * exp * test_result
(** The type of declaration ASTs. *)

type modl = Mod of Info.t * Bident.Id.t * Bident.Qid.t list * decl list
(** The type of module ASTs: the name of the module, a list of "open"
    modules, and a list of declarations. *)
          
val (^>) : sort -> sort -> sort
(** [s1 ^> s2] is the function sort from [s1] to [s2]. *)

val (^*) : sort -> sort -> sort
(** [s1 ^* s2] is the product sort between [s1] and [s2]. *)

val info_of_exp : exp -> Info.t
(** [info_of_exp e] returns the parsing info associated to expression [e]. *)
      
val info_of_pat : pat -> Info.t
(** [info_of_pat p] returns the parsing info associated to pattern [p]. *)

val info_of_module : modl -> Info.t
(** [info_of_module m] returns the parsing info associated to module [m]. *)

val id_of_module : modl -> Bident.Id.t
(** [id_of_module m] returns the name of module [m]. *)

val sort_of_param : param -> sort
(** [sort_of_param p] returns the sort declared with parameter [p]. *)

val id_of_param : param -> Bident.Id.t
(** [sort_of_param p] returns the name of parameter [p]. *)

val pat_of_binding : binding -> pat
(** [pat_of_binding b] returns the name of the variable bound in [b]. *)

val exp_of_binding : binding -> exp
(** [exp_op_binding p] returns the expression of binding [b]. *)

val exp_of_blame : Info.t -> blame -> exp
(** [exp_of_blame i b] builds an expression that compiles to a run-time representation of [b]. *)

val sl_of_svl : Bident.Id.t list -> sort list
(** [sl_of_svl l] turns a list of sort variables l into a list of sorts *)
