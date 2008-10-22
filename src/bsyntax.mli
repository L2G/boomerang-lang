(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/src/bsyntax.mli                                                 *)
(* Boomerang abstract syntax interface                                        *)
(* $Id$ *)
(******************************************************************************)

open Bident

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
    | SChar                           (* chars *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)

    (* products and sums *)
    | SProduct of sort * sort         (* products *)
    | SData of sort list * Qid.t (* data types *)

    (* dependent function types, with allocation *)
    | SFunction of Id.t * sort * (int * exp) list * sort 
    | SRefine of Id.t * sort * exp    (* refinement types *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)
 
(* parameters *)
and param = Param of Info.t * Id.t * sort

(* variable bindings *)
and binding = Bind of Info.t * pat * sort option * exp 

(* expressions *)
and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Qid.t 
    | EOver of Info.t * op * exp list 
    | EFun  of Info.t * param * sort option * exp 
    | ELet  of Info.t * binding * exp 

    (* or rather... System F *)
    | ETyFun of Info.t * Id.t * exp 
    | ETyApp of Info.t * exp * sort

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list * sort

    (* casts, locations, and allocations *)
    | ECast    of Info.t * sort * sort * blame * exp
    | ELoc     of Info.t * int
    | EAlloc   of Info.t * (int * exp) list * exp
        
    (* unit, strings, ints, character sets *)
    | EUnit    of Info.t  
    | EInteger of Info.t * int    
    | EChar    of Info.t * char
    | EString  of Info.t * string
    | ECSet    of Info.t * bool * (char * char) list 

    (* booleans with counter examples *)
    (* None ~ true; Some s ~ false with counterexample s *)
    | EBoolean of Info.t * exp option 
(** Expression abstract syntax. *)

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
(** Overloaded operator abstract syntax. *)

and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PCex of Info.t * pat
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * sort option 
  | PVnt of Info.t * Qid.t * pat option 
  | PPar of Info.t * pat * pat
(** Pattern abstract syntax. *)
    
type test_result =
    | TestError
    | TestPrint
    | TestEqual of exp
    | TestSortPrint of sort option
    | TestSortEqual of sort
(** Unit test abstract syntax. *)

type decl = 
    | DLet  of Info.t * binding
    | DType of Info.t * Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod  of Info.t * Id.t * decl list 
    | DTest of Info.t * exp * test_result
(** Declaration abstract syntax. *)

type modl = Mod of Info.t * Id.t * Qid.t list * decl list
(** Module abstract syntax: the name of the module, a list of "open"
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

val sl_of_svl : Bident.Id.t list -> sort list
(** [sl_of_svl l] converts the sort variable list [l] to a sort list. *)
