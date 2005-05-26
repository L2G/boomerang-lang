(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* error.ml - compiler exceptions                                              *)
(*                                                                             *)
(*******************************************************************************)
(* $Id$ *)

type i = Info.t
exception Syntax_error of i * string * string 
exception Sort_error of i * string * string
exception Run_error of i * string * string 
exception Fatal_error of string 

    



