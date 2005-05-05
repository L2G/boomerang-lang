(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* error.ml - compiler exceptions                                              *)
(*                                                                             *)
(* $Id$ *)
(*                                                                             *)
(*******************************************************************************)

exception Syntax_error of string * Info.t
exception Parse_error of string * Info.t
exception Sort_error of string * Info.t
exception Run_error of string

(* helper function for constructing a Focal type error *)
let focal_type_error s = 
  raise (Run_error ("Incorrect type for Focal expression:" ^ s))
    



