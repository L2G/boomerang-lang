(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* error.ml - compiler exceptions                                              *)
(*                                                                             *)
(* $Id: error.ml,v 1.1 2005/01/21 05:00:00 jnfoster Exp $ *)
(*                                                                             *)
(*******************************************************************************)

exception Syntax_error of string * Info.t
exception Parse_error of string * Info.t
exception Type_error of string * Info.t
exception Run_error of string * Info.t

(* helper function for constructing a Focal type error *)
let focal_type_error s = 
  raise (Run_error (("Incorrect type for Focal expression:" ^ s), Info.bogus))
    



