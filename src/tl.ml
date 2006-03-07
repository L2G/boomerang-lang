(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* tl.ml - tree logic                                    *)
(*********************************************************)
(* $Id$ *)

type formula = 
    True of Info.t  
  | Not of Info.t * formula
  | Or of Info.t * formula * formula
  | Empty of Info.t
  | Element of Info.t * Label.t * formula 
  | Sum of Info.t * formula * formula
  | Star of Info.t * formula

(* --------------- constructors for derived forms --------------- *)
let mkAnd i f1 f2 = Not(i,Or(i,Not(i,f1),Not(i,f2)))
let mkImplies i f1 f2 = Or(i,Not(i,f1),f2)
let mkEquivalent i f1 f2 = And(i,mkImplies(i,f1,f2),mkImplies(i,f2,f1))

(* --------------- utility functions --------------- *)
let info_of_formula = function
  | True(i)        -> i
  | Not(i,_)       -> i
  | Or(i,_,_)      -> i
  | Empty(i)       -> i
  | Element(i,_,_) -> i
  | Sum(i,_,_)     -> i
  | Star(i,_)      -> i
 
let format_t t0 = function _ -> Format.printf "UNIMPLEMENTED"
