(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* sl.ml - sheaves logic                                 *)
(*********************************************************)
(* $Id$ *)

type element = Location of Info.t * Label.t * formula
and formula = 
    True of Info.t
  | Composition of Info.t * Name.t list * Presburger.formula * element list

(* --------------- utility functions --------------- *)
let info_of_element Location(i,_,_) = i
and info_of_formula = function 
    True(i) -> i
  | Composition(i,_,_,_) -> i

(* --------------- constructors for derived forms --------------- *)
let anyE i = Location(i, CoFinite(i,Name.Set.empty), True(i))
let mkEmpty i = Composition(i,["N"],Presburger.Equal(i,Presburger.Var(i,"N"),Presburger.Const(i,0)), anyE)
let mkTrue i = Composition(i,["N"],Presburger.Equal(i,Presburger.Var(i,"N"),Presburger.Const(i,0)), anyE)
let mkLocation i l f = Composition(i,
                                   ["N1";"N2";"N3"],
                                   
let mkOr f1 f2 = match f1,f2 with
    Composition(i1,n1,p1,e1), Composition(i2,n2,p2,e2) -> 
      (* TODO: check that n1 = n2 and e1 = e2 *)
      Composition(Info.merge_inc i1 i2, 
                  n1, 
                  Presburger.Or(i,p1,p2),
                  e1)
let mkSum f1 f2 = match f1,f2 with
    Composition(i1,n1,p1,e1), Composition(i2,n2,p2,e2) -> 
      (* TODO: check that n1 = n2 and e1 = e2 *)
      Composition(Info.merge_inc i1 i2, 
                  n1, 
                  Presburger.mkSum i p1 p2,
                  e1)

let mkStar f1 = match f1 with
    Composition(i,n,p,e) -> Composition(i,n,Presburger.mkStar p,e)

let mkNot f1 = match f1 with
    Composition(i,n,p,e) -> 
      (* TODO: check that E is a basis *)
      Composition(i,n,Presburger.mkNot p,e)
        
let mkAnd f1 f2 = match f1,f2 with 
    Composition(i1,n1,p1,e1),Composition(i2,n2,p2,e2) -> 
      (* TODO: check that n1 = n2, e1 = e2 and is a basis *)
      Composition(Info.merge_inc i1 i2, 
                  n1,
                  Presburger.mkAnd i p1 p2,
                  e1)
        
 
let format_t t0 = function _ -> Format.printf "UNIMPLEMENTED"
