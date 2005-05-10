(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(********************************************************************)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
  
type t = 
    N of Name.t                 (* names *)
  | L of Lens.t                 (* lenses *)      
  | T of Type.t                 (* types *)
  | V of V.t                    (* views *)
  | F of (t -> t)               (* functions *)

(* pretty print *)
let rec string_of_t = function
    N(n) -> n
  | L(l) -> "<lens>"
  | T(t) -> Type.string_of_t t
  | V(t) -> "<view>"      
  | F(f) -> "<fun>"
      
(* dummy value generator *)
let rec dummy s = match s with 
    Syntax.SName(_) -> N "_"
  | Syntax.SLens(_) -> L (Lens.native (fun _ -> assert false) (fun _ _ -> assert false))
  | Syntax.SType(i) -> T (Type.TT(Type.Empty(i)))
  | Syntax.SView(_) -> V (V.empty)
  | Syntax.SArrow(_,_,rs) -> F (fun _ -> dummy rs)
  | Syntax.STOper(i,_,rs) -> T (Type.TT(dummy_ptype s))

and dummy_ptype = function
    Syntax.STOper(i,_,rs) -> Type.Fun (i, fun _ -> dummy_ptype rs)
  | Syntax.SType(i)       -> Type.Empty(i)
  | _                     -> assert false
      
(* MEMOIZATION *)
type thist = t (* HACK! *)
module H =
  Hashtbl.Make(
    struct
      type t = thist
      let equal = (==)                                (* Use physical equality test *)
      let hash o = Hashtbl.hash (Obj.magic o : int)   (* Hash on physical addr *)
    end)
    
let memoize v =
  match v with 
      N _ -> v
    | T _ -> v
    | V _ -> v
    | L l -> L (Lens.memoize_lens l) 
    | F f -> F (let memotable = H.create 1 in
		  (fun x -> try
		     H.find memotable x
		   with Not_found -> begin 
		     let fx = f x in
		       H.add memotable x fx;
		       fx
		   end))
