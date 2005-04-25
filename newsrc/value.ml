(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                                  *)
(********************************************************************)
  
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
  | T(t) -> Type.string_of_type t
  | V(t) -> "<view>"      
  | F(f) -> "<fun>"

(* dummy value, used during initialization of environments
   for recursive definitions *)
let dummy = N("_")
   
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
