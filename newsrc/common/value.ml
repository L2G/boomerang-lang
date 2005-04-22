(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                                  *)
(********************************************************************)

(* names, qualified names *)
type n = string
type qn = n list * n

(* useful coercions *)
let n_of_id (_,n) = n
let qn_of_qid (qs,i) = (List.map n_of_id qs, n_of_id i)
let qn_of_n n = ([],n)
let qn_of_id x = qn_of_n (n_of_id x)  
  
(* "dot" two qualified names *)
let dot qno qn2 = match qno with 
    None -> qn2 
  | Some (qs1,n1) -> 
      let (qs2,n2) = qn2 in 
	(qs1@[n1]@qs2, n2) 

let dot_id mo x = dot mo (qn_of_id x)

module EMap = 
  Mapplus.Make(
    struct
      type t = qn
      let compare = compare
      let to_string =
	fun (qs,x) -> List.fold_right
	  (fun h t -> if (t = ".") then h else (h ^ "." ^ t))
	  (qs@[x])
	  "."
    end)
module QNMap = EMap.Map
module QNSet = EMap.KeySet
  
type t = 
    N of Name.t                 (* names *)
  | L of Lens.t                 (* lenses *)      
  | T of Type.t                 (* types *)
  | V of V.t                    (* views *)
  | F of (t -> t)               (* functions *)

(* dummy value, used during initialization of environments
   for recursive definitions *)
let dummy = N("_")
let dummy_rtv = Syntax.SName(Info.bogus), dummy
  
(* an environment is a pair of a naming context and a map from qns to run-time values *)
type rtv = Syntax.sort * t
type env = (rtv ref) QNMap.t

let t_of_rtv (s,v) = v
let sort_of_rtv (s,v) = s
    
(* the empty environment *)
let empty : env = QNMap.empty
  
(* produce env[q:=v]; yields the SAME env *)
let overwrite e q r = 
  try 
    (QNMap.find q e):=r; 
    e
  with Not_found ->
    QNMap.add q (ref r) e

let overwrite_id e x r = overwrite e ([],n_of_id x) r
  
(* produce env[q->v]; yields a NEW env *)
let update e q r = QNMap.add q (ref r) e     
let update_id ev x r = update ev ([],n_of_id x) r
  
(* lookup in an enviroment *)
let lookup e q =
  try Some !(QNMap.find q e)
  with Not_found -> None
let lookup_qid env q = lookup env (qn_of_qid q)
let lookup_id env x = lookup env ([], n_of_id x)
  
let fold = QNMap.fold
let domain = QNMap.domain

(* memoization infrastructure *)
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
    | L l -> let memotable = H.create 1 in	
	(* We use memo information in both directions -- to
	   short-circuit a get when we see it for the second time, and
	   also to avoid computing the put when we can see what its
	   result must be from the GetPut law *)
	L (Lens.native (fun c -> 
		       try
			 H.find memotable (V(c))
		       with Not_found -> begin
			 let a = Lens.get l c in
			   H.add memotable (V(c)) a;
			   a
		       end)
	  (fun a co -> 
	     match co with
		 None -> Lens.put l a None
	       | Some c ->
		   try
		     let a' = H.find memotable (V(c)) in
		       if a' == a then c else Lens.put l a co
		   with Not_found -> Lens.put l a co))
    | F f -> F (let memotable = H.create 1 in
	(fun x -> try
	   H.find memotable x
	 with Not_found -> begin 
	   let fx = f x in
	     H.add memotable x fx;
	     fx
	 end))

(* library *)
let library = ref empty
let get_library () = !library
