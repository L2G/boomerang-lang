(***************************************)
(* The Harmony Project                 *)
(* harmony@lists.seas.upenn.edu        *)
(*                                     *)
(* env.ml - Focal environments         *)
(***************************************)
(* $Id $ *)

(* finite maps whose keys are Syntax.qids *)
module EMap = 
  Mapplus.Make(
    struct
      type t = Syntax.qid
      let compare = Syntax.qid_compare
      let to_string = Syntax.string_of_qid 
    end)
module QidMap = EMap.Map
module QidSet = EMap.KeySet

type 'a t = ('a ref) QidMap.t
    
(* the empty environment *)
let empty () : 'a t = QidMap.empty

(* produce env[q->v]; yields a NEW env *)
let update ev q r = (QidMap.add q (ref r) ev)
  
(* produce env[q:=v]; yields the SAME env 
   unless q is not in env; then it uses update to give a NEW env *)
let overwrite ev q r =
  try 
    (QidMap.find q ev):=r; 
    ev 
  with Not_found ->
    update ev q r
      
let lookup ev q = 
  try 
    Some !(QidMap.find q ev)
  with Not_found -> None
    
let to_string ev string_of_r = 
  Misc.curlybraces 
    (QidMap.fold (fun q r acc -> 
		    Printf.sprintf "\n\t%s=%s%s%s"		      
		      (Syntax.string_of_qid q)
		      (string_of_r (!r))
		      (if (acc = "") then "" else ", ")
		      acc)
       ev "")

let iter f = QidMap.iter (fun q rvr -> f q (!rvr))
