(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(********************************************************************)
(* $Id$ *)

type t = 
    N of Name.t                 (* names *)
  | L of (V.t, V.t) Lens.t      (* lenses *)      
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
let rec dummy ?(msg="") s = match s with 
    Syntax.SName -> N "_"
  | Syntax.SLens -> 
      let error _ = 
	flush stdout; flush stderr;
	prerr_string (Printf.sprintf "Fatal error: dummy %s was not overwritten.\n" msg);
	flush stderr; 
	assert false
      in
	L (Lens.native error error)
  | Syntax.SType -> T (Type.TT (Type.mk_ptype (Type.Empty(Info.dummy))))
  | Syntax.SView -> V (V.empty)
  | Syntax.SArrow(_,rs) -> F (fun _ -> dummy ~msg:msg rs)
  | Syntax.STOper(_,rs) -> T (Type.TT (dummy_ptype s))

and dummy_ptype = function
    Syntax.STOper(_,rs) -> Type.mk_ptype (Type.Fun (Info.dummy, fun _ -> Type.it_of_pt (dummy_ptype rs)))
  | Syntax.SType        -> Type.mk_ptype (Type.Empty(Info.dummy))
  | s                   -> 
      raise (Error.Fatal_error(Printf.sprintf "unexpected type, %s, in Value.dummy_ptype" (Syntax.string_of_sort s)))
      
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
	
