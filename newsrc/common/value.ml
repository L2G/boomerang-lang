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
module NSet = Set.Make(
  struct
    type t = n
    let compare = compare
  end)

let fold = QNMap.fold
  
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
type env = (rtd ref) QNMap.t

(* some coercions *)
let t_of_rtv (s,v) = v
let sort_of_rtv (s,v) = s
  
(* the empty environment *)
let empty : env = QNMap.empty

(* registry of values *)
let library = ref empty
let search_path = ref "/home/nate/shared/harmony4/newsrc/plugins/" (* FIXME!!! *)
let loaded = ref NSet.empty
let loading = ref NSet.empty
let get_library () = !library
  
(* produce env[q:=v]; yields the SAME env if q already there *)
let overwrite e q r = 
  try 
    (QNMap.find q e):=r; 
    e
  with Not_found ->
    QNMap.add q (ref r) e      
      
(* produce env[q->v]; yields a NEW env *)
let update e q r = QNMap.add q (ref r) e     
  
(* helpers *)
let overwrite_id e x r = overwrite e ([],n_of_id x) r
let update_id ev x r = update ev ([],n_of_id x) r
let lookup_qid env q = lookup env (qn_of_qid q)
let lookup_id env x = lookup env ([], n_of_id x)
  
(* get the filename that a module is stored at *)
(* FIXME: simple for now, assumes everything is in the single search path; will generalize later *)
let get_module_prefix q = 
  match q with 
    | ([],_) -> assert false
    | (n::_,_) -> n
	
let get_filename n = (!search_path) ^ (String.uncapitalize (string_of_n n)) ^ ".fcl"

(* lookup in an enviroment *)
let lookup ev q = Some !(QNMap.find q e) with Not_found -> None
  
(* load a module, if needed *)
let load q = 
  let n = get_module_prefix q in
    if (not (Set.mem (!loaded) n)) then
      begin 
	let fn = get_filename n in
	let _ = if (not(Sys.file_exists fn)) then 
	  raise (Error.Sort_error(("Module " ^ (Pretty.string_of_qn q) ^ " does not exist"), Info.bogus))
	in
	let _ = loading := (NSet.add n) in
	let _ = prerr_string "[loading " ^ fn ^ "]" in
	let lex = Lexing.from_channel (open_in fn) in
	let ast = Parser.modl Lexer.token lexbuf in
	let _ = close_in fn in
	let ast = Checker.sc_module n ast in 
	let _ = Compiler.compile_module ast in
	let _ = loading := (NSet.remove n) in
	let _ = loaded := (NSet.add n) in
      end	
	()
        
(* lookup in a naming context *)
let lookup_in_ctx ev nctx q = 
  let rec lookup_in_ctx_aux os q2 =     
    match lookup ev q2 with
      | Some rtv -> Some rtv
      | None -> match os with 
	  | []       -> None
	  | o::orest -> lookup_in_ctx_aux orest (dot (Some o) q) 
  in
    lookup_in_ctx_aux ev os q
      
let register_native qs nv ss = 
  let q = qn_of_qid (qid_of_string qs) in
  let s = sort_of_string ss in
  let _ = overwrite (!pre_library) q (s,nv) in
    ()  
      
(*** REGISTRY ***)
(* parse various bits of syntax *)
let sort_of_string s = 
  let lexbuf = Lexing.from_string s in
    Parser.sort Lexer.token lexbuf 

let qid_of_string s = 
  let lexbuf = Lexing.from_string s in
    Parser.qid Lexer.token lexbuf 

 
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
