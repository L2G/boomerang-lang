(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                                  *)
(********************************************************************)

(* backpatch hack *)
let compile_file_impl = ref (fun _ -> ())

(* names, qualified names *)
type n = string
type qn = n list * n

let concat sep list = 
  if (list = []) then "" 
  else List.fold_right 
    (fun h t -> if (t = sep) then h else (h ^ sep ^ t))
    list sep    
let string_of_qn (qs,n) = concat "." (qs@[n])

(* useful coercions *)
let n_of_id (_,n) = n
let qn_of_qid (qs,i) = (List.map n_of_id qs, n_of_id i)
let qn_of_n n = ([],n)
let qn_of_id x = qn_of_n (n_of_id x)  
  
(* "dot" two qualified names *)
let dot (qs1,n1) (qs2, n2) = (qs1@[n1]@qs2, n2)   
let dot_id m x = dot m (qn_of_id x)

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
type env = (rtv ref) QNMap.t

(* some coercions *)
let t_of_rtv (s,v) = v
let sort_of_rtv (s,v) = s
  
(* the empty environment *)
let empty : env = QNMap.empty

(* registry of values *)
let library : env ref = ref empty
let search_path = ref "/Users/nate/shared/harmony4/newsrc/plugins/" (* FIXME!!! *)
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
  
(* get the filename that a module is stored at *)
(* FIXME: simple for now, assumes everything is in the single search path; will generalize later *)
let get_module_prefix q = 
  match q with 
    | ([],_) -> None
    | (n::_,_) -> Some n
	
let get_filename n = (!search_path) ^ (String.uncapitalize n) ^ ".fcl"
  
(* lookup in an enviroment *)
let lookup ev q = try Some !(QNMap.find q ev) with Not_found -> None
let lookup_qid env q = lookup env (qn_of_qid q)
let lookup_id env x = lookup env ([], n_of_id x)
  
(* load a module, if needed *)
let string_of_env ev = 
  let string_of_value = function
      N(n) -> n
    | L(l) -> "<lens>"
    | T(t) -> "<type>"
    | V(t) -> "<view>"      
    | F(f) -> "<fun>"
  in
    (fold 
       (fun q r acc -> 
	  let (s,v) = !r in
	    (concat "" [ string_of_qn q
		       ; "->"
		       ; string_of_value v
		       ; if (acc = "") then "" else ", "
		       ; acc])))
       ev
       ""
let load q = match get_module_prefix q with 
  | None -> ()
  | Some n -> 
      let isloaded = NSet.mem n (!loaded) in
      let isloading = NSet.mem n (!loading) in
      let fn = get_filename n in
	if (NSet.mem n (!loaded)) or (not(Sys.file_exists fn)) then () 
	else if (NSet.mem n (!loading)) then 
	  raise (Error.Sort_error(("Circular module references " ^ n), Info.bogus))
	else 
	  begin
	    loading := (NSet.add n (!loading));
	    prerr_string ("[ loading " ^ fn ^ " ]\n");
	    (!compile_file_impl) fn;
	    loading := (NSet.remove n (!loading));
	    loaded := (NSet.add n (!loaded)) 
	  end

(* lookup in a naming context *)
let lookup_in_ctx nctx q = 
  let rec lookup_in_ctx_aux os q2 =         
    let _ = load q2 in
      match lookup (!library) q2 with	  
	| Some rtv -> Some rtv
	| None -> 
	    match nctx with 
	      | []       -> None
	      | o::orest -> lookup_in_ctx_aux orest (dot o q) 
  in
    lookup_in_ctx_aux nctx q
      
let register q s v = library := (overwrite (!library) q (s,v))
      
let register_native qs ss v = 
  let qid_of_string s = 
    let lexbuf = Lexing.from_string s in
      Parser.qid Lexer.token lexbuf 
  in
  let sort_of_string s = 
    let lexbuf = Lexing.from_string s in
      Parser.sort Lexer.token lexbuf 
  in	
  let q = qn_of_qid (qid_of_string qs) in
  let s = sort_of_string ss in
    register q s v
      
let register_env ev m = QNMap.iter (fun q r -> let (s,v) = !r in register (dot m q) s v) ev  
       
 
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
