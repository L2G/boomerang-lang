(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.ml - library of Focal values                            *)
(*                                                                  *)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                                  *)
(********************************************************************)

open Pretty

(* backpatch hack *)
let compile_file_impl = ref (fun _ -> ())

module EMap = 
  Mapplus.Make(
    struct
      type t = Syntax.qid
      let compare = Syntax.qid_compare
      let to_string = Syntax.string_of_qid 
    end)
module QidMap = EMap.Map
module QidSet = EMap.KeySet
module IdSet = Set.Make(
  struct
    type t = Syntax.id
    let compare = Syntax.id_compare
  end)

let fold = QidMap.fold

(* run-time values, environments *)
type rtv = Syntax.sort * Value.t
let dummy_rtv = Syntax.SName(Info.bogus), Value.dummy
  
type env = (rtv ref) QidMap.t

(* the empty environment *)
let empty : env = QidMap.empty
    
(* some coercions *)
let value_of_rtv (s,v) = v
let sort_of_rtv (s,v) = s

(* registry of values *)
let library : env ref = ref empty
let search_path = ref "/Users/nate/shared/harmony4/newsrc/plugins/" (* FIXME!!! *)
let loaded = ref IdSet.empty
let loading = ref IdSet.empty
let get_library () = !library
  
(* produce env[q:=v]; yields the SAME env if q already there *)
let overwrite e q r = 
  try 
    (QidMap.find q e):=r; 
    e
  with Not_found ->
    QidMap.add q (ref r) e      
      
(* produce env[q->v]; yields a NEW env *)
let update e q r = QidMap.add q (ref r) e     
  
(* helpers *)
let overwrite_id e x r = overwrite e (Syntax.qid_of_id x) r
let update_id ev x r = update ev (Syntax.qid_of_id x) r
  
(* get the filename that a module is stored at *)
(* FIXME: simple for now, assumes everything is in the single search path; will generalize later *)
let get_module_prefix q = 
  match q with 
    | ([],_) -> None
    | (n::_,_) -> Some n
	
let get_filename n = (!search_path) ^ (String.uncapitalize n) ^ ".fcl"
  
(* lookup in an enviroment *)
let lookup ev q = try Some !(QidMap.find q ev) with Not_found -> None
let lookup_id env x = lookup env ([], x)
  
(* load a module, if needed *)
let load q = match get_module_prefix q with 
  | None -> ()
  | Some n -> 
      let isloaded = IdSet.mem n (!loaded) in
      let isloading = IdSet.mem n (!loading) in
      let ns = Syntax.string_of_id n in
      let fn = get_filename ns in
	if (IdSet.mem n (!loaded)) or (not(Sys.file_exists fn)) then () 
	else if (IdSet.mem n (!loading)) then 
	  raise (Error.Sort_error(("Circular module references " ^ ns), Info.bogus))
	else 
	  begin
	    loading := (IdSet.add n (!loading));
	    prerr_string ("[ loading " ^ fn ^ " ]\n");
	    (!compile_file_impl) fn;
	    loading := (IdSet.remove n (!loading));
	    loaded := (IdSet.add n (!loaded)) 
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
	      | o::orest -> lookup_in_ctx_aux orest (Syntax.dot o q) 
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
  let q = qid_of_string qs in
  let s = sort_of_string ss in
    register q s v
      
let register_env ev m = QidMap.iter (fun q r -> let (s,v) = !r in register (Syntax.dot m q) s v) ev  


(* pretty printer *)
and string_of_env ev = 
  Pretty.curlybraces 
    (fold (fun q r acc -> 
	     let (s,v) = !r in
	       (Pretty.concat "" [ "\n\t"
				 ; Syntax.string_of_qid q
				 ; "->"
				 ; Value.string_of_t v
				 ; ":"
				 ; Syntax.string_of_sort s
				 ; if (acc = "") then "" else ", "
				 ; acc]))
       ev
       "")
    
