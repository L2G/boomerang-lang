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

(* REGISTRY VALUES *)
type rv = Syntax.sort * Value.t
let dummy_rv = Syntax.SName(Info.bogus), Value.dummy
let make_rv s v = (s,v)
let value_of_rv (s,v) = v
let sort_of_rv (s,v) = s

(* ENVIRONMENTS *)
(* Maps whose keys are Syntax.qids *)
module EMap = 
  Mapplus.Make(
    struct
      type t = Syntax.qid
      let compare = Syntax.qid_compare
      let to_string = Syntax.string_of_qid 
    end)
module QidMap = EMap.Map
module QidSet = EMap.KeySet

type env = (rv ref) QidMap.t
let empty : env = QidMap.empty

(* produce env[q->v]; yields a NEW env *)
let update oev get_ev put_ev q r = put_ev oev (QidMap.add q (ref r) (get_ev oev))
  
(* produce env[q:=v]; yields the SAME env *)
let overwrite oev get_ev put_ev q r = 
  try 
    (QidMap.find q (get_ev oev)):=r; 
    oev 
  with Not_found ->
    raise (Error.Run_error("Tried to overwrite a non-existent mapping"))

let lookup oev get_ev q = try Some !(QidMap.find q (get_ev oev)) with Not_found -> None

(* env pretty printer *)
let string_of_env ev = 
  Pretty.curlybraces 
    (QidMap.fold (fun q r acc -> 
	     let (s,v) = !r in
	       (Pretty.concat "" [ "\n\t"
				 ; Syntax.string_of_qid q
				 ; " -> "
				 ; Value.string_of_t v
				 ; " : "
				 ; Syntax.string_of_sort s
				 ; if (acc = "") then "" else ", "
				 ; acc]))
       ev
       "")

(* LIBRARY *)
(* Sets whose elements are Syntax.ids *)
module IdSet = Set.Make(
  struct
    type t = Syntax.id
    let compare = Syntax.id_compare
  end)
  
(* the library's state *)
let qid_of_string s =     
  let lexbuf = Lexing.from_string s in
    Parser.qid Lexer.token lexbuf 

let pre_ctx = List.map qid_of_string ["Pervasives.Native"; "Pervasives.Derived"]

let library : env ref = ref empty
let loaded = ref IdSet.empty
let search_path = ref ["/Users/nate/shared/harmony4/newsrc/plugins/"]

let get_library () = !library

let register q r = library := (QidMap.add q (ref r) (!library))
let register_env ev m = QidMap.iter (fun q r -> register (Syntax.dot m q) !r) ev
  
let register_native qs ss v = 
  let sort_of_string s = 
    let lexbuf = Lexing.from_string s in
      Parser.sort Lexer.token lexbuf 
  in	
  let q = qid_of_string qs in
  let s = sort_of_string ss in
    register q (s,v)
  
(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match q with 
    | ([],_) -> None
    | (n::_,_) -> Some n
	
let find_filename n = 
  let fn = (String.uncapitalize n) ^ ".fcl" in
  let rec loop ds = match ds with
    | []    -> None
    | d::drest -> 
	let full_fn = d ^ fn in
	  if (Sys.file_exists full_fn) then Some full_fn
	  else loop drest
  in
    loop (!search_path)
      
(* load modules dynamically *)
(* backpatch hack *)
let compile_file_impl = ref (fun _ _ -> ())  
let load q = match get_module_prefix q with 
  | None -> ()
  | Some n -> 
      let ns = Syntax.string_of_id n in
      let fno = find_filename ns in	
	if (IdSet.mem n (!loaded)) then ()	  
	else 
	  begin
	    match fno with 
	      | None -> ()
	      | Some fn ->
		  prerr_string ("[ loading " ^ fn ^ " ]\n");
		  loaded := (IdSet.add n (!loaded)); 
		  (!compile_file_impl) fn n
	  end

(* lookup in a naming context *)
let lookup_library2 nctx q = 
  let rec lookup_library_aux os q2 =       
    let sq = Syntax.string_of_qid in
    let _ = load q2 in
      match lookup (!library) (fun x -> x) q2 with
	| Some r -> Some r
	| None -> match os with 
	    | []       -> None
	    | o::orest -> lookup_library_aux orest (Syntax.dot o q) 
  in
    lookup_library_aux nctx q

let lookup_library oev ev_of_oev ctx_of_oev q = 
  let ev = ev_of_oev oev in
    match lookup ev (fun x -> x) q with
      | Some r -> Some r
      | None -> lookup_library2 (ctx_of_oev oev) q

      
      
