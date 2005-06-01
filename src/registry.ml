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

(* DEBUGGING *)

let registry_debug = Prefs.createBool "debug-registry" false 
  "print debugging information about Focal registry"
  "print debugging information about Focal registry"

let debug s_thk = 
  if Prefs.read registry_debug 
  then 
    begin 
      prerr_string (sprintf "%s\n" (s_thk ()));
      flush stderr
    end

(* REGISTRY VALUES *)
type rv = Syntax.sort * Value.t
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
let empty () : env = QidMap.empty

(* produce env[q->v]; yields a NEW env *)
let update oev get_ev put_ev q r = put_ev oev (QidMap.add q (ref r) (get_ev oev))
  
(* produce env[q:=v]; yields the SAME env 
   unless q is not in env; then it uses update to give a NEW env
*)
let overwrite oev get_ev put_ev q r = 
  try 
    (QidMap.find q (get_ev oev)):=r; 
    oev 
  with Not_found ->
    update oev get_ev put_ev q r
      
let lookup oev get_ev q = try Some !(QidMap.find q (get_ev oev)) with Not_found -> None

(* env pretty printer *)
let string_of_rv rv = 
  let (s,v) = rv in    
    sprintf "%s:%s" (Value.string_of_t v) (Syntax.string_of_sort s)
      
let string_of_env ev = 
  Pretty.curlybraces 
    (QidMap.fold (fun q r acc -> 
		    sprintf "\n\t%s=%s%s%s"		      
		      (Syntax.string_of_qid q)
		      (string_of_rv (!r))
		      (if (acc = "") then "" else ", ")
		      acc)
       ev "")


(* LIBRARY *)
(* Sets whose elements are Syntax.ids *)
module StringSet = Set.Make(
  struct
    type t = string
    let compare = compare
  end)
  
(* the library's state *)
let parse_qid s =     
  let lexbuf = Lexing.from_string s in
    Lexer.reset ();
    Parser.qid Lexer.main lexbuf 

let pre_ctx = List.map parse_qid ["Prelude"]

let library : env ref = ref (empty ())
let loaded = ref StringSet.empty

(* clear all loaded modules; used, e.g., in the visualizer *)
let old_library = ref None
let reset () = 
  if !old_library = None then old_library := Some !library;
  loaded := StringSet.empty;  
  library := 
    begin 
      match !old_library with 
	  Some lib -> lib
	| None -> assert false
    end
      
let get_library () = !library
  
let register q r = library := (QidMap.add q (ref r) (!library))
let register_env ev m = 
  debug (fun () -> sprintf "register_env for %s" (Syntax.string_of_qid m));
  QidMap.iter 
    (fun q r -> 
       debug (fun () -> sprintf "registring %s" (Syntax.string_of_qid (Syntax.dot m q)));
       register (Syntax.dot m q) !r) ev
    
let register_native qs ss v = 
  let sort_of_string s = 
    let lexbuf = Lexing.from_string s in
      Lexer.reset ();
      Parser.sort Lexer.main lexbuf 
  in	
  let q = parse_qid qs in
  let s = sort_of_string ss in
    register q (s,v)
  
(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match q with 
    | ([],_) -> None
    | (n::_,_) -> Some n
	
let paths = Prefs.createStringList 
  "include" 
  "search path for .fcl sources"
  "Focal modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources."
let _ = Prefs.alias paths "I"
  
let find_filename n = 
  let fn = (String.uncapitalize n) ^ ".fcl" in
  let rec loop ds = match ds with
    | []    -> None
    | d::drest -> 
	let full_fn = d ^ (if d.[String.length d - 1] = '/' then "" else "/") ^ fn in
	  if (Sys.file_exists full_fn) then Some full_fn
	  else loop drest
  in
  let includes = Prefs.read paths in
    loop (includes)
      
(* load modules dynamically *)
(* backpatch hack *)
let compile_file_impl = ref (fun _ _ -> ())  

let load ns = 
  let fno = find_filename ns in	
    if (StringSet.mem ns (!loaded)) then ()	  
    else 
      begin
	match fno with 
	  | None -> ()
	  | Some fn ->
	      prerr_string (sprintf "[ loading %s]\n" fn); flush stderr;
	      loaded := (StringSet.add ns (!loaded)); 
	      (!compile_file_impl) fn ns
      end
	
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> load (Syntax.string_of_id n)
      
(* lookup in a naming context *)
let lookup_library2 nctx q = 
  let _ = debug (fun () -> sprintf "lookup_library2: %s in [%s] from %s\n"
		   (Syntax.string_of_qid q)
		   (concat_list ", " (Safelist.map Syntax.string_of_qid nctx))
		   (string_of_env !library))
  in
  let rec lookup_library_aux os q2 =       
    let _ = debug (fun () -> sprintf "lookup_library_aux %s in [%s] from %s "
		     (Syntax.string_of_qid q2)
		     (concat_list ", " (Safelist.map Syntax.string_of_qid os))
		     (string_of_env !library)
		  )
    in
    let sq = Syntax.string_of_qid in
    let try_lib () = lookup !library (fun x -> x) q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
	  Some r -> Some r
	| None -> 
	    begin
	      match load_var q2; try_lib () with
		| Some r -> Some r
		| None -> match os with 
		    | []       -> None
		    | o::orest -> lookup_library_aux orest (Syntax.dot o q) 
	    end
    in
      lookup_library_aux nctx q
	
let lookup_oev oev ev_of_oev ctx_of_oev q = 
  let _ = debug (fun () -> sprintf "looking up %s in [%s] from %s\n"
		   (Syntax.string_of_qid q)
		   (concat_list ", " (Safelist.map Syntax.string_of_qid (ctx_of_oev oev)))
		   (string_of_env (ev_of_oev oev)))
  in    
  let ev = ev_of_oev oev in
    match lookup ev (fun x -> x) q with
      | Some r -> Some r
      | None -> lookup_library2 (ctx_of_oev oev) q
	  
let lookup_library q = lookup_library2 [] q

