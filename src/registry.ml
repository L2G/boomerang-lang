(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.ml - library of Focal values                            *)
(********************************************************************)
(* $Id $ *)

let debug = Trace.debug "registry"

(* --------------- Registry values -------------- *)

type rv = Syntax.sort * Value.t

(* utility functions *)
let make_rv s v = (s,v)
let value_of_rv (s,v) = v
let sort_of_rv (s,v) = s
let string_of_rv rv = 
  let (s,v) = rv in    
    Printf.sprintf "%s:%s" (Value.string_of_t v) (Syntax.string_of_sort s)

(* utility functions for parsing *)      
(* FIXME: these could live somewhere else, but dependencies make it
   difficult *)
let parse_qid s =     
  let lexbuf = Lexing.from_string s in
    Lexer.setup "qid constant";
    let q = Parser.qid Lexer.main lexbuf in
      Lexer.finish ();
      q

let parse_sort s =
  let lexbuf = Lexing.from_string s in
    Lexer.setup "sort constant";
    let (i,st) = Parser.sort Lexer.main lexbuf in
      Lexer.finish ();
      st
	
(* --------------- Focal library -------------- *)

(* constants *)
let pre_ctx = List.map parse_qid ["Prelude"]

(* state *)
let loaded = ref []
let library : (rv Env.t) ref = ref (Env.empty ())

(* utilities *)
let get_library () = !library
  
(* hack to reset library to native stuff in visualizer *)
let old_library = ref None
let reset () = 
  if !old_library = None then old_library := Some !library;
  loaded := [];
  library := 
    match !old_library with 
	Some l -> l
      | None -> assert false

(* --------------- Registration functions -------------- *)
	  
(* register a value *)
(* FIXME: it would be save to use overwrite here *)
let register q r = library := (Env.update (!library) q r)
  
(* register a native function *)
let register_native qs ss v = 
  let q = parse_qid qs in
  let s = parse_sort ss in
    register q (s,v)
	  
(* register a whole (rv Env.t) in m *)
let register_env ev m = Env.iter (fun q r -> register (Syntax.dot m q) r) ev

(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match Syntax.get_qualifiers q with 
    | [] -> None
    | n::_ -> Some n
	
(* --------------- Lookup functions -------------- *)

let paths = Prefs.createStringList 
  "include" 
  "search path for .fcl sources"
  "Focal modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources."
let _ = Prefs.alias paths "I"
  
let find_filename fn = 
  let rec loop ds = match ds with
    | []    -> None
    | d::drest -> 	
	let full_fn = d ^ (if d.[String.length d - 1] = '/' then "" else "/") ^ fn in
	  if (Sys.file_exists full_fn) then Some full_fn
	  else loop drest
  in
    loop (Prefs.read paths)
      
(* load modules dynamically *)
(* backpatch hack *)
let compile_file_impl = ref (fun _ _ -> ())  

let load ns = 
  let fno = find_filename ((String.uncapitalize ns) ^ ".fcl") in	
    if (Safelist.mem ns (!loaded)) then true
    else 
      begin
	match fno with 
	  | None -> false
	  | Some fn ->
	      prerr_string (Printf.sprintf "[ loading %s]\n%!" fn);
	      loaded := ns::(!loaded); 
	      (!compile_file_impl) fn ns;
	      prerr_string (Printf.sprintf "[ loaded %s]\n%!" fn);

	      true
      end
	
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Syntax.string_of_id n))
      
(* lookup in a naming context *)
let lookup_library_ctx nctx q = 
  let rec lookup_library_aux nctx q2 =       
    let _ = debug (fun () -> Printf.eprintf "lookup_library_aux %s in [%s] from %s "
		     (Syntax.string_of_qid q2)
		     (Misc.concat_list ", " (Safelist.map Syntax.string_of_qid nctx))
		     (Env.to_string !library string_of_rv)
		  )
    in
    let sq = Syntax.string_of_qid in
    let try_lib () = Env.lookup !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
	  Some r -> Some r
	| None -> 
	    begin
	      match 
		(load_var q2; 
		 try_lib ())
	      with
		| Some r -> Some r
		| None -> match nctx with 
		    | []       -> None
		    | o::orest -> lookup_library_aux orest (Syntax.dot o q) 
	    end
  in
    lookup_library_aux nctx q

let lookup_library q = lookup_library_ctx [] q
