(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.ml - library of Focal values                            *)
(********************************************************************)
(* $Id $ *)

let debug = Trace.debug "registry"
let sprintf = Printf.sprintf

let verbose_flag =
  Prefs.createBool "verbose" false
    "display more information"
    "display more information"
let _ = Prefs.alias verbose_flag "v"

let verbose thk = 
  if (Prefs.read verbose_flag) then 
    thk ()
  else ()

(* --------------- Registry values -------------- *)

type rv = Syntax.sort * Value.t

(* utility functions *)
let make_rv s v = (s,v)
let value_of_rv (s,v) = v
let sort_of_rv (s,v) = s
let format_rv rv = 
  let (s,v) = rv in    
    Format.printf "@[";
    Value.format_t v;
    Format.printf ":";
    Syntax.format_sort s;
    Format.printf "]"
	
(* --------------- Focal library -------------- *)

(* constants *)
let pre_ctx = List.map Value.parse_qid ["Prelude"]

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
let register q r = library := (Env.update (!library) q r)
  
(* register a native value *)
let register_native qs ss v = 
  let q = Value.parse_qid qs in
  let s = Value.parse_sort ss in
    register q (s,v)
	  
(* register a whole (rv Env.t) in m *)
let register_env ev m = Env.iter (fun q r -> register (Syntax.dot m q) r) ev

(* --------------- Lookup functions -------------- *)

let paths = Prefs.createStringList 
  "include" 
  "search path for .fcl sources"
  "Focal modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources."
let _ = Prefs.alias paths "I"

let focalpath =
  try Util.splitIntoWords (Unix.getenv "FOCALPATH") ':'
  with Not_found -> []

(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match Syntax.get_qualifiers q with 
    | [] -> None
    | n::_ -> Some n

let find_filename basename exts = 
  let rec loop dirs = match dirs with
    | []    -> None
    | dir::drest ->
        begin 
          let try_fn ext k = 
            let fn = sprintf "%s%s%s%s%s"
              dir
              (if dir.[String.length dir - 1] = '/' then "" else "/")
              basename 
              (if String.length ext = 0 then "" else ".")
              ext in 
              if Sys.file_exists fn then Some fn
              else k ()
          in
          let rec inner_loop = function
              [] -> try_fn "" (fun () -> loop drest)                
            | ext::erest -> try_fn ext (fun () -> inner_loop erest) in
            inner_loop exts              
        end
  in
    loop ((Prefs.read paths) @ focalpath)
      
(* load modules dynamically *)
(* backpatch hack *)
let compile_file_impl = ref (fun _ _ -> Format.eprintf "@[Focal compiler is not linked! Exiting...@]"; exit 1)  
let compile_fcl_str_impl = ref (fun _ _ -> Format.eprintf "@[Focal compiler is not linked! Exiting...@]"; exit 1)  
let compile_src_str_impl = ref (fun _ _ -> Format.eprintf "@[Focal compiler is not linked! Exiting...@]"; exit 1)  

let load ns = 
  (* helper, when we know which compiler function to use *)
  let go comp source = 
    verbose (fun () -> Format.eprintf "@[loading %s ...@]@\n%!" source);
    loaded := ns::(!loaded);
    comp ();
    verbose (fun () -> Format.eprintf "@[loaded %s@]@\n%!" source) in      
  let uncapped = String.uncapitalize ns in
    if (Safelist.mem ns (!loaded)) then true
    else begin
      match find_filename uncapped ["src";"fcl"] with 
	| None -> 
            begin
              try 
                (* check for baked in source *)
                let (is_src,str) = Hashtbl.find Bakery.items uncapped in 
                  if is_src then go (fun () -> (!compile_src_str_impl) str ns) (sprintf "<baked source for %s>" ns)
                  else go (fun () -> (!compile_fcl_str_impl) str ns) (sprintf "<baked source for %s>" ns);
                  true
              with Not_found -> false
            end
	| Some fn ->
	    go (fun () -> (!compile_file_impl) fn ns) fn; 
            true
    end
	
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Syntax.string_of_id n))
      
(* lookup in a naming context *)
let lookup_library_ctx nctx q = 
  let rec lookup_library_aux nctx q2 =       

    (* Printf.eprintf "lookup_library_aux %s in [%s] from %s "
      (Syntax.string_of_qid q2)
      (Misc.concat_list ", " (Safelist.map Syntax.string_of_qid nctx))
      (Env.to_string !library string_of_rv);
    *)
    (* let _ = debug (fun () -> Printf.eprintf "lookup_library_aux %s in [%s] from %s "
       (Syntax.string_of_qid q2)
       (Misc.concat_list ", " (Safelist.map Syntax.string_of_qid nctx))
       (Env.to_string !library string_of_rv)
       )
       in *)
    let sq = Syntax.string_of_qid in
    let try_lib () = Env.lookup !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
	  Some r -> Some r
	| None -> 
	    begin
	      match load_var q2; try_lib () with
		| Some r -> Some r
		| None -> match nctx with 
		    | []       -> None
		    | o::orest -> lookup_library_aux orest (Syntax.dot o q) 
	    end
  in
    lookup_library_aux nctx q

let lookup_library q = lookup_library_ctx [] q
