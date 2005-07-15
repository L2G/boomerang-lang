open Error

let debug = Trace.debug "harmony"

let failwith s = prerr_endline s; exit 1

let lookup qid_str = 
  Registry.lookup_library (Value.parse_qid qid_str)
	  
let lookup_lens qid_str =
  match lookup qid_str with
      None -> failwith (Printf.sprintf "lens %s not found" qid_str)
    | Some rv -> 
	Value.get_lens 
	  (Info.M (Printf.sprintf "%s is not a lens" qid_str))
	  (Registry.value_of_rv rv)

let lookup_type qid_str =
  match lookup qid_str with
      None -> failwith (Printf.sprintf "%s not found" qid_str)
    | Some rv -> 
	Value.get_type 
	  (Info.M (Printf.sprintf "%s is not a type" qid_str))
	  (Registry.value_of_rv rv)

let read_tree fn = 
  let (fn, ekeyo) = Surveyor.parse_filename fn in
  let ekey = Surveyor.get_ekey ekeyo fn None in
  Surveyor.tree_of_file fn (Surveyor.get_reader ekey)
      
let write_tree fn v = 
  let (fn, ekey) = Surveyor.parse_filename fn in
  let ekey = Surveyor.get_ekey ekey fn None in
  (Surveyor.get_writer ekey) v fn  

(*********)
(* CHECK *)
(*********)
let check m = 
  if not (Registry.load m) then
    failwith (Printf.sprintf "Error: could not find module %s\n" m)
	      
(*******)
(* GET *)
(*******)
let get lens c_fn o_fn = 
  let cvo = read_tree c_fn in
  let lens = lookup_lens lens in
  let av = match cvo with
    | Some cv -> Lens.get lens cv
    | None -> failwith (Printf.sprintf "Concrete tree file %s is missing." c_fn) 
  in
    write_tree o_fn av

(*******)
(* PUT *)
(*******)
let put lens a_fn c_fn o_fn = 
  let avo = read_tree a_fn in
  let cvo = read_tree c_fn in 
  let lens = lookup_lens lens in
  let cv' = match avo with
      Some av -> Lens.put lens av cvo
    | None -> failwith (Printf.sprintf "The abstract tree file %s is missing or empty" a_fn)
  in	
    write_tree o_fn cv'
      
(********)
(* SYNC *)
(********)
let sync o_fn a_fn b_fn s lenso lensa lensb o'_fn a'_fn b'_fn =
  let o = read_tree o_fn in
  let a = read_tree a_fn in
  let b = read_tree b_fn in
  let s = lookup_type s in 
  let lenso = lookup_lens lenso in
  let lensa = lookup_lens lensa in 
  let lensb = lookup_lens lensb in         
  let oa = Misc.map_option (Lens.get lenso) o in
  let aa = Misc.map_option (Lens.get lensa) a in
  let ba = Misc.map_option (Lens.get lensb) b in
  let (act, oa', aa', ba') = Sync.sync s oa aa ba in
  let o' = Misc.map_option (fun o' -> Lens.put lenso o' o) oa' in
  let a' = Misc.map_option (fun a' -> Lens.put lensa a' a) aa' in
  let b' = Misc.map_option (fun b' -> Lens.put lensb b' b) ba' in
  Sync.format_without_equal act;
  Format.print_newline();
  ignore (Misc.map_option (write_tree o'_fn) o');
  ignore (Misc.map_option (write_tree a'_fn) a');
  ignore (Misc.map_option (write_tree b'_fn) b');
  exit (if Sync.conflict_free act then 0 else 1)

let rest = Prefs.createStringList "rest" "*stuff" ""

