open Error

let debug = Trace.debug "harmony"

let failwith s = prerr_endline s; exit 1

let lookup qid_str = 
  Registry.lookup_library (Value.parse_qid qid_str)
	  
let lookup_lens qid_str =
  match lookup qid_str with
      None -> failwith (Printf.sprintf "%s not found" qid_str)
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

let read_view fn = 
  let (fn, ekeyo) = Surveyor.parse_filename fn in
  let ekey = Surveyor.get_ekey ekeyo fn None in
    Surveyor.view_of_file fn (Surveyor.get_reader ekey)
      
let write_view fn v = 
  let (fn, ekey) = Surveyor.parse_filename fn in
  let ekey = Surveyor.get_ekey ekey fn None in
  let v_str = (Surveyor.get_writer ekey) v in    
    Misc.write fn v_str

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
  let cvo = read_view c_fn in
  let lens = lookup_lens lens in
  let av = match cvo with
    | Some cv -> Lens.get lens cv
    | None -> failwith (Printf.sprintf "Concrete view file %s is missing." c_fn) 
  in
    write_view o_fn av

(*******)
(* PUT *)
(*******)
let put lens a_fn c_fn o_fn = 
  let avo = read_view a_fn in
  let cvo = read_view c_fn in 
  let lens = lookup_lens lens in
  let cv' = match avo with
      Some av -> Lens.put lens av cvo
    | None -> failwith (Printf.sprintf "The abstract view file %s is missing or empty" a_fn)
  in	
    write_view o_fn cv'
      
(********)
(* SYNC *)
(********)
let sync o_fn a_fn b_fn s lenso lensa lensb o'_fn a'_fn b'_fn =
  debug (fun()->Printf.eprintf "Start...\n%!");
  let o = read_view o_fn in
  let a = read_view a_fn in
  let b = read_view b_fn in
  let s = lookup_type s in 
  debug (fun()->Printf.eprintf "Lenses...\n%!");
  let lenso = lookup_lens lenso in
  let lensa = lookup_lens lensa in 
  let lensb = lookup_lens lensb in         
  let oa = Misc.map_option (Lens.get lenso) o in
  let aa = Misc.map_option (Lens.get lenso) a in
  let ba = Misc.map_option (Lens.get lenso) b in
  debug (fun()->Printf.eprintf "Sync...\n%!");
  let (act, oa', aa', ba') = Sync.sync s oa aa ba in
  let o' = Misc.map_option (fun o' -> Lens.put lenso o' o) oa' in
  let a' = Misc.map_option (fun a' -> Lens.put lenso a' o) aa' in
  let b' = Misc.map_option (fun b' -> Lens.put lenso b' o) ba' in
  Format.eprintf "\n%!";
  Sync.format_without_equal act;
  Format.printf "\n%!";
  ignore (Misc.map_option (write_view o'_fn) o');
  ignore (Misc.map_option (write_view a'_fn) a');
  ignore (Misc.map_option (write_view b'_fn) b')

(****************************  Command-line switches *******************************)

let lens = Prefs.createString 
  "lens" 
  ""
  "lens to use for get and put"
  "the fully-qualified name of the lens to use in get and put modes"
let _ = Prefs.alias lens "l"

let concrete = Prefs.createString 
  "concrete" 
  ""
  "concrete view to use for get and put" 
  "name of the file to use as the concrete argument to get and put"
let _ = Prefs.alias concrete "c"

let abstract = Prefs.createString 
  "abstract" 
  ""
  "abstract view to use for get and put" 
  "name of the file to use as the abstract argument to get and put"
let _ = Prefs.alias abstract "a"

let output = Prefs.createString 
  "output" 
  ""
  "output file for get and put"
  "name of the file to use as the output file for get and put"
let _ = Prefs.alias output "o"

let replica1 = Prefs.createString "replica1" "" "*the first of the 2 replicas to synchronize" ""
let _ = Prefs.alias replica1 "r1"

let replica2 = Prefs.createString "replica2" "" "*the second of the 2 replicas to synchronize" ""
let _ = Prefs.alias replica2 "r2"

let archive =  Prefs.createString "archive" "" "*the archive" ""
let _ = Prefs.alias archive "ar"

let newarchive = Prefs.createString "newarchive" "" "*the new archive after synchronization" ""
let _ = Prefs.alias newarchive "newar"

let newreplica1 = Prefs.createString "newreplica1" "" "*the updated first replica" ""
let _ = Prefs.alias newreplica1 "newr1"

let newreplica2 = Prefs.createString "newreplica2" "" "*the updated second replica" ""
let _ = Prefs.alias newreplica2 "newr2"

let lensar = Prefs.createString "lensar" "" "*the lens to use for the archive" ""
let _ = Prefs.alias lensar "la"

let lensr1 = Prefs.createString "lensr1" "" "*the lens for the first replica" ""
let _ = Prefs.alias lensr1 "l1"

let lensr2 = Prefs.createString "lensr2" "" "*the lens for the second replica" ""
let _ = Prefs.alias lensr2 "l2"

let schema = Prefs.createString "schema" "" "*the schema for the synchronization" ""
let _ = Prefs.alias schema "s"

let module_qid = Prefs.createString "module" "" "*the module to check" ""
let _ = Prefs.alias module_qid "m"

let rest = Prefs.createStringList "rest" "*stuff" ""

(****************************  Command-line Processing *******************************)
  
let usageMsg = 
    "usage: harmony get -lens LENS -concrete FILE -output FILE [options]\n"
  ^ "       harmony put -lens LENS -abstract FILE -concrete FILE -output FILE [options]\n"
  ^ "       harmony check -module MODULE [options]\n"
  ^ "       harmony sync -schema SCHEMA"
  ^ "               [-archive FILE -lensar LENS] \n"
  ^ "               -replica1 FILE -lensr1 LENS \n"
  ^ "               -replica2 FILE -lensr2 LENS \n"      
  ^ "               -newarchive FILE \n"
  ^ "               -newreplica1 FILE \n"
  ^ "               -newreplica2 FILE \n"      
  ^ "\n"
  ^ "Options:"
    
let main () =
  (* retrieves the value of a -option, fails if argument is missing on
     the command line *)
  let p pref = 
    match Prefs.read pref with 
	"" ->  
	  let names = Prefs.name pref in 
	    failwith (Printf.sprintf "argument '%s' required for 'harmony %s'" 
			(Safelist.nth names (Safelist.length names - 1))
			(Safelist.nth (Prefs.read rest) 0)
		     )    
      | s -> s in
    Prefs.parseCmdLine usageMsg;
    match Prefs.read rest with
      | ["check"]          -> check (p module_qid)	  
      | ["get"] | ["show"] -> get (p lens) (p concrete) (p output)
      | ["put"]            -> put (p lens) (p abstract) (p concrete) (p output)
      | ["sync"]           -> 
	  sync 
	    (p archive) (p replica1) (p replica2) 
	    (p schema)
	    (p lensar) (p lensr1) (p lensr2)
	    (p newarchive) (p newreplica1) (p newreplica2)
      | [ss] -> failwith(Printf.sprintf "Unknown command : %s \n%s" ss (Prefs.printUsage usageMsg; ""))
      | []   -> failwith(Printf.sprintf "%s\n" (Prefs.printUsage usageMsg;""))
      |  _   -> failwith(Printf.sprintf "Only one command at a time :\n %s" (Prefs.printUsage usageMsg; ""))

let _ = 
  try 
    Unix.handle_unix_error (fun () -> Error.fail_on_error main) ()
  with
      V.Illformed(s,m) -> 
	failwith (Printf.sprintf "V.Illformed: %s %s"
		    s
		    (Misc.concat_list ", " (Safelist.map V.string_of_t m)))
	  
