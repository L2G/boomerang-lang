(****************************  Top-level functions *******************************)
let debug s = prerr_string (s ^ "\n"); flush stderr 
 
let failwith s = prerr_endline s; exit 1
  
let get_ekey f = function
    Some ekey ->
      (try let _ = Surveyor.get_encoding ekey in ekey with
	   Not_found -> prerr_endline ("unknown encoding key " ^ ekey); exit 1)
  | None -> 
      (* We shall be able to pass some contents instead of None *)
      (match (Surveyor.find_encodings f None) with
	 | [ek] -> ek
	 | _    -> 
	     failwith (Printf.sprintf "No unique encoding found for file '%s'" f))
	
let parse_replica r =
  try 
    let n = String.rindex r ':' in
    (String.sub r 0 n, Some(String.sub r (n+1) ((String.length r) - (n+1))))
  with
    Not_found -> (r,None)

let get_view_from_file reader file = 
  if Sys.file_exists file then
    Some (Util.convertUnixErrorsToFatal "Harmony" ( fun () -> reader (Misc.read file)))
  else
    None
      
let view_to_file written file =
  let _ = Printf.printf "\nWriting [%s] to file %s" written file in
  let outc = open_out file in
  (output_string outc written; close_out outc)
   
    
let synchronize o a b = (o,a,b)
    
(* ========== *)	
(* GET / SHOW *)
(* ========== *)	
let get lens_qid concrete_file output_file =
  (* look if there is also an encoding specification *)
  let (r, reader_ek) = parse_replica concrete_file in
  (* get the encoding key *)
  let ekey = get_ekey r reader_ek in
  (* apply the reader, get the concrete view *)
  let cv = get_view_from_file (Surveyor.get_reader ekey) r in
  (* now we'll get the lens *)
  let lens = match (Registry.lookup_lens (Registry.parse_qid lens_qid)) with 
      Some l -> l
    | None -> failwith (Printf.sprintf "lens %s not found" lens_qid)
  in
  (* apply it to get the abstract view *)
  let av = 
    try match cv with
      |	Some cview -> Lens.get lens cview 
      | None -> failwith (Printf.sprintf "Concrete view file %s is missing." concrete_file) 
    with V.Error(e) -> (V.format_msg e; failwith "Error in get function")
      | V.Illformed(m,vs) -> 
	  V.format_msg ([`String m]@(Safelist.map (fun vi -> `View vi) vs)) ; failwith "Error in get function"
  in
  (* we're gonna have to output it *)
  let (o, viewer_ek) = parse_replica output_file in
  (* get back the viewer *)
  let okey = get_ekey o viewer_ek in
  (* apply it to get a string *)
  let written = (Surveyor.get_writer okey) av in
  (* and do the output *)
    view_to_file written o
    
(* ========== *)	
(*     PUT    *)
(* ========== *)	
let put lens_string abstract concrete output =
  (* look if there is also an encoding specification *)
  let (a, reader_ek) = parse_replica abstract in
    (* get the encoding key of the abstract*)
  let akey = get_ekey a reader_ek in
  (* apply the reader, get the abstract view *)
  let av = get_view_from_file (Surveyor.get_reader akey) a in
  (* do the same job for the concrete view *)
  let (c, reader_ek) = parse_replica concrete in
  let ckey = get_ekey c reader_ek in
  let cv = get_view_from_file (Surveyor.get_reader ckey) c in
  (* now we'll get the lens *)
  let lens = match (Registry.lookup_lens (Registry.parse_qid lens_string)) with 
      Some l -> l
    | None -> failwith (Printf.sprintf "Lens %s not found" lens_string)
  in
  (* apply it to put the abstract view into the concrete view*)
  let newcv = 
    try match av with
      Some aview -> Lens.put lens aview cv
    | None -> failwith (Printf.sprintf "The abstract view file %s is missing or empty" a)
    with V.Error(e) -> (V.format_msg e; failwith "Error in put function")
    | V.Illformed(m,vs) -> 
	V.format_msg ([`String m]@(Safelist.map (fun vi -> `View vi) vs)) ; failwith "Error in put function"
  in

  (* we're gonna have to output it *)
  let (o, viewer_ek) = parse_replica output in
  (* get back the viewer *)
  let okey = get_ekey o viewer_ek in
  (* apply it to get a string *)
  let written = (Surveyor.get_writer okey) newcv in
  (* and do the output *)
  view_to_file written o

(* ========== *)	
(*    SYNC    *)
(* ========== *)
let sync archive replica1 replica2 schema_string lensa_string lens1_string lens2_string newarchive newreplica1 newreplica2 =
  (* reading the views from the given filenames *)
  (* the archive *)
  let (ar, reader_ek) = parse_replica archive in
  let arkey = get_ekey ar reader_ek in
  let arcv = get_view_from_file (Surveyor.get_reader arkey) ar in
  (* the 1st replica *)
  let (r1, reader_ek) = parse_replica replica1 in
  let r1key = get_ekey r1 reader_ek in
  let r1cv = get_view_from_file (Surveyor.get_reader r1key) r1 in
  (* the 2nd replica *)
  let (r2, reader_ek) = parse_replica replica2 in
  let r2key = get_ekey r2 reader_ek in
  let r2cv = get_view_from_file (Surveyor.get_reader r2key) r2 in
  (* the lenses *)
  let lensa = match (Registry.lookup_lens (Registry.parse_qid lensa_string)) with 
    Some l -> l
  | None -> failwith (Printf.sprintf "Lens %s not found" lensa_string)
  in
  let lens1 = match (Registry.lookup_lens (Registry.parse_qid lens1_string)) with 
    Some l -> l
  | None -> failwith (Printf.sprintf "Lens %s not found" lens1_string)
  in
  let lens2 = match (Registry.lookup_lens (Registry.parse_qid lens2_string)) with 
    Some l -> l
  | None -> failwith (Printf.sprintf "Lens %s not found" lens2_string)
  in
  (* the schema *)
  let schema = 
    match (Registry.lookup () (Registry.get_library) (Registry.parse_qid schema_string)) with
      Some rv ->
	begin
	  match (Registry.sort_of_rv rv, Registry.value_of_rv rv) with
	    Syntax.SType _, Value.T(t) -> t 
	  | _ -> failwith (Printf.sprintf "%s is not a schema" schema_string)
	end
    | None -> failwith (Printf.sprintf "Schema %s not found" schema_string)
  in
  (* apply the lenses in the get direction *)
  let arav =
    match arcv with
      Some v -> Some (Lens.get lensa v)
    | None -> None in
  let r1av =
    match r1cv with
      Some v -> Some (Lens.get lens1 v)
    | None -> None in
  let r2av =
    match r2cv with
      Some v -> Some (Lens.get lens2 v)
    | None -> None in
  (* the actual synchronization *)
  let (action, newarav, newr1av, newr2av) =
    Sync.sync schema arav r1av r2av in
  (** for DEBUG, pretty-print of the action *)
  let _ = Sync.format action in
  (* we apply the lenses in the put direction *)
  let newarcv =
    match newarav with
      Some v -> Some (Lens.put lensa v arcv) | None -> None in
  let newr1cv =
    match newr1av with
      Some v -> Some (Lens.put lens1 v r1cv) | None -> None in
  let newr2cv =
    match newr2av with
      Some v -> Some (Lens.put lens2 v r2cv) | None -> None in
  (* and we write the results *)
  (* first the archive *)
  let (o, viewer_ek) = parse_replica newarchive in
  let okey = get_ekey o viewer_ek in
  (match newarcv with
    Some a ->
      let s = (Surveyor.get_writer okey) a in
      view_to_file s o
  | None -> assert false);
  (* now the first replica *)
  let (o, viewer_ek) = parse_replica newreplica1 in
  let okey = get_ekey o viewer_ek in
  (match newr1cv with
    Some r1 ->
      let s = (Surveyor.get_writer okey) r1 in
      view_to_file s o
  | None -> assert false);
  (* and finally the second replica *)
  let (o, viewer_ek) = parse_replica newreplica2 in
  let okey = get_ekey o viewer_ek in
  (match newr2cv with
    Some r2 ->
      let s = (Surveyor.get_writer okey) r2 in
      view_to_file s o
  | None -> assert false)


(****************************  Command-line switches *******************************)

(* get and put options *)
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

(* sync stuff *)
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

let rest = Prefs.createStringList "rest" "*stuff" ""

(****************************  Command-line Processing *******************************)
  
let usageMsg = 
  "usage: harmony get -lens l -concrete cf -output of [options]\n"
  ^ "       harmony put -lens l -abstract af -concrete cf -output of [options]\n\n"
  ^ "Options:"
    
let main () =
  (* retrieves the value of a -option, fails if argument is missing on the cmd ine *)
  let p pref = 
    match Prefs.read pref with 
      "" ->  
	let names = Prefs.name pref in 
	failwith (Printf.sprintf "argument '%s' required for 'harmony %s'" 
		    (Safelist.nth names (Safelist.length names - 1))
                    (Safelist.nth (Prefs.read rest) 0)
        )    
    | s -> s in
  (* we parse the command line *)
  Prefs.parseCmdLine usageMsg;
  (* and check the anonymous args to see what's next *)
  match Prefs.read rest with
    ["get"] | ["show"]-> 
	get (p lens) (p concrete) (p output)
  | ["put"] ->
	put (p lens) (p abstract) (p concrete) (p output)
  | ["sync"] ->
	sync 
	  (p archive) (p replica1) (p replica2) 
	  (p schema)
	  (p lensar) (p lensr1) (p lensr2)
	  (p newarchive) (p newreplica1) (p newreplica2)
  | [ss] -> failwith(Printf.sprintf "Unknown command : %s \n%s" ss (Prefs.printUsage usageMsg; ""))
  | []   -> failwith(Printf.sprintf "%s\n" (Prefs.printUsage usageMsg;""))
  |  _   -> failwith(Printf.sprintf "Only one command at a time :\n %s" (Prefs.printUsage usageMsg; ""))
	

let _ = 
  Unix.handle_unix_error main ()
