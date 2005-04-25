open Config
  
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
	     failwith (Printf.sprintf "No unique encoding found for this file : %s." f))
	
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
  let outc = open_out file in
  output_string  outc written
    
let synchronize o a b = (o,a,b)
    
(* ========== *)	
(* GET / SHOW *)
(* ========== *)	
let get replica lens_string output =
  (* look if there is also an encoding specification *)
  let (r, reader_ek) = parse_replica replica in
  (* get the encoding key *)
  let ekey = get_ekey r reader_ek in
  (* apply the reader, get the concrete view *)
  let cv = get_view_from_file (Surveyor.get_reader ekey) r in
  (* now we'll get the lens *)
  let lens = match (Registry.lookup_lens (Registry.parse_qid lens_string)) with 
      Some l -> l
    | None -> failwith (Printf.sprintf "Lens %s not found" lens_string)
  in
  (* apply it to get the abstract view *)
  let av = 
    try match cv with
      Some cview -> Lens.get lens cview 
    | None -> failwith (Printf.sprintf "The replica file %s is missing or empty" replica) 
    with V.Error(e) -> V.format_msg e; failwith "Error in get function"
  in
    
  (* we're gonna have to output it *)
  let (o, viewer_ek) = parse_replica output in
  (* get back the viewer *)
  let okey = get_ekey o viewer_ek in
  (* apply it to get a string *)
  let written = (Surveyor.get_writer okey) av in
  (* and do the output *)
    view_to_file written o
    
(* ========== *)	
(*     PUT    *)
(* ========== *)	
let put abstract concrete lens_string output =
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
    match av with
      Some aview -> Lens.put lens aview cv
    | None -> failwith (Printf.sprintf "The abstract view file %s is missing or empty" a) in
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
(* let sync archive replica1 replica2 schema lensa lens1 lens2 newarchive newreplica1 newreplica2 = *)
(*   (\* reading the views from the given filenames *\) *)
(*   (\* the archive *\) *)
(*   let (ar, reader_ek) = parse_replica archive in *)
(*   let arkey = get_ekey ar reader_ek in *)
(*   let arcv = get_view_from_file (Surveyor.get_reader arkey) ar in *)
(*   (\* the 1st replica *\) *)
(*   let (r1, reader_ek) = parse_replica replica1 in *)
(*   let r1key = get_ekey r1 reader_ek in *)
(*   let r1cv = get_view_from_file (Surveyor.get_reader r1key) r1 in *)
(*   (\* the 2nd replica *\) *)
(*   let (r2, reader_ek) = parse_replica replica2 in *)
(*   let r2key = get_ekey r2 reader_ek in *)
(*   let r2cv = get_view_from_file (Surveyor.get_reader r2key) r2 in *)
(*   (\* the lenses *\) *)
(*   let lensa = Native.id in *)
(*   let lens1 = Native.id in *)
(*   let lens2 = Native.id in *)
(*   (\* apply the lenses in the get direction *\) *)
(*   let arav = *)
(*     match arcv with  *)
(*       Some v -> Some (Lens.get lensa v)  *)
(*     | None -> None in *)
(*   let r1av = *)
(*     match r1cv with *)
(*       Some v -> Some (Lens.get lens1 v)  *)
(*     | None -> None in *)
(*   let r2av = *)
(*     match r2cv with *)
(*       Some v -> Some (Lens.get lens2 v)  *)
(*     | None -> None in *)
(*   (\** TODO here we do some synchronization *\) *)
(*   let (newarav, newr1av, newr2av) =  *)
(*     synchronize arav r1av r2av in *)
(*   (\* we apply the lenses in the put direction *\) *)
(*   let newarcv = *)
(*     match newarav with  *)
(*       Some v -> Some (Lens.put lensa v arcv) | None -> None in *)
(*   let newr1cv = *)
(*     match newr1av with *)
(*       Some v -> Some (Lens.put lens1 v r1cv) | None -> None in *)
(*   let newr2cv = *)
(*     match newr2av with *)
(*       Some v -> Some (Lens.put lens2 v r2cv) | None -> None in *)
(*   (\* and we write the results *\) *)
(*   (\* first the archive *\) *)
(*   let (o, viewer_ek) = parse_replica newarchive in *)
(*   let okey = get_ekey o viewer_ek in *)
(*   let s = (Surveyor.get_writer okey) newarcv in *)
(*   view_to_file s o ; *)
(*   (\* now the first replica *\) *)
(*   let (o, viewer_ek) = parse_replica newreplica1 in *)
(*   let okey = get_ekey o viewer_ek in *)
(*   let s = (Surveyor.get_writer okey) newr1cv in *)
(*   view_to_file s o *)
(*   (\* and finally the second replica *\) *)
(*   let (o, viewer_ek) = parse_replica newreplica2 in *)
(*   let okey = get_ekey o viewer_ek in *)
(*   let s = (Surveyor.get_writer okey) newr2cv in *)
(*   view_to_file s o *)
  
    
let main () =
  let usage = " Usage \n" in
  (* retrieves the value of a -option, fails if argument is missing on the cmd ine *)
  let p pref = 
    match Prefs.read pref with 
      "" ->  
	let names = Prefs.name pref in 
	failwith (Printf.sprintf "missing argument %s" 
		    (Safelist.nth names (Safelist.length names - 1))
        )    
    | s -> s in
  (* we parse the command line *)
  Prefs.parseCmdLine usage;
  (* and check the anonymous arg to see what's next *)
  match Prefs.read Config.rest with
    ["get"] | ["show"]-> 
	get (p replica) (p lens) (p output)
  | ["put"] ->
	put (p abstract) (p concrete) (p lens) (p output)
  | ["sync"] ->
      assert false
(*	sync 
	  (p archive) (p replica1) (p replica2) 
	  (p schema)
	  (p lensar) (p lensr1) (p lensr2)
	  (p newarchive) (p newreplica1) (p newreplica2)
*)
  | [ss] -> failwith( Printf.sprintf "Unknown command : %s \n %s" ss usage)
  | [] -> failwith usage
  |  _ -> failwith(Printf.sprintf "Only one command at a time :\n %s" usage)
	

let _ = 
  Unix.handle_unix_error main ()
