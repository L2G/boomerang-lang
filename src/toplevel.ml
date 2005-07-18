open Error;;

Viewers.init();;

let debug = Trace.debug "toplevel"

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

let lookup_schema qid_str =
  match lookup qid_str with
      None -> failwith (Printf.sprintf "schema %s not found" qid_str)
    | Some rv -> 
	Value.get_schema 
	  (Info.M (Printf.sprintf "%s is not a schema" qid_str))
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
  let s = lookup_schema s in 
  let lenso = lookup_lens lenso in
  let lensa = lookup_lens lensa in 
  let lensb = lookup_lens lensb in         
  let oa = Misc.map_option (Lens.get lenso) o in
  let aa = Misc.map_option (Lens.get lensa) a in
  let ba = Misc.map_option (Lens.get lensb) b in
  (* fill this in...
  debug (fun() ->
           Format.printf "Synchronizing...\n";
           Format.printf "  o = ";
        );
  *)
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

(**********************************************************************************)
(* Infrastructure for custom top-level programs *)             

(* Common preferences *)

let r1pref = Prefs.createString "r1" "" "*first replica to synchronize" ""

let r2pref = Prefs.createString "r2" "" "*second replica to synchronize" ""

let arpref =  Prefs.createString "ar" "" "*the archive" ""

let newarpref = Prefs.createString "newar" "" "*the new archive after synchronization" ""

let newr1pref = Prefs.createString "newr1" "" "*the updated first replica" ""

let newr2pref = Prefs.createString "newr2" "" "*the updated second replica" ""

(* Running external commands *)

let runcmd cmd = 
  debug (fun() -> Format.eprintf "%s\n" cmd);
  if Sys.command cmd <> 0 then failwith ("Command failed: "^cmd)

let cp f g = runcmd (Printf.sprintf "cp %s %s" f g)
  
(* Top-level boilerplate *)

type 'a filetype = Meta | UserType of 'a

let toplevel' progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens () =
  let usageMsg = 
      "Usage:\n"
    ^ "    "^progName^" FILE FILE [options]\n"
    ^ " or "^progName^" -ar FILE -r1 FILE -r2 FILE [options]\n"
    ^ " or "^progName^" -ar FILE -r1 FILE -r2 FILE -newar FILE -newr1 FILE -newr2 FILE [options]\n"
    ^ "\n"
    ^ "Options:" in

  (* Deal with command line *)
  Prefs.parseCmdLine usageMsg;
  debug (fun() -> Prefs.dumpPrefsToStderr() );

  (* Handle command lines of the special form 'harmonize-blah r1 r2' *)
  begin match Prefs.read rest with
      [r1;r2] -> Prefs.set r1pref r1; Prefs.set r2pref r2
    | [] -> ()
    | _ -> Prefs.printUsage usageMsg; exit 1
  end;

  (* Make up an archive name if none was provided *)
  if Prefs.read arpref = "" then
    Prefs.set arpref
      (Printf.sprintf ".harmonyar-%s-%s-%s.meta"
         (archNameUniquifier()) (Prefs.read r1pref) (Prefs.read r2pref));

  (* Overwrite original files if no new filenames are specified *)
  let overwrite pnew p = if Prefs.read pnew = "" then Prefs.set pnew (Prefs.read p) in
  overwrite newarpref arpref;
  overwrite newr1pref r1pref;
  overwrite newr2pref r2pref;

  (* Grab all the preferences *)
  let p pref = 
    match Prefs.read pref with 
        "" ->  
          let names = Prefs.name pref in 
            failwith (Printf.sprintf "argument '%s' is required" 
                        (Safelist.nth names (Safelist.length names - 1))
                     )    
      | s -> s in
  let ar    = p arpref in
  let r1    = p r1pref in
  let r2    = p r2pref in
  let newar = p newarpref in
  let newr1 = p newr1pref in
  let newr2 = p newr2pref in
            
  (* Figure out encodings, types, and pre/postprocessing requirements *)
  let encoding f =
    if Util.endswith f ".meta" then ("meta",Meta,None,None)
    else begin
      let (fenc,ftypeuser,fpre,fpost) = chooseEncoding f in
      (fenc,UserType ftypeuser,fpre,fpost)
    end in
  let (arenc,artype,arpre,arpost) = encoding ar in
  let (r1enc,r1type,r1pre,r1post) = encoding r1 in
  let (r2enc,r2type,r2pre,r2post) = encoding r2 in

  (* Choose abstract schema *)
  let undup = function
      [x;y] -> if x=y then [x] else [x;y]
    | l -> l in
  let sort_types l = undup (List.sort compare l) in
  let rec remove_meta = function
      [] -> []
    | Meta::rest -> remove_meta rest
    | (UserType t)::rest -> t::(remove_meta rest) in
  let schema = chooseAbstractSchema (sort_types (remove_meta [artype;r1type;r2type])) in

  (* Choose what lenses to use *)
  let choose = function
      Meta -> "Prelude.id"
    | UserType t -> chooseLens t schema in
  let arlens = choose artype in
  let r1lens = choose r1type in
  let r2lens = choose r2type in

  let enc f e = f ^ ":" ^ e in

  let tempnames = ref [] in
  let tempname f =
    let n = Misc.tempFileName f in
    tempnames := f::!tempnames;
    n  in
  let cleanupTempFiles () =
    List.iter (fun n -> Sys.remove n) !tempnames in

  Util.finalize (fun () ->
    (* Do pre-processing *)
    let preprocess f p =
      match p with
        None -> f
      | Some pfun ->
          let ftemp = tempname f in
          pfun f ftemp;
          ftemp   in
    let artemp = preprocess ar arpre in
    let r1temp = preprocess r1 r1pre in
    let r2temp = preprocess r2 r2pre in

    (* Make up temporary output file names *)
    let newartemp = tempname newar in
    let newr1temp = tempname newr1 in
    let newr2temp = tempname newr2 in

    (* Sync *)
    sync (enc artemp arenc) (enc r1temp r1enc) (enc r2temp r2enc)
         schema
         arlens r1lens r2lens
         (enc newartemp arenc) (enc newr1temp r1enc) (enc newr2temp r2enc);

    (* Postprocess *)
    let postprocess p fpost f =
      match p with
        None -> cp fpost f
      | Some pfun -> pfun fpost f   in
    postprocess arpost newartemp newar;
    postprocess r1post newr1temp newr1;
    postprocess r2post newr2temp newr2
  )
  (* Clean up *)
  (fun () -> 
    (* cleanupTempFiles() *) ()
    
  )
  
let toplevel progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens =
  Unix.handle_unix_error 
    (fun () -> Error.exit_on_error
                 (toplevel' progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens))
    ()
  
