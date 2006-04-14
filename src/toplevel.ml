open Error 

let _ = 
  (* initialize required but not directly referenced modules *)
  Compiler.init();
  Viewers.init();
  Prelude.init()

let debug = Trace.debug "toplevel"

(* FIX: This probably needs to be integrated with the new printing stuff *)
let failwith s = prerr_endline s; exit 1

let lookup qid_str = Registry.lookup_library (Value.parse_qid qid_str)
      	  
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
  if fn="" then None else 
  let (fn, ekeyo) = Surveyor.parse_filename fn in
  let ekey = Surveyor.get_ekey ekeyo fn None in
  Surveyor.tree_of_file fn (Surveyor.get_reader ekey)
      
let write_tree fn v = 
  if fn="" then () else 
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

let signalconflicts = Prefs.createBool "signalconflicts" false "exit with status 1 (instead of 0) on conflicts" ""

let forcer1 = Prefs.createBool "forcer1" false "overwrite r2 and archive with r1" ""


(********)
(* SYNC *)
(********)
let sync o_fn a_fn b_fn s lenso lensa lensb o'_fn a'_fn b'_fn =
  let s = lookup_schema s in 
  let lenso = lookup_lens lenso in
  let lensa = lookup_lens lensa in 
  let lensb = lookup_lens lensb in         
  let forcer1 = Prefs.read forcer1 in
  let (o, a, b, (act, oa', aa', ba')) =
    if forcer1 then begin
      let a = read_tree a_fn in
      let aa = Misc.map_option (Lens.get lensa) a in
      (a, a, a, (Sync.equal, aa, aa, aa))
    end
    else 
      let a = read_tree a_fn in
      let o = read_tree o_fn in
      let b = read_tree b_fn in
      let aa = Misc.map_option (Lens.get lensa) a in
      let oa = Misc.map_option (Lens.get lenso) o in
      let ba = Misc.map_option (Lens.get lensb) b in
      (o, a, b, Sync.sync s oa aa ba)
    in
  Sync.format act;
  Format.print_newline();
  let o' = Misc.map_option (fun o' -> Lens.put lenso o' (if forcer1 then None else o)) oa' in
  let a' = Misc.map_option (fun a' -> Lens.put lensa a' a) aa' in
  let b' = Misc.map_option (fun b' -> Lens.put lensb b' (if forcer1 then None else b)) ba' in
  ignore (Misc.map_option (write_tree o'_fn) o');
  ignore (Misc.map_option (write_tree a'_fn) a');
  ignore (Misc.map_option (write_tree b'_fn) b');
  if not (Sync.conflict_free act) && Prefs.read signalconflicts then exit 1

(**********************************************************************************)
(* Infrastructure for custom top-level programs *)             

(* Common preferences *)

let rest = Prefs.createStringList "rest" "*stuff" ""

let r1pref = Prefs.createString "r1" "" "first replica to synchronize" ""

let r2pref = Prefs.createString "r2" "" "second replica to synchronize" ""

let arpref =  Prefs.createString "ar" "" "archive for synchronization" ""

let newarpref = Prefs.createString "newar" "" "new archive after synchronization" ""

let newr1pref = Prefs.createString "newr1" "" "new first replica" ""

let newr2pref = Prefs.createString "newr2" "" "new second replica" ""

let lensarpref = Prefs.createString "lensar" "" "explicitly specified lens for archive" ""

let lensr1pref = Prefs.createString "lensr1" "" "explicitly specified lens for first replica" ""

let lensr2pref = Prefs.createString "lensr2" "" "explicitly specified lens for second replica" ""

let schemapref = Prefs.createString "schema" "" "explicitly specified synchronization schema" ""
let _ = Prefs.alias schemapref "s"

let check_pref = Prefs.createStringList "check" "run unit tests for given module(s)" ""
                   
let dryrun = Prefs.createBool "dryrun" false "don't write any files" ""

(* Running external commands *)

let runcmd cmd = 
  debug (fun() -> Format.eprintf "%s\n" cmd);
  if Sys.command cmd <> 0 then failwith ("Command failed: "^cmd)

let cp_or_del f g =
  if Sys.file_exists f
  then runcmd (Printf.sprintf "cp %s %s" f g)
  else if Sys.file_exists g then runcmd (Printf.sprintf "rm %s" g)
  
(* Top-level boilerplate *)

type 'a filetype = Unknown | Meta | UserType of 'a

let toplevel' progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens () =
  let usageMsg = 
      "Usage:\n"
    ^ "    "^progName^" FILE [options]\n                                      dump"
    ^ " or "^progName^" FILE FILE [options]                                   transform\n"
    ^ " or "^progName^" -ar FILE -r1 FILE -r2 FILE [more options]             sync (in place)\n"
    ^ " or "^progName^" -ar FILE -r1 FILE -r2 FILE                            sync\n"
    ^ "                 -newar FILE -newr1 FILE -newr2 FILE [more options]\n"
    ^ "\n"
    ^ "Options:" in

  (* Deal with command line *)
  Prefs.parseCmdLine usageMsg;
  debug (fun() -> Prefs.dumpPrefsToStderr() );

  (* Run unit tests if requested *)
  if Prefs.read check_pref <> [] then
    begin
      Safelist.iter check (Prefs.read check_pref);
      if Prefs.read rest = [] then exit 0
    end;

  (* Handle command lines of the special forms 'harmonize-blah r1 r2' or 'harmonize-blah r1' *)
  begin match Prefs.read rest with
    | [r1] -> Prefs.set r1pref r1
    | [r2;r1] -> Prefs.set r1pref r1; Prefs.set r2pref r2
    | [] -> ()
    | _ -> Prefs.printUsage usageMsg; exit 1
  end;

  (* Make up an archive name if none was provided *)
  let fixup s = Misc.replace_substring s "/" "-" in
  if Prefs.read arpref = "" && Prefs.read r2pref <> "" then
    Prefs.set arpref
      (Printf.sprintf "harmonyar-%s-%s-%s.meta"
         (archNameUniquifier()) (fixup (Prefs.read r1pref)) (fixup (Prefs.read r2pref)));

  (* Overwrite original files if no new filenames are specified *)
  let overwrite pnew p = if Prefs.read pnew = "" then Prefs.set pnew (Prefs.read p) in
  overwrite newarpref arpref;
  overwrite newr1pref r1pref;
  overwrite newr2pref r2pref;

  (* Grab all the preferences *)
  let p pref = Prefs.read pref in
  let ar    = p arpref in 
  let r1    = p r1pref in
  let r2    = p r2pref in
  let newar = p newarpref in
  let newr1 = p newr1pref in
  let newr2 = p newr2pref in
            
  (* Figure out encodings, types, and pre/postprocessing requirements *)
  let encoding f =
    if f="" then ("","meta",Meta,None,None)
    else match Surveyor.parse_filename f with
      (f',Some e) -> (f',e,Unknown,None,None)
    | (_,None) -> 
        if Util.endswith f ".meta" then (f,"meta",Meta,None,None)
        else
          try
            let (fenc,ftypeuser,fpre,fpost) = chooseEncoding f in
            (f,fenc,UserType ftypeuser,fpre,fpost)
          with
            Not_found ->
              let e = Surveyor.get_ekey None f None in
              (f,e,Unknown,None,None)
        in
  let (arf,arenc,artype,arpre,arpost) = encoding ar in
  let (r1f,r1enc,r1type,r1pre,r1post) = encoding r1 in
  let (r2f,r2enc,r2type,r2pre,r2post) = encoding r2 in

  (* Choose abstract schema *)
  let undup = function
      [x;y] -> if x=y then [x] else [x;y]
    | [x;y;z] ->
        let lend = if y=z then [] else [z] in
          if x=y then x :: lend else x :: y :: lend
    | l -> l in
  let sort_types l = undup (List.sort compare l) in 
  let rec remove_meta = function
      [] -> []
    | Unknown::rest -> remove_meta rest
    | Meta::rest -> remove_meta rest
    | (UserType t)::rest -> t::(remove_meta rest) in
  let schema =
    match Prefs.read schemapref with
      "" -> chooseAbstractSchema (sort_types (remove_meta [artype;r1type;r2type]))
    | s -> s   in

  (* Choose what lenses to use *)
  let choose f s default t =
    if f = "" then "Prelude.id"
    else 
      let d = Prefs.read default in
        if d <> "" then d
        else match t with 
            Meta -> "Prelude.id"
          | Unknown -> failwith ("-lens"^s^" preference must be specified explicitly")
          | UserType ut -> chooseLens ut schema in
  let arlens = choose arf "ar" lensarpref artype in
  let r1lens = choose r1f "r1" lensr1pref r1type in
  let r2lens = choose r2f "r2" lensr2pref r2type in

  let enc f e = f ^ ":" ^ e in

  let tempnames = ref [] in
  let tempname f =
    let n = Misc.tempFileName f in
    tempnames := f::!tempnames;
    n  in
  (* let cleanupTempFiles () =
     List.iter (fun n -> Sys.remove n) !tempnames in *)

  Util.finalize (fun () ->
    (* Do pre-processing *)
    let preprocess f p =
      match p with
        None -> f
      | Some pfun ->
          let ftemp = tempname f in
          pfun f ftemp;
          ftemp   in
    let artemp = if Prefs.read forcer1 then arf else preprocess arf arpre in
    let r1temp = preprocess r1f r1pre in
    let r2temp = if Prefs.read forcer1 then r2f else preprocess r2f r2pre in

    (* Do it *)
    if r2="" then begin
      get r1lens (enc r1temp r1enc) (enc "-" "meta")
    end else begin
      (* Make up temporary output file names *)
      let newartemp = tempname newar in
      let newr1temp = tempname newr1 in
      let newr2temp = tempname newr2 in

      sync (enc artemp arenc) (enc r1temp r1enc) (enc r2temp r2enc)
           schema
           arlens r1lens r2lens
           (enc newartemp arenc) (enc newr1temp r1enc) (enc newr2temp r2enc);

      (* Postprocess *)
      let postprocess p fpost f =
        if not (Prefs.read dryrun) then
        match p with
          None -> cp_or_del fpost f
        | Some pfun -> pfun fpost f   in
      postprocess arpost newartemp newar;
      postprocess r1post newr1temp newr1;
      postprocess r2post newr2temp newr2
    end)
  (* Clean up *)
  (fun () -> 
    (* cleanupTempFiles() *)
      Format.print_flush ()
  )
  
let toplevel progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens =
  Unix.handle_unix_error 
    (fun () -> Error.exit_on_error
                 (toplevel' progName archNameUniquifier chooseEncoding chooseAbstractSchema chooseLens))
    ()
  
