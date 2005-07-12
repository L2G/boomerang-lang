(* Remove this: *)
open Toplevel

let r1pref = Prefs.createString "r1" "" "*first replica to synchronize" ""

let r2pref = Prefs.createString "r2" "" "*second replica to synchronize" ""

let arpref =  Prefs.createString "ar" "" "*the archive" ""

let newarpref = Prefs.createString "newar" "" "*the new archive after synchronization" ""

let newr1pref = Prefs.createString "newr1" "" "*the updated first replica" ""

let newr2pref = Prefs.createString "newr2" "" "*the updated second replica" ""

let orderedpref = Prefs.createBool "ordered" true "*keep bookmark ordering" ""

let usageMsg = 
    "Usage:\n"
  ^ "    harmonize-bookmarks FILE FILE [options]\n"
  ^ " or harmonize-bookmarks -ar FILE -r1 FILE -r2 FILE [options]\n"
  ^ " or harmonize-bookmarks -ar FILE -r1 FILE -r2 FILE \n"
  ^ "                        -newar FILE -newr1 FILE -newr2 FILE [options]\n"
  ^ "\n"
  ^ "Options:"

let p pref = 
  match Prefs.read pref with 
      "" ->  
        let names = Prefs.name pref in 
          failwith (Printf.sprintf "argument '%s' is required" 
                      (Safelist.nth names (Safelist.length names - 1))
                   )    
    | s -> s

let badUsage() =
  Prefs.printUsage usageMsg;
  exit 1

let debug = Trace.debug "startup"

type bookmarkType = Mozilla | Safari | Meta

let bookmarktype2string = function
  | Mozilla -> "Mozilla"
  | Safari  -> "Safari"
  | Meta    -> "Meta"
        
let runcmd cmd = 
  debug (fun() -> Format.eprintf "%s\n" cmd);
  if Sys.command cmd <> 0 then failwith ("Command failed: "^cmd)

let moz2xml f fpre = runcmd (Printf.sprintf "moz2xml < %s > %s" f fpre)

let xml2moz fpost f = runcmd (Printf.sprintf "xml2moz < %s > %s" fpost f)

let plutil f fpre = runcmd (Printf.sprintf "plutil -convert xml1 %s -o %s" f fpre)
  
let cp f g = runcmd (Printf.sprintf "cp %s %s" f g)
  

let main () =
  Prefs.parseCmdLine usageMsg;

  debug (fun() -> Prefs.dumpPrefsToStderr() );

  (* Handle command lines of the special form 'harmonizebookmarks r1 r2' *)
  begin match Prefs.read rest with
      [r1;r2] -> Prefs.set r1pref r1; Prefs.set r2pref r2
    | [] -> ()
    | _ -> badUsage()
  end;

  let ordered = Prefs.read orderedpref in

  (* Make up an archive name if none was provided *)
  if Prefs.read arpref = "" then
    Prefs.set arpref
      (Printf.sprintf ".harmonyar-%b-%s-%s.meta"
         ordered (Prefs.read r1pref) (Prefs.read r2pref));

  (* Overwrite original files if no new filenames are specified *)
  let overwrite pnew p = if Prefs.read pnew = "" then Prefs.set pnew (Prefs.read p) in
  overwrite newarpref arpref;
  overwrite newr1pref r1pref;
  overwrite newr2pref r2pref;

  (* Grab all the preferences *)
  let ar    = p arpref in
  let r1    = p r1pref in
  let r2    = p r2pref in
  let newar = p newarpref in
  let newr1 = p newr1pref in
  let newr2 = p newr2pref in
            
  (* Figure out encodings, types, and pre/postprocessing requirements *)
  let choose_encoding f =
    if Filename.check_suffix f ".html" then ("xml",Mozilla,Some moz2xml,Some xml2moz)
    else if Filename.check_suffix f ".plist" then ("xml",Safari,Some plutil,None)
    else if Filename.check_suffix f ".meta" then ("meta",Meta,None,None)
    else failwith ("Unrecognized suffix: "^f) in
  let (arenc,artype,arpre,arpost) = choose_encoding ar in
  let (r1enc,r1type,r1pre,r1post) = choose_encoding r1 in
  let (r2enc,r2type,r2pre,r2post) = choose_encoding r2 in

  (* Choose abstract schema *)
  let undup = function
      [x;y] -> if x=y then [x] else [x;y]
    | l -> l in
  let sort_types l = undup (List.sort compare l) in
  let schema =
    match (sort_types [r1type;r2type]),ordered with
      [Safari],true -> "Abstract"
    | [Safari],false -> "BushAbstract"
    | [Mozilla],true -> "Abstract" 
    | [Mozilla;Safari],true -> "Abstract" 
    | _ -> failwith (Printf.sprintf "Unimplemented combination of file types: %s, %s, ordered=%b"
                       (bookmarktype2string r1type) (bookmarktype2string r2type) ordered) in

  let choose_lens t =
    match t,schema with
      Safari,"Abstract"     -> "Safari.l2"
    | Safari,"BushAbstract" -> "Safari.l2"
    | Mozilla,"Abstract"    -> "Mozilla.l"
    | Meta,_                -> "Prelude.id" 
    | _                     -> assert false in
  let arlens = choose_lens artype in
  let r1lens = choose_lens r1type in
  let r2lens = choose_lens r2type in

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
         ("Bookmarks."^schema)
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

let _ = Unix.handle_unix_error 
  (fun () -> Error.exit_on_error main)     
  ()
  
  
