(* Remove this: *)
open Toplevel

let r1pref = Prefs.createString "r1" "" "*the first replica to synchronize" ""

let r2pref = Prefs.createString "r2" "" "*the second of the 2 replicas to synchronize" ""

let arpref =  Prefs.createString "ar" "" "*the archive" ""

let newarpref = Prefs.createString "newar" "" "*the new archive after synchronization" ""

let newr1pref = Prefs.createString "new1" "" "*the updated first replica" ""

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

type bookmarkType = Mozilla | Safari | Meta

let debug = Trace.debug "startup"

let bookmarktype2string = function
  | Mozilla -> "Mozilla"
  | Safari  -> "Safari"
  | Meta    -> "Meta"
        
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
            
  (* Figure out encodings *)
  let choose_encoding f =
    if Filename.check_suffix f ".html" then ("xml",Mozilla)
    else if Filename.check_suffix f ".plist" then ("xml",Safari)
    else if Filename.check_suffix f ".meta" then ("meta",Meta)
    else failwith ("Unrecognized suffix: "^f) in
  let (arenc,artype) = choose_encoding ar in
  let (r1enc,r1type) = choose_encoding r1 in
  let (r2enc,r2type) = choose_encoding r2 in

  let schema =
    match r1type,r2type,ordered with
      Safari,Safari,true -> "Abstract"
    | Safari,Safari,false -> "BushAbstract"
    | Mozilla,Mozilla,true -> "Abstract"
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

  sync (enc ar arenc) (enc r1 r1enc) (enc r2 r2enc)
       ("Bookmarks."^schema)
       arlens r1lens r2lens
       (enc newar arenc) (enc newr1 r1enc) (enc newr2 r2enc)

let _ = Unix.handle_unix_error 
  (fun () -> Error.exit_on_error main)     
  ()
  
  
