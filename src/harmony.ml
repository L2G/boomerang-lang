(* Remove this: *)
open Toplevel

let lens = Prefs.createString 
  "lens" 
  ""
  "lens to use for get and put"
  "the fully-qualified name of the lens to use in get and put modes"
let _ = Prefs.alias lens "l"

let concrete = Prefs.createString 
  "concrete" 
  ""
  "concrete tree to use for get and put" 
  "name of the file to use as the concrete argument to get and put"
let _ = Prefs.alias concrete "c"

let abstract = Prefs.createString 
  "abstract" 
  ""
  "abstract tree to use for get and put" 
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

let usageMsg = 
    "usage: harmony get -lens LENS -concrete FILE -output FILE [options]\n"
  ^ "       harmony put -lens LENS -abstract FILE -concrete FILE -output FILE [options]\n"
  ^ "       harmony check -module MODULE [options]\n"
  ^ "       harmony sync -schema SCHEMA\n"
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
  Unix.handle_unix_error 
    (fun () -> Error.exit_on_error main)     
    ()
  
  
