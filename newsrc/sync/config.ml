(* to replace the cmdlineparser *)
let replica = Prefs.createString "replica" "dummyreplica" "a replica to get" ""
let _ = Prefs.alias "replica" "r"

let replica1 = Prefs.createString "replica1" "" "the first of the 2 replicas to synchronize" ""
let _ = Prefs.alias "replica1" "r1"

let replica2 = Prefs.createString "replica2" "" "the second of the 2 replicas to synchronize" ""
let _ = Prefs.alias "replica2" "r2"

let abstract = Prefs.createString "abstract" "" "an abstract view to put" ""
let _ = Prefs.alias "abstract" "a"

let concrete = Prefs.createString "concrete" "" "a concrete view to put into" ""
let _ = Prefs.alias "concrete" "c"

let archive =  Prefs.createString "archive" "" "the archive" ""
let _ = Prefs.alias "archive" "ar"

let newarchive = Prefs.createString "newarchive" "" "the new archive after synchronization" ""
let _ = Prefs.alias "newarchive" "newar"

let newreplica1 = Prefs.createString "newreplica1" "" "the updated first replica" ""
let _ = Prefs.alias "newreplica1" "newr1"

let newreplica2 = Prefs.createString "newreplica2" "" "the updated second replica" ""
let _ = Prefs.alias "newreplica2" "newr2"

let output = Prefs.createString "output" "" "the output file for get or put" ""
let _ = Prefs.alias "output" "o"

let lens = Prefs.createString "lens" "" "the lens to use for a get or put" ""
let _ = Prefs.alias "lens" "l"

let lensar = Prefs.createString "lensar" "" "the lens to use for the archive" ""
let _ = Prefs.alias "lensar" "la"

let lensr1 = Prefs.createString "lensr1" "" "the lens for the first replica" ""
let _ = Prefs.alias "lensr1" "l1"

let lensr2 = Prefs.createString "lensr2" "" "the lens for the second replica" ""
let _ = Prefs.alias "lensr2" "l2"

let viewer = Prefs.createString "viewer" "" "the viewer to use for the get or put output" ""
let _ = Prefs.alias "viewer" "v"

let viewerar = Prefs.createString "viewerar" "" "the viewer for the archive output" ""
let _ = Prefs.alias "viewerar" "va"

let viewerr1 = Prefs.createString "viewerr1" "" "the viewer for the first updated replica" ""
let _ = Prefs.alias "viewerr1" "v1"

let viewerr2 = Prefs.createString "viewerr2" "" "the viewer for the second updated replica" ""
let _ = Prefs.alias "viewerr2" "v2"

let path = Prefs.createStringList "path" "the path to the lenses" ""
let _ = Prefs.alias "path" "p"

let schema = Prefs.createString "schema" "" "the schema for the synchronization" ""
let _ = Prefs.alias "schema" "s"

(* get preferences switches *)
let verbose = Prefs.createBool "verbose" false "display more information" ""
let progress = Prefs.createBool "progress" false "display progress for some lenses" ""
(* XXX: paranoid should not remain default true! *)
let paranoid = Prefs.createBool "paranoid" true "extra sanity checks" ""
let backup = Prefs.createBool "backup" true "backup original files" ""
let replace = Prefs.createBool "replace" false "overwrite original files" ""
let pause = Prefs.createBool "pause" false
                               "pause for confirmation before transferring"
                               ""
let terse = Trace.terse
let ext = Prefs.createString "e" "new" "extension for new files" ""
let alignfile = Prefs.create "align" None
  "file to use as persistent store for alignment" ""
  (fun old s -> Some s) (function None -> ["n/a"] | Some s -> [s])


let home_dir = (Unix.getpwuid (Unix.geteuid ())).Unix.pw_dir
let harmony_dir = home_dir ^ "/.harmony"
let temp_dir = harmony_dir ^ "/temp"
let just_do f = Unix.handle_unix_error (fun () -> try f () with e -> ()) ()
let startup () =
  just_do (fun () -> let _ = Unix.system ("/bin/rm -rf " ^ temp_dir) in ());
  just_do (fun () -> Unix.mkdir harmony_dir 0o0755);
  just_do (fun () -> Unix.mkdir temp_dir 0o0755)
