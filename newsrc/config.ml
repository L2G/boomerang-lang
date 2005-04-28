(* general options *)
let paths = Prefs.createStringList 
  "include" 
  "search path for .fcl sources"
  "Focal modules are loaded, compiled, and registered on-demand. The search path specifies where the run-time system should search for module sources."
let _ = Prefs.alias paths "I"

(* get and put options *)
let lens = Prefs.createString 
  "lens" 
  "Pervasives.Native.id"
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
