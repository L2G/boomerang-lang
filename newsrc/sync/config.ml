(* to replace the cmdlineparser *)
open Prefs

let replica = Prefs.createString "replica" "" "*a replica to get" ""
let _ = Prefs.alias replica "r"

let replica1 = Prefs.createString "replica1" "" "*the first of the 2 replicas to synchronize" ""
let _ = Prefs.alias replica1 "r1"

let replica2 = Prefs.createString "replica2" "" "*the second of the 2 replicas to synchronize" ""
let _ = Prefs.alias replica2 "r2"

let abstract = Prefs.createString "abstract" "" "*an abstract view to put" ""
let _ = Prefs.alias abstract "a"

let concrete = Prefs.createString "concrete" "" "*a concrete view to put into" ""
let _ = Prefs.alias concrete "c"

let archive =  Prefs.createString "archive" "" "*the archive" ""
let _ = Prefs.alias archive "ar"

let newarchive = Prefs.createString "newarchive" "" "*the new archive after synchronization" ""
let _ = Prefs.alias newarchive "newar"

let newreplica1 = Prefs.createString "newreplica1" "" "*the updated first replica" ""
let _ = Prefs.alias newreplica1 "newr1"

let newreplica2 = Prefs.createString "newreplica2" "" "*the updated second replica" ""
let _ = Prefs.alias newreplica2 "newr2"

let output = Prefs.createString "output" "" "*the output file for get or put" ""
let _ = Prefs.alias output "o"

let lens = Prefs.createString "lens" "" "*the lens to use for a get or put" ""
let _ = Prefs.alias lens "l"

let lensar = Prefs.createString "lensar" "" "*the lens to use for the archive" ""
let _ = Prefs.alias lensar "la"

let lensr1 = Prefs.createString "lensr1" "" "*the lens for the first replica" ""
let _ = Prefs.alias lensr1 "l1"

let lensr2 = Prefs.createString "lensr2" "" "*the lens for the second replica" ""
let _ = Prefs.alias lensr2 "l2"

let path = Prefs.createStringList "path" "*the path to the lenses" ""
let _ = Prefs.alias path "p"

let schema = Prefs.createString "schema" "" "*the schema for the synchronization" ""
let _ = Prefs.alias schema "s"

let rest = Prefs.createStringList "rest" "*stuff" ""
