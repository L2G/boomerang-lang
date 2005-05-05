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
