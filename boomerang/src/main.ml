(* Trivial front end; all command-line arguments are explicit *)
Toplevel.toplevel
  "boomerang"
  (fun () -> "")
  (fun _ _ -> "Prelude.id")

