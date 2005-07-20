(* Trivial front end, where all command-line arguments must be specified explicitly *)

Toplevel.toplevel
  "harmony"
  (fun() -> "")
  (fun f -> Toplevel.failwith ("Encoding must be specified explicitly for " ^ f))
  (fun _ -> "Prelude.Any")
  (fun _ _ -> "Prelude.id")

