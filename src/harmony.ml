(* Trivial front end, where all command-line arguments must be specified explicitly *)

(*
 (* Nate's testing loop *)
 Presburger.init();; 
*)

Toplevel.toplevel
  "harmony"
  (fun() -> "")
  (fun f -> raise Not_found)
  (fun _ -> "Prelude.Any")
  (fun _ _ -> "Prelude.id")

