(* choose_encoding : Surveyor.filename -> Surveyor.encoding list
                        -> Surveyor.encoding *)
let choose_encoding file = function
    [] -> prerr_endline ("Unable to find encoding for " ^ file ^ ".  Exiting...");
        exit 1
  | [e] -> e
  | es ->
      begin
        print_endline (file ^ " might be encoded as:\n");
        Safelist.iter Surveyor.print_description es;
        print_endline "\nToo many choices!  Exiting...";
        exit 1
      end

(* choose_vt : Optometrist.type_desc option
            -> Optometrist.type_desc list -> Optometrist.type_desc *)
let choose_vt sugg choices =
  match sugg,choices with
    Some vt, vts when Safelist.mem vt vts -> vt
  | Some vt, vts ->
      prerr_endline ("Suggested view type "
                    ^(Optometrist.type_desc_as_string vt)^ " is not reachable.");
      prerr_endline ("Maybe try one of these?");
      prerr_endline (Optometrist.type_desc_list_as_string vts);
      exit 1
  | None, [] ->
      prerr_endline "Unable to find common type, can't synchronize.";
      exit 1
  | None, [vt] -> vt
  | None, vts ->
      (* BCP: in order to make progress, let's just choose one! *)
      print_endline
        ("I can synchronize these as:\n  "
         ^ (String.concat "\n  " (Safelist.map Optometrist.type_desc_as_string vts)));
      let vt = Safelist.hd (Safelist.rev vts) in
      print_endline ("I'll use " ^ (Optometrist.type_desc_as_string vt) ^".");
      vt
(*
      print_endline
         ("I can synchronize these as:\n"
          ^ (String.concat "\n" (Safelist.map Optometrist.view_type_as_string vts))
          ^ "\n" ^ "Too many choices!  Exiting...");
       exit 1
*)

(* massage_worklist : Sync.action -> Sync.action *)
let massage_action a =
  print_endline
    (Misc.color "\nTransfer instructions..." Misc.White ~bold:true);
  Sync.format_without_equal a;
  Format.print_newline ();
  if Prefs.read Config.pause then
    begin
      print_endline ("Press c to continue, a to abort"); flush stdout;
      let rec f () =
        let ch = Misc.read_char () in
        match ch with
          'a' -> print_endline ("aborting.."); exit 1
        | 'c' -> print_endline ("continuing.."); a
        | _ -> f ()
      in
      f ()
    end
  else a
