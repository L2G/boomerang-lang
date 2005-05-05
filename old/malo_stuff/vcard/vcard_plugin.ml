(* vcard viewer and lenses *)
(* Spec: almost vcard v2.1 *)



let reader f =
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  Some (Vcardparser.vcard Vcardlex.token lexbuf )


let item_to_string v =
  match V.to_list v with
      [item,vc] -> item^(
	match V.to_list vc with
	    ["DATA",ld;"PARAM",lp] ->
	      (
		match V.to_list lp with
		    [] -> ""
		  | lpp -> ";TYPE="^(String.concat "," (Safelist.map fst lpp))
	      )^":"^
	      (String.concat ";" (Safelist.map V.get_value (V.list_from_structure ld)))
	  | ["PARAM",lp;"DATA",ld] ->
	      (
		match V.to_list lp with
		    [] -> ""
		  | lpp -> ";TYPE="^(String.concat "," (Safelist.map fst lpp))
	      )^":"^
	      (String.concat ";" (Safelist.map V.get_value (V.list_from_structure ld)))
	  | _ -> "#error#"
      )
    | _ -> "#error#"


let contact_to_string v =
  match V.get v "VCARD" with
      None -> "#error#"
    | Some l ->"BEGIN:VCARD\n"^(String.concat "\n" (Safelist.map item_to_string (V.list_from_structure l)))^"\nEND:VCARD"

let addbook_to_string v =
  String.concat "\n" (Safelist.map contact_to_string (V.list_from_structure v))

let writer vo f =
  match vo with
    None -> Sys.remove f
  | Some v -> Misc.write f (addbook_to_string v)



let _ =
  let etest filename copt = (Misc.filename_extension filename = "vcd")
			    || (Misc.filename_extension filename = "vcf")
			    || (Misc.filename_extension filename = "vcard") in
  let encoding = {
    Surveyor.description = "Virtual Business Card format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = reader;
    Surveyor.writer = writer;
    Surveyor.base_type = ["vcard"]
  }
  in
  Surveyor.register_encoding "vcard" encoding;
  Optometrist.register_lens ["vcard"] ["address_book"] Lens.id
