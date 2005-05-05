(* vcard viewer and lenses *)
(* Spec: almost vcard v2.1 *)



let reader f =
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  Some (Ldapparser.ldap Ldaplex.token lexbuf)



let writer vo f =
  match vo with
    None -> Sys.remove f
  | Some v -> V.dump_to_file f v



let _ =
  let etest filename copt = Misc.filename_extension filename = "ldif" in
  let encoding = {
    Surveyor.description = "LDAP format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = reader;
    Surveyor.writer = writer;
    Surveyor.base_type = ["ldap"]
  }
  in
  Surveyor.register_encoding "ldap" encoding;
  Optometrist.register_lens ["meta"] ["appointments"] Lens.id;
  Optometrist.register_lens ["meta"] ["abs_ical"] Lens.id
