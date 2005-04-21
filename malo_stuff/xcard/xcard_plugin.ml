(* plugin for synchronizing xml vcards *)

let _ =
  let etest filename copt = Misc.filename_extension filename = "xcard" in
  let encoding = {
    Surveyor.description = "XCard (XML Vcard)";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Xml.simple_reader;
    Surveyor.writer = Xml.simple_writer;
    Surveyor.base_type = ["xml";"xcard"];
  }
  in
  Surveyor.register_encoding "xcard" encoding;
  Optometrist.register_lens ["xml"; "xcard"] ["xcard"] Lens.id;
  Optometrist.register_lens ["xcard"] ["address_book"] Lens.id
