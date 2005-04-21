(* $Id: xml_plugin.ml,v 1.3 2004/12/05 00:25:02 jnfoster Exp $ *)
(* plugin for synchronizing abstract xml *)

let _ =
  let etest filename copt = Misc.filename_extension filename = "xml" in
  let encoding = {
    Surveyor.description = "XML";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Xml.simple_reader;
    Surveyor.writer = Xml.simple_writer;
    Surveyor.from_string = Xml.from_string;
    Surveyor.to_string = Xml.to_string;
    Surveyor.base_type = ["xml"];
  }
  in
  Surveyor.register_encoding "xml" encoding
