(* $Id: xmlarchive.ml,v 1.3 2004/12/05 00:25:02 jnfoster Exp $ *)

open Pxp_document
open Pxp_yacc
open Pxp_types

class warner =
  object 
    method warn w =
      print_endline ("WARNING: " ^ w)
  end

let get_field n : string =
  match (n # node_type) with
    T_element _ ->
      (match (n # sub_nodes) with
	[sub1] ->
	  (match (sub1 # node_type) with
	    T_data -> Misc.unescaped (sub1 # data)
	  | _ -> Misc.bad "Xmlarchive.get_label: expecting T_data")
      | [] -> ""
      | l -> Misc.bad 
	    (Printf.sprintf "Xmlarchive.get_label: got %d kids"
	       (Safelist.length l)))
  | _ -> assert false
	
let rec xmlelts2view elts : V.t =
  V.from_list
    (Safelist.map
       (fun n ->
	 match (n # node_type) with
	   T_element "sub" ->
	     (match (n # sub_nodes) with
	       [ln;lv'] ->
		 (match (ln # node_type, lv' # node_type) with
		   T_element "label", T_element "xmlview" ->
		     (get_field ln, xmlelts2view (lv' # sub_nodes))
		 | _ -> Misc.bad ("Xmlarchive.xmlelts2view: "^
				  "wrong node types"))
	     | _ -> Misc.bad ("Xmlarchive.xmlelts2view: "^
			      "wrong number of sub kids"))
	 | _ -> Misc.bad "Xmlarchive.xmlelts2view: unrecognized node")
       elts)
       
let read_from file_name = 
  let config = { default_config with warner = new warner; 
		 drop_ignorable_whitespace = true} in 
  try 
    let docRoot = 
      parse_wfcontent_entity config (from_file file_name) default_spec in
    match (docRoot # node_type) with
      T_element "xmlview" -> xmlelts2view (docRoot # sub_nodes)
    | _ -> Misc.bad "Viewasxml.read_from: don't recognize top level"
  with
    e -> Misc.bad (Pxp_types.string_of_exn e)

let write_to v file_name =
  let ch = open_out file_name in
  let rec write_view v =
    output_string ch 
      (Printf.sprintf "<xmlview>");
    V.iter
      (fun k' v' -> 
	output_string ch 
	  (Printf.sprintf "<sub><label>%s</label>" 
	     (Xml.escapeXml (String.escaped k')));
	write_view v';
	output_string ch "</sub>")
      v;
    output_string ch "</xmlview>" in
  write_view v;
  close_out ch

let reader file =
  try
    Some (read_from file)
  with Misc.Bad s ->
    begin
      print_endline ("[Xmlarchive.reader] using empty archive ("^s^").");
      None
    end
  | _ ->
    begin
      print_endline ("[Xmlarchive.reader] using empty archive.");
      None
    end

let writer vo file =
  match vo with
    None -> failwith "[Xmlarchive.writer] unimplemented."
  | Some v -> write_to v file

let encoding = {     
  Surveyor.description = "XML Archive";
  Surveyor.encoding_test = (fun filename _ -> Misc.filename_extension filename = "harmony");
  Surveyor.reader = reader;
  Surveyor.writer = writer;
  Surveyor.from_string = (fun s -> failwith "xmlarchive from_string not implemented");
  Surveyor.to_string = (fun s -> failwith "xmlarchive to_string not implemented");
  Surveyor.base_type = [];
}

let _ = Surveyor.register_encoding "xmlarchive" encoding;

