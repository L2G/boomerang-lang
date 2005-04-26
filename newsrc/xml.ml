(* $Id: xml.ml,v 1.1 2004/08/03 21:12:16 denielou Exp $ *)
(* support for reading/writing XML data *)

(* [This comment is a bit out of date...]

   An XML element is either a data or is a tag. If it is data, we represent
   this as a value containing the data, pointed to by a PCDATA child, or:
     {PCDATA = {<data> = {}
                *** = {}}
   If it is a tag, it may have attribute elements and/or sub-elements. We
   recurse on the sub-elements, and create a list structure, storing it
   under "", and store the attribute values under their names. This whole
   view is then stored under the tag name. So:
     <t a1="v1" a2="v2">hi</t>
   is represented as:
     {t = {"" = {PCDATA = {hi = {}
                           *** = {}}
                 *** = {}}
           a1 = {v1 = {}
                 *** = {}}
           a2 = {v2 = {}
                 *** = {}}}}
   Now a true XML document consists of one top-level element, but HTML may
   have multiple top-level elements. So that we may re-use code for writing
   the views out, we represent the entire document as a list structure of
   top-level elements stored under "".
*)

open Pxp_document
open Pxp_yacc
open Pxp_types

let debug = Trace.debug "xml"

class warner =
  object 
    method warn w =
      print_endline ((Misc.color "WARNING: " Misc.Yellow ~bold:true) ^ w)
  end

let pcdata_tag = "PCDATA"

let escapeXmlChar = function
    '&' -> "&amp;"
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '\'' -> "&apos;"
  | '"' -> "&quot;"
  | c -> 
      if ((int_of_char c) >= 128) || ((int_of_char c) < 32) then 
	Printf.sprintf "&#%d;" (int_of_char c)
      else
	"-"
let escapeXml = Misc.escape escapeXmlChar

let rec xml2view n =
  match n # node_type with
    T_element name -> 
      debug (fun () -> Format.printf "in element \"%s\"\n" name);
      let kids = 
        match n#sub_nodes with
          [] -> debug (fun () -> Format.printf "zero subnodes\n"); []
        | [n'] when n'#node_type=T_data && Misc.is_blank n'#data ->
            debug (fun () -> Format.printf "skipping blank PCDATA\n");
            []
        | [n'] ->
            debug (fun () -> Format.printf "one nonblank subnode, recursing\n");
            [xml2view n']
        | ns ->
            let rec loop acc = function
                [] -> Safelist.rev acc
              | n'::ns when n'#node_type=T_data && Misc.is_blank n'#data ->
                  debug (fun () -> Format.printf "skipping blank PCDATA\n");
                  loop acc ns 
              | n'::ns ->
                  debug (fun () -> Format.printf "NOT skipping blank PCDATA\n");
                  loop ((xml2view n')::acc) ns in
            loop [] ns in 
      let attrkids =
        Safelist.map
          (fun (k,a) ->
             match a with
               Value s -> (k, V.new_value s)
             | _ -> assert false)
          n#attributes in
      let kid_struct = V.structure_from_list kids in
      V.set V.empty name (Some (
                            V.set
                            (V.from_list attrkids) 
                            ""
                            (Some kid_struct)))
  | T_data ->
      let str = Util.trimWhitespace (n # data) in
      debug (fun () -> Format.printf "found string \"%s\"\n" (Misc.whack str));
      V.set V.empty pcdata_tag (Some (V.new_value str))
  | _ -> assert false

let generic_read_from rd =
  let config = { default_config with warner = new warner; 
		 drop_ignorable_whitespace = true} in 
  try 
    let docRoot = 
      parse_wfcontent_entity config rd default_spec in
    V.set V.empty "" (Some (V.structure_from_list [xml2view docRoot]))
  with
    e -> Misc.bad (Pxp_types.string_of_exn e)

let reader s = generic_read_from (from_string s)

let pcdata_only kids =
  Safelist.for_all
    (fun kid ->
       let d = V.dom kid in
       (Name.Set.cardinal d > 0) && (Name.Set.choose d = pcdata_tag))
    kids

(* assumes v is a view of the form produced by xml2view above *)
let rec dump_tag fmtr v =
  (* V.format_msg [`String "enter dump_tag"; `View v]; *)
  (* v should have one (singleton) child, which is the tag name *)
  let dom = V.dom v in
  if ((Name.Set.cardinal dom) <> 1) then
    V.error_msg [`String "dump_view_as_pretty_xml, should have 1 child ";
		 `String (Printf.sprintf "(has %d): "
			    (Name.Set.cardinal dom));
		 `View v];
  let tag = Name.Set.choose dom in
  debug (fun() -> Format.printf "dump_tag %s @," (Misc.whack tag));
  if (tag = pcdata_tag) then
    let data = V.get_value (V.get_required v tag) in
    debug (fun() -> Format.printf "dump_tag -- data %s @," (Misc.whack data));
    Format.fprintf fmtr "%s" (escapeXml data) 
  else
    let subv = V.get_required v tag in
    (* now, subv should have one "" child (a list of sub-elements) and may *)
    (* have some other children representing attributes of the tag *)
    let kids_struct = V.get_required subv "" in
    let kids = V.list_from_structure kids_struct in
    let attrnames = Name.Set.elements (Name.Set.remove "" (V.dom subv)) in
    let attrs =
      Safelist.map
	(fun k -> (k, escapeXml (V.get_value (V.get_required subv k))))
        attrnames in
    Format.fprintf fmtr "@[<v2><%s" tag;
    Format.fprintf fmtr "@[<hv>";
    (* CANNOT have a break between attributes or mozilla bookmarks breaks.. *)
    Safelist.iter (fun (k,a) -> Format.fprintf fmtr " %s=\"%s\"" k a) attrs;
    Format.fprintf fmtr "@]>";
    if (kids <> []) then begin
      if not (pcdata_only kids) then Format.fprintf fmtr "@,";
      Misc.iter_with_sep (dump_tag fmtr)
	(fun () -> Format.fprintf fmtr "@ ") kids;
    end;
    debug (fun() -> Format.printf "finished dump_tag %s @," (Misc.whack tag));
    Format.fprintf fmtr "</%s>@]" tag

let dump_view_as_pretty_xml fmtr v =
  if V.is_empty v then
    begin
      debug (fun() -> Format.printf "writing out empty view == empty file");
      Format.fprintf fmtr "";
    end
  else
    let tags = V.list_from_structure (V.get_required v "") in
    Format.fprintf fmtr "@[<v0>";
    Misc.iter_with_sep (dump_tag fmtr)
    (fun () -> Format.fprintf fmtr "@,") tags;
    Format.fprintf fmtr "@]"

let writer v =
  let buf = Buffer.create 20 in
  let fmtr = Format.formatter_of_buffer buf in
  dump_view_as_pretty_xml fmtr v;
  Format.fprintf fmtr "\n@.";
  Buffer.contents buf

let strip_doctype = Str.global_replace (Str.regexp "<!DOCTYPE[^>]*>") ""

let _ =
  let etest filename copt = Misc.filename_extension filename = "xml" in
  let encoding = {
    Surveyor.description = "XML";
    Surveyor.encoding_test = etest;
    Surveyor.reader = reader;
    Surveyor.writer = writer;
  }
  in
  Surveyor.register_encoding "xml" encoding