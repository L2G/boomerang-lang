(******************************************************************************)
(*                               Meta viewer                                  *)
(******************************************************************************)

(* viewer for "meta" format, a simple concrete syntax for trees *)

let metareader s =
  let _ = Metal.setup "meta string" in
  let lexbuf = Lexing.from_string s in        
  let res = 
    try 
      (Metay.tree Metal.token lexbuf) 
    with Parsing.Parse_error -> 
	raise (Error.Harmony_error
		 (fun () -> Format.printf "%s: syntax error." 
		    (Info.string_of_t (Lexer.info lexbuf))))
  in
  let _ = Metal.finish () in
    res

let metawriter = V.string_of_t

let _ =
  let etest filename copt = Misc.filename_extension filename = "meta" in
  let encoding = {
    Surveyor.description = "ASCII tree format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_reader metareader;
    Surveyor.writer = Surveyor.simple_writer metawriter;
  }
  in
    Surveyor.register_encoding "meta" encoding


(******************************************************************************)
(*                           One-big-blob viewer                              *)
(******************************************************************************)

let _ =
  let etest filename copt = (Misc.filename_extension filename = "txt") in
  let encoding = {
    Surveyor.description = "one-big-blob format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_reader (fun s -> V.new_value s);
    Surveyor.writer = Surveyor.simple_writer (fun s -> V.get_value s);
  }
  in
    Surveyor.register_encoding "blob" encoding

(******************************************************************************)
(*                               CSV viewers                                  *)
(******************************************************************************)

let _ =
  let rec read_tbl cd =
    match cd with
    | Surveyor.FromString(s) ->
        let (fn, ouch) = Filename.open_temp_file "harmony" ".csv" in
        output_string ouch s;
        close_out ouch;
        let x = read_tbl (Surveyor.FromFile(fn)) in
        Sys.remove fn;
        x
    | Surveyor.FromFile(fn) ->
        Treedb.rel_to_tree (Csvdb.load_tbl fn)
  in
  let write_tbl tr fn =
    Csvdb.save_tbl fn (Treedb.tree_to_rel tr)
  in
  let etest filename copt = (Misc.filename_extension filename = "csv") in
  let encoding = {
    Surveyor.description = "CSV relational table";
    Surveyor.encoding_test = etest;
    Surveyor.reader = read_tbl;
    Surveyor.writer = write_tbl;
  }
  in
    Surveyor.register_encoding "csv" encoding

let _ =
  let rec read_db cd =
    match cd with
    | Surveyor.FromString(s) ->
      raise (Error.Harmony_error (fun () -> 
        Format.printf "%s" "Cannot view a string as a database"))
    | Surveyor.FromFile(fn) ->
        Treedb.db_to_tree (Csvdb.load_db fn)
  in
  let write_db tr fn =
    Csvdb.save_db fn (Treedb.tree_to_db tr)
  in
  let etest filename copt = true in
  let encoding = {
    Surveyor.description = "CSV relational database";
    Surveyor.encoding_test = etest;
    Surveyor.reader = read_db;
    Surveyor.writer = write_db;
  }
  in
    Surveyor.register_encoding "csvdb" encoding

(******************************************************************************)
(*                               XML viewer                                   *)
(******************************************************************************)
open Pxp_document
open Pxp_yacc
open Pxp_types

let debug = Trace.debug "xml"

class warner =
  object 
    method warn w =
      print_endline ((Misc.color "WARNING: " Misc.Yellow ~bold:true) ^ w)
  end

let pcdata_tag = "@pcdata"
let pcdata_tag_lib = Value.N pcdata_tag
let _ = Registry.register_native "Native.Xml.pcdata_tag" "name" pcdata_tag_lib

let children_tag = "@children"
let children_tag_lib = Value.N children_tag
let _ = Registry.register_native "Native.Xml.children_tag" "name" children_tag_lib

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

let rec xml2tree n =
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
            [xml2tree n']
        | ns ->
            let rec loop acc = function
                [] -> Safelist.rev acc
              | n'::ns when n'#node_type=T_data && Misc.is_blank n'#data ->
                  debug (fun () -> Format.printf "skipping blank PCDATA\n");
                  loop acc ns 
              | n'::ns ->
                  debug (fun () -> Format.printf "NOT skipping blank PCDATA\n");
                  loop ((xml2tree n')::acc) ns in
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
                            children_tag
                            (Some kid_struct)))
  | T_data ->
      let str = Util.trimWhitespace (n # data) in
      debug (fun () -> Format.printf "found string \"%s\"\n" (Misc.whack str));
      V.set V.empty pcdata_tag (Some (V.new_value str))
  | _ -> assert false

let generic_read_from rd =
  let config = { default_config with warner = new warner; 
		 encoding = `Enc_utf8;
		 drop_ignorable_whitespace = true} in 
  try 
    let docRoot = 
      parse_wfcontent_entity config rd default_spec in
    V.structure_from_list [xml2tree docRoot]
  with
      e -> raise (Error.Harmony_error
		    (fun () -> 
		       Format.printf "%s" (Pxp_types.string_of_exn e)))

let strip_doctype = 
  Str.global_replace (Str.regexp "<!DOCTYPE[^>]*>") ""

let xmlreader ?(preproc = strip_doctype) s = 
  generic_read_from (from_string (preproc s))

let pcdata_only kids =
  Safelist.for_all
    (fun kid ->
       let d = V.dom kid in
       (Name.Set.cardinal d > 0) && (Name.Set.choose d = pcdata_tag))
    kids

(* assumes v is a tree of the form produced by xml2tree above *)
let rec dump_tag fmtr v =
  (* V.format_msg [`String "enter dump_tag"; `Tree v]; *)
  (* v should have one (singleton) child, which is the tag name *)
  let dom = V.dom v in
  if ((Name.Set.cardinal dom) <> 1) then
    V.error_msg [`String "dump_tree_as_pretty_xml, should have 1 child ";
		 `String (Printf.sprintf "(has %d): "
			    (Name.Set.cardinal dom));
		 `Tree v];
  let tag = Name.Set.choose dom in
  debug (fun() -> Format.printf "dump_tag %s @," (Misc.whack tag));
  if (tag = pcdata_tag) then
    let data = V.get_value (V.get_required v tag) in
    debug (fun() -> Format.printf "dump_tag -- data %s @," (Misc.whack data));
    Format.fprintf fmtr "%s" (escapeXml data) 
  else
    let subv = V.get_required v tag in
    (* now, subv should have one children_tag child (a list of sub-elements)
       and may have some other children representing attributes of the tag *)
    let kids_struct = V.get_required subv children_tag in
    let kids = V.list_from_structure kids_struct in
    let attrnames =
      Name.Set.elements (Name.Set.remove children_tag (V.dom subv)) in
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

let dump_tree_as_pretty_xml fmtr v =
  if V.is_empty v then
    begin
      debug (fun() -> Format.printf "writing out empty tree == empty file");
      Format.fprintf fmtr ""; 
    end
  else
    let tags = V.list_from_structure v in
    Format.fprintf fmtr "@[<v0>";
    Misc.iter_with_sep (dump_tag fmtr)
    (fun () -> Format.fprintf fmtr "@,") tags;
    Format.fprintf fmtr "@]"

let xmlwriter v =
  let buf = Buffer.create 20 in
  let fmtr = Format.formatter_of_buffer buf in
  dump_tree_as_pretty_xml fmtr v;
  Format.fprintf fmtr "\n@.";
  Buffer.contents buf

let _ =
  let etest filename copt = Misc.filename_extension filename = "xml" in
  let encoding = {
    Surveyor.description = "XML";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_reader xmlreader;
    Surveyor.writer = Surveyor.simple_writer xmlwriter;
  }
  in
  Surveyor.register_encoding "xml" encoding
