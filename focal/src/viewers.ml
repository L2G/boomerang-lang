(******************************************************************************)
(*                               Meta viewer                                  *)
(******************************************************************************)

(* viewer for "meta" format, a simple concrete syntax for trees *)

(* OLD -- NUKE
let metareader s =
  let _ = Metal.setup "meta string" in
  let lexbuf = Lexing.from_string s in        
  let res = 
    try 
      (Metay.tree Metal.token lexbuf) 
    with Parsing.Parse_error -> 
        raise (Error.Harmony_error
                 (fun () -> Util.format "%s: syntax error in meta tree." 
                    (Info.string_of_t (Metal.info lexbuf)))) in
  Metal.finish();
  res
*)

let metareader s =
  let _ = Lexer.setup "<meta string>" in
  let lexbuf = Lexing.from_string s in        
  let e = 
    try 
      (Parser.exp Lexer.main lexbuf) 
    with Parsing.Parse_error -> 
        raise (Error.Harmony_error
                 (fun () -> Util.format "%s: syntax error in meta file." 
                    (Info.string_of_t (Lexer.info lexbuf)))) in
  Lexer.finish();
  let res = Compiler.compile_exp (Compiler.empty_cenv ()) e in
  Value.get_v (Info.M "<meta string>") (Registry.value_of_rv res)

let metawriter = V.string_of_t 

let _ =
  let etest filename copt = Misc.filename_extension filename = "meta" in
  let encoding = {
    Surveyor.description = "ASCII tree format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_reader metareader;
    Surveyor.writer = metawriter;
  } in
  Surveyor.register_encoding "meta" encoding


(******************************************************************************)
(*                           One-big-blob viewer                              *)
(******************************************************************************)

let _ =
  let etest filename copt = (Misc.filename_extension filename = "txt") in
  let encoding = {
    Surveyor.description = "one-big-blob format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_tree_reader (fun s -> Tree.mk_value s);
    Surveyor.writer = (fun s -> Tree.get_value (V.tree_of (Info.M "blob writer") s));
  } in
  Surveyor.register_encoding "blob" encoding

(*
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
    Surveyor.writer = write_tbl; }
  in
    Surveyor.register_encoding "csv" encoding

let _ =
  let rec read_db cd =
    match cd with
    | Surveyor.FromString(s) ->
      raise (Error.Harmony_error (fun () -> 
        Util.format "%s" "Cannot view a string as a database"))
    | Surveyor.FromFile(fn) ->
        Treedb.db_to_tree (Csvdb.load_db fn)
  in
  let write_db tr fn =
    Csvdb.save_db fn (Treedb.tree_to_db tr)
  in
  let etest filename copt = false in
  let encoding = {
    Surveyor.description = "CSV relational database";
    Surveyor.encoding_test = etest;
    Surveyor.reader = read_db;
    Surveyor.writer = write_db;
  }
  in
    Surveyor.register_encoding "csvdb" encoding
*)

(******************************************************************************)
(*                               XML viewer                                   *)
(******************************************************************************)
open Pxp_document
open Pxp_yacc
open Pxp_types

let debug = Trace.debug "viewers+"

class warner =
  object 
    method warn w =
      print_endline ((Misc.color "WARNING: " Misc.Yellow ~bold:true) ^ w)
  end

let pcdata_tag = "@pcdata"
let pcdata_tag_lib = Value.N pcdata_tag
let _ =
  Registry.register_native
    "Native.Xml.pcdata_tag" Syntax.SName pcdata_tag_lib

let children_tag = "@children"
let children_tag_lib = Value.N children_tag
let _ =
  Registry.register_native
    "Native.Xml.children_tag" Syntax.SName children_tag_lib

let escapeXmlChar inAttr = function
    '&' -> "&amp;"
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '\'' -> if inAttr then "&apos;" else "'"
  | '"' -> if inAttr then "&quot;" else "\""
  | '\n' -> "\n" (* we don't want to escape carriage returns*)
  | c -> 
      if ((int_of_char c) >= 128) || ((int_of_char c) < 32) then 
        Printf.sprintf "&#%d;" (int_of_char c)
      else
        "-"
let escapeXml inAttr = Misc.escape (escapeXmlChar inAttr)

let rec xml2tree n =
  match n # node_type with
    T_element name -> 
      debug (fun () -> Util.format "in element \"%s\"@\n" name);
      let kids = 
        match n#sub_nodes with
          [] -> debug (fun () -> Util.format "zero subnodes@\n"); []
        | [n'] when n'#node_type=T_data && Misc.is_blank n'#data ->
            debug (fun () -> Util.format "skipping blank PCDATA@\n");
            []
        | [n'] ->
            debug (fun () -> Util.format "one nonblank subnode, recursing@\n");
            [xml2tree n']
        | ns ->
            let rec loop acc = function
                [] -> Safelist.rev acc
              | n'::ns when n'#node_type=T_data && Misc.is_blank n'#data ->
                  debug (fun () -> Util.format "skipping blank PCDATA@\n");
                  loop acc ns 
              | n'::ns ->
                  debug (fun () -> Util.format "NOT skipping blank PCDATA@\n");
                  loop ((xml2tree n')::acc) ns in
            loop [] ns in 
      let attrkids =
        Safelist.map
          (fun (k,a) ->
             match a with
               Value s -> (k, Tree.mk_value s)
             | _ -> assert false)
          n#attributes in
      let kid_struct = Tree.structure_from_list kids in
      Tree.set Tree.empty name (Some (
                            Tree.set
                            (Tree.from_list attrkids) 
                            children_tag
                            (Some kid_struct)))
  | T_data ->
      let str = Util.trimWhitespace (n # data) in
      debug (fun () -> Util.format "found string \"%s\"@\n" (Misc.whack str));
      Tree.set Tree.empty pcdata_tag (Some (Tree.mk_value str))
  | _ -> assert false

let generic_read_from rd =
  let config = { default_config with warner = new warner; 
                 encoding = `Enc_utf8;
                 drop_ignorable_whitespace = true} in 
  try 
    let docRoot = 
      parse_wfcontent_entity config rd default_spec in
    Tree.structure_from_list [xml2tree docRoot]
  with
      e -> raise (Error.Harmony_error
                    (fun () -> 
                       Util.format "%s" (Pxp_types.string_of_exn e)))

let strip_doctype = 
  Str.global_replace (Str.regexp "<!DOCTYPE[^>]*>") ""

let xmlreader ?(preproc = strip_doctype) s = 
  generic_read_from (from_string (preproc s))

let pcdata_only kids =
  Safelist.for_all
    (fun kid ->
       let d = Tree.dom kid in
       (Name.Set.cardinal d > 0) && (Name.Set.choose d = pcdata_tag))
    kids

(* assumes v is a tree of the form produced by xml2tree above *)
let rec dump_tag fmtr v =
  (* Tree.format_msg [`String "enter dump_tag"; `Tree v]; *)
  (* v should have one (singleton) child, which is the tag name *)
  let dom = Tree.dom v in
  if ((Name.Set.cardinal dom) <> 1) then
    V.error_msg [`String "dump_tag, should have 1 child ";
                 `String (Printf.sprintf "(has %d): "
                            (Name.Set.cardinal dom));
                 `Tree v];
  let tag = Name.Set.choose dom in
  debug (fun() -> Util.format "dump_tag %s @," (Misc.whack tag));
  if (tag = pcdata_tag) then
    let data = Tree.get_value (Tree.get_required v tag) in
    debug (fun() -> Util.format "dump_tag -- data %s @," (Misc.whack data));
    Format.fprintf fmtr "%s" (escapeXml false data) 
  else
    let subv = Tree.get_required v tag in
    (* now, subv should have one children_tag child (a list of sub-elements)
       and may have some other children representing attributes of the tag *)
    let kids_struct = Tree.get_required subv children_tag in
    let kids = Tree.list_from_structure kids_struct in
    let attrnames =
      Name.Set.elements (Name.Set.remove children_tag (Tree.dom subv)) in
    let attrs =
      Safelist.map
        (fun k -> (k, escapeXml true (Tree.get_value (Tree.get_required subv k))))
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
    debug (fun() -> Util.format "finished dump_tag %s @," (Misc.whack tag));
    Format.fprintf fmtr "@]";
    if not (pcdata_only kids) then Format.fprintf fmtr "@,";
    Format.fprintf fmtr "</%s>" tag

let dump_tree_as_pretty_xml fmtr v =
  if Tree.is_empty v then
    begin
      debug (fun() -> Util.format "writing out empty tree == empty file");
      Format.fprintf fmtr ""; 
    end
  else
    let tags = Tree.list_from_structure v in
    Format.fprintf fmtr "@[<v0>";
    Misc.iter_with_sep (dump_tag fmtr)
    (fun () -> Format.fprintf fmtr "@,") tags;
    Format.fprintf fmtr "@]"

let xmlwriter v =
  let buf = Buffer.create 20 in
  let fmtr = Format.formatter_of_buffer buf in
  dump_tree_as_pretty_xml fmtr v;
  Format.fprintf fmtr "@?";
  Buffer.contents buf

let _ =
  let etest filename copt =
    let ext = Misc.filename_extension filename in
    ext = "xml" || ext = "plist" in
  let encoding = {
    Surveyor.description = "XML";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Surveyor.simple_tree_reader xmlreader;
    Surveyor.writer = (fun v -> xmlwriter (V.tree_of (Info.M "xml writer") v));
  }
  in
  Surveyor.register_encoding "xml" encoding

(* The initialization function actually does nothing -- all initialization happens
   at module loading time.  However, we need something here that can be called externally
   to make sure that this module *is* loaded. *)
let init () = ()    