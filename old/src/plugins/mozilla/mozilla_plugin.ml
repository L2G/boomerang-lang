(* $Id: mozilla_plugin.ml,v 1.7 2004/12/05 00:25:01 jnfoster Exp $ *)
(* synchronizing mozilla bookmarks *)
(* separators DON'T work!  they cause the lens to break :( *)

let mozilla_bookmark_arg =
  Compiler.compile_focal
"let remove_atomic = fork {*} (const {} {*}) id

let hoist_hd p = 
  hoist_nonunique *h p;remove_atomic;hoist_nonunique *t (neg p)

let link =
        hoist \"\" ;
	hd [];(* This hoist_nonnique was in the previous bookmark lens, why ?
	hoist_nonunique a {\"\" \"id\" last_charset last_visit add_date href} *)
	hoist a;
	filter {\"\" href add_date} {};
	rename \"\" name;
	rename href url;
	prune add_date { \"01/01/1970\"} ;
	wmap < name -> (hd [];hoist PCDATA) >

let rec folder =
	hoist \"\";
	hoist_hd {h3};
	fork {h3} id (hoist_hd {dl});
	remove_atomic;
	rename h3 name;
	rename dl contents;
	wmap < name -> (filter {\"\"} {};hoist \"\";hd [];hoist PCDATA)
	       contents -> (hoist \"\";list_map item)>
and item = 
	wmap < dd -> folder 
	       dt -> link > ;
	rename_if_present dd \"folder\";
	rename_if_present dt \"link\"

and bookmarks =
	hoist \"\";
	hd [];
	hoist html;
	hoist \"\";
	tl {head = {\"\"=[{title={\"\"=[{PCDATA= Bookmarks}]}}]}};
	hd [];
	hoist body;
	hoist \"\";
	tl {h1 ={\"\" =[{PCDATA = Bookmarks}]}};
	hd [];
	hoist dl;
	hoist \"\";
	list_map item;
        (* strip preamble *)
        hd []; 
        focus \"folder\" {}; 
        focus \"contents\" {}
do bookmarks"

let mozilla_bookmark = Syntax.lensOfArg mozilla_bookmark_arg

(*
let toolbar_folder =
  tracepoint "Mozilla.toolbar_folder" [
    lens;
    Bookmarks.focus_item "Personal Toolbar Folder"
  ]
 *)
(*let strip_p = Str.global_replace (Str.regexp "<p>") ""
let strip_meta = Str.global_replace (Str.regexp "<META[^>]*>") ""
let strip_title = Str.global_replace (Str.regexp "<TITLE[^>]*>.*</TITLE>") ""
let strip_h1 = Str.global_replace (Str.regexp "<H1[^>]*>.*</H1>") ""
let preprocess = Misc.composel
  [Xml.strip_doctype; strip_p; strip_meta; strip_title; strip_h1]*)

(* BCP: Is this needed to avoid using tidy?  Why can't it just be done with
   a global rename? *)
let dd_to_dt = Str.global_replace (Str.regexp "<dd>") "<dt>"
let cdd_to_cdt = Str.global_replace (Str.regexp "</dd>") "</dt>"
let postproc = Misc.composel [dd_to_dt; cdd_to_cdt]

let etest filename copt = 
    try "<!DOCTYPE NETSCAPE-Bookmark-file-1>" = input_line (open_in filename)
    with _ -> false 

let _ =
  (* To improve (conflicts with classical html files) *)
  let encoding = {
    Surveyor.description = "Mozilla bookmarks";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Html.gen_reader ~tidy:true ();
    Surveyor.writer = Html.gen_writer ~postproc:postproc ();
    Surveyor.from_string = Html.from_string;
    Surveyor.to_string = Html.to_string;
    Surveyor.base_type = ["html"; "mozilla"];
  }
  in
    Surveyor.register_encoding "mozilla" encoding;
    Optometrist.register_lens ["html"; "mozilla"] ["mozilla"] 
      Schemas.bookmarks 
      mozilla_bookmark;
    Optometrist.register_lens ["mozilla"] ["bookmarks"]  
      Schemas.bookmarks 
      Pervasives_plugin.id;
    (*  Optometrist.register_lens ["mozilla"] ["mozilla-toolbar-folder"] Lens.id (* toolbar_folder *) 
	Is this special lens intersting ??? *)
