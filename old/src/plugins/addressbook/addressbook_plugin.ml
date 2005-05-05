open Pervasives

(* ADDRESSBOOK *)
(* A self-defined schema for addressbooks - stephane *)
let addressbook_av =
  Types.string2abstract_type
    "type AB = *[ABEntryName]
     type ABEntryName = ![ABEntry]
     type ABEntry = name[Name].tel[Tel].birthday[Date].address[Address].note?[V].secemails?[Emails]
     type Name = first[V].last[V].others[VL]
     type Tel = home[V].mobile?[V].fax?[V]
     type Date = day[V].month[V].year[V]
     type Address = V
     type Emails = VLNE
     type VL = \"*h\"[V].\"*t\"[VL] | \"*nil\"[{}]
     type VLNE = \"*h\"[V].\"*t\"[VL]
     type V = ![{}]"

let addressbook_cv =
  Types.string2abstract_type
    "type AB = \"*h\"[ABEntry].\"*t\"[AB] | \"*nil\"[{}]
     type ABEntry = name[Name].tel[Tel].birthday[Date].address[Address].note?[V].email[Email]
     type Name = first[V].last[V].others[VL]
     type Tel = home[V].mobile?[V].fax?[V]
     type Date = day[V].month[V].year[V]
     type Address = V
     type Email = !\{pref, others}[{}] | pref[V].others[VL]
     type VL = \"*h\"[V].\"*t\"[VL] | \"*nil\"[{}]
     type V = ![{}]"

let xmltoaddbook = 
  "let init = hoist \"\"; hd []; hoist addressbook; hoist \"\" in
   let inh n = hoist_nonunique \"*h\" {n}  in
   let flattenvalue = hoist \"\"; hd []; hoist \"PCDATA\" in
   let flattenonames = hoist \"\"; flatten; hoist oname; list_map flattenvalue in
   let flattenoemail = hoist \"\"; flatten; hoist oemail; list_map flattenvalue; flatten; map (const {} {}) in
   let flattenbirthday = 
     hoist \"\"; 
     inh day;
     wmap <\"*t\" -> (
       inh month;
       wmap <\"*t\" -> focus \"*h\" []>;
	 hoist_nonunique \"*t\" {year, \"*h\", \"*t\"})>;
       hoist_nonunique \"*t\" {month, year, \"*h\", \"*t\"};
       map flattenvalue
   in
   let flattenemail =
     let specl = wmap < \"*t\" -> focus \"*h\" {\"*t\"=[]}>; hoist_nonunique \"*t\" {others};
       wmap < others -> flattenoemail, pref -> flattenvalue >
     in 
     hoist \"\"; wmap <\"*h\" -> acond (equalDom {pref}) (equalDom {pref}) (id) (plunge \"*main\")>;
       hoist_nonunique \"*h\" {pref, \"*main\"};
       acond (hasChild pref) (hasChild pref) specl (hoist_nonunique \"*main\" {PCDATA}; prune \"*t\" {\"*nil\"}; hoist PCDATA)
   in
   let flattenname = 
     hoist \"\";
     inh first;
     wmap <\"*t\" -> (
       inh last;
       wmap <\"*t\" -> acond (hasChild \"*h\") (hasChild \"*h\") (focus \"*h\" []) (prune \"*nil\" [])>;
	 hoist_nonunique \"*t\" {others, \"*h\", \"*t\"})>;
       hoist_nonunique \"*t\" {last, others, \"*h\", \"*t\"};
       wmap < first -> flattenvalue, last -> flattenvalue, others -> flattenonames>
   in
   let flattennote = id in
   let flattentel = 
     hoist \"\";
     inh home;
     let is_mobile = (inh mobile;
		      wmap <\"*t\" -> acond (hasChild \"*h\") (hasChild \"*h\") (focus \"*h\" []) (prune \"*nil\" [])>;
			hoist_nonunique \"*t\" {fax, \"*h\", \"*t\"}) in
     wmap <\"*t\" -> acond (hasChild \"*h\") (hasChild \"*h\") is_mobile (prune \"*nil\" []) >;
       hoist_nonunique \"*t\" {mobile, fax, \"*h\", \"*t\"};
       wmap < home -> flattenvalue, mobile -> flattenvalue, fax -> flattenvalue>
   in
   let flattenentry =
     inh name;
     wmap <\"*t\" -> (
       inh tel;
       wmap <\"*t\" -> (
	 inh birthday;
	 wmap <\"*t\" -> (
	   inh address;
	   wmap <\"*t\" -> focus \"*h\" []>;
	     hoist_nonunique \"*t\" {email, \"*h\", \"*t\"})>;
	   hoist_nonunique \"*t\" {address, email, \"*h\", \"*t\"})>;
	 hoist_nonunique \"*t\" {birthday, address, email, \"*h\", \"*t\"})>;
       hoist_nonunique \"*t\" {tel, birthday, address, email, \"*h\", \"*t\"};
       wmap < address -> flattenvalue, birthday -> flattenbirthday, email -> flattenemail, 
         name -> flattenname, note -> flattennote, tel -> flattentel>
   in
   init; 
   let treatentry = 
     hoist_nonunique entry {\"\", note};
     hoist_nonunique \"\" {\"*t\", \"*h\"};
     flattenentry
   in
   list_map treatentry"

let xmltoaddbook_lens_arg = Compiler.compile_focal xmltoaddbook

let xmltoaddbook_lens = Syntax.lensOfArg xmltoaddbook_lens_arg

let () =
  let etest filename copt = Misc.filename_extension filename = "xml" in
  let encoding = {
    Surveyor.description = "Addressbook";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Xml.simple_reader;
    Surveyor.writer = Xml.simple_writer;
    Surveyor.from_string = Xml.from_string;
    Surveyor.to_string = Xml.to_string;
    Surveyor.base_type = [ "xml"];
  }
  in    
  Surveyor.register_encoding "addressbook" encoding;
  Optometrist.register_lens ["xml"] ["addressbook"]
    addressbook_av
    xmltoaddbook_lens;
  Optometrist.register_lens ["addressbook"] ["meta"]
    addressbook_av
    Pervasives_plugin.id
