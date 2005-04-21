(* $Id: html.ml,v 1.2 2004/08/04 17:10:23 denielou Exp $ *)

(* For future reference, here are some potential problems with the HTML
 * viewer:
 * - it ignores comments
 * - it doesn't agree with <!...> declarations (e.g. doctype)
 * - it ignores processing instructions (<? ... ?>)
 *)

let pcdata_tag = "PCDATA"

let debug = Trace.debug "html"

let rec doc2string = function
  Nethtml.Data s -> ("Data ('" ^ s ^ "')")
| Nethtml.Element(name, args, kids) ->
    "Element (" ^ name ^ ", "
    ^ "[" ^
      (String.concat "; "
        (Safelist.map (fun (k,a) -> "(" ^ k ^ ", " ^ a ^ ")") args))
    ^ "], "
    ^ "[" ^ (String.concat "; " (Safelist.map doc2string kids)) ^ "])"

let rec doc2view = function
    Nethtml.Data s ->
      debug (fun () -> Format.printf "Data: %s\n" (Misc.whack s));
      let str = Util.trimWhitespace s in
      V.set V.empty pcdata_tag (Some (V.new_value str))
  | Nethtml.Element(name,args,kids) ->
      let newkids = (* Safelist.map doc2view kids in *)
        (* FIXME: this is uglier than it has to be *)
        match kids with
          [] -> []
        | [d] -> [doc2view d]
        | ds ->
            let rec loop acc = function
                [] -> Safelist.rev acc
              | (Nethtml.Data s)::ds when Misc.is_blank s -> loop acc ds
              | d::ds -> loop ((doc2view d)::acc) ds in
            loop [] ds in
      let attrkids = Safelist.map (fun (k,a) -> k, V.new_value a) args in
      let kid_struct = V.structure_from_list newkids in
      V.set V.empty name (Some (V.set (V.from_list attrkids) "" (Some kid_struct)))

let from_lexbuf lb =
  let doclist = Nethtml.parse_document ~dtd:(Nethtml.relaxed_html40_dtd) lb in
  let decoded_doclist = Nethtml.decode doclist in
  (*
  Safelist.iter (fun d -> print_endline ("--> " ^ (doc2string d))) decoded_doclist;
  *)
  V.set V.empty "" (Some
  (V.structure_from_list (Safelist.map (fun d -> doc2view d) 
                                   (Safelist.filter (function 
                                     | Nethtml.Data s -> not (Misc.is_blank s)
                                     | _ -> true) decoded_doclist))))

let from_file fname =
  try
    let ch = open_in fname in
    let v = from_lexbuf (Lexing.from_channel ch) in
    close_in ch; Some v
  with Sys_error s ->
        print_endline ("Warning [Html.from_file]: unable to view file "
                      ^ fname ^ ": " ^ s);
        None

let from_string s =
  Some (from_lexbuf (Lexing.from_string s))

let gen_reader ?(preproc = (fun s->s)) ?(tidy = false) () = fun file ->
  try
    let raw_html = Misc.read file in
     (*Format.printf "Here is the raw HTML that I read in from %s...@,"
    inname; Format.printf "%s@," raw_html; *)
    let preproc'd_html = preproc raw_html in
     (*Format.printf "Here is the fixed HTML after preprocessing...@,";
       Format.printf "%s@," preproc'd_html; *)
    let tidy_html =
      if tidy then
        let out,status = Tidy.fix_string [Tidy.ShowWarnings false;
                                          Tidy.Quiet true;
                                          Tidy.Mark false;
                                          Tidy.WrapLen 0]
                                          preproc'd_html in
        match status with
          Tidy.Success -> out
        | Tidy.Warning ->
            print_endline "[Html.gen_reader] tidy issued warnings."; out
      else preproc'd_html
    in
     (*print_endline (Misc.color "Tidied HTML:" Misc.White ~bold:true);
     print_endline tidy_html;*)
    from_string tidy_html
  with Sys_error s ->
    prerr_endline ("[Html.reader " ^ file ^ "]: unable to view file: " ^ s);
    None

let gen_writer ?(postproc = (fun s->s)) () vo file =
  match vo with
    None ->
      raise
      (Misc.Unimplemented "[Html.get_writer] writing empty view unsupported.")
  | Some v ->
      let synced_html = Xml.to_string v in
      (* Format.printf "Here is the HTML that I'm about to write out...@,";
         Format.printf "%s@," synced_html; *)
      let postproc'd_html = postproc synced_html in
      (* Format.printf "Here it is again after postprocessing...@,";
         Format.printf "%s@," synced_and_cooked; *)
      Misc.write file postproc'd_html

let to_string ?(postproc = (fun s->s)) v =
  let synced_html = Xml.to_string v in
  postproc synced_html

let simple_reader = gen_reader ()

let simple_writer = gen_writer ()
