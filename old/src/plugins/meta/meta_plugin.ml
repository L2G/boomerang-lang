(* meta.ml - meta (abstract value) *)

let reader f =
  try 
    let c = open_in f in
    let lexbuf = Lexing.from_channel c in
      try	
	Some (Compiler.compile_view_lexbuf lexbuf)
	  (*       Some (Metay.main Metal.token lexbuf) *)
      with
	| Parsing.Parse_error -> Lexer.error lexbuf "parse error"
  with 
    | Sys_error s -> 
	print_endline ("Warning: unable to read from " ^ f);
	None

let writer vo f =
  match vo with
    None -> Sys.remove f
  | Some v -> V.dump_to_file f v

let from_string s =
    try 
      let lexbuf = Lexing.from_string s in    
	Some (Compiler.compile_view_lexbuf lexbuf)
	  (* Some(Metay.main Metal.token lexbuf) *)
    with
	_ -> None

let to_string v =
  let out,flush = Format.get_formatter_output_functions () in
  let buf = Buffer.create 64 in
  Format.set_formatter_output_functions 
    (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
  V.format v;
  Format.print_flush();
  let s = Buffer.contents buf in
  Format.set_formatter_output_functions out flush;
  s

let _ =
  let id = Pervasives_plugin.id in
  let etest filename copt = Misc.filename_extension filename = "meta" in
  let encoding = {
    Surveyor.description = "Meta abstract view format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = reader;
    Surveyor.writer = writer;
    Surveyor.from_string = from_string;
    Surveyor.to_string = to_string;
    Surveyor.base_type = ["meta"];
  }
  in
  let all = Types.string2abstract_type "type X = *[X]" in
    Surveyor.register_encoding "meta" encoding;
    (* this only looks useful for debugging only. is it? -nate *)
    Optometrist.register_lens ["meta"] ["appointments"] 
      Schemas.appointments
      id;
    (* this looks bogus. is it? commenting out for now --nate *)
    (* Optometrist.register_lens ["meta"] ["abs_ical"] all (* BOGUS *) id
    *)      
