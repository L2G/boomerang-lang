(* meta.ml - meta (abstract value) *)

let reader s =
  let lexbuf = Lexing.from_string s in        
    try 
      (Parser.ext_view Lexer.token lexbuf) 
    with Parsing.Parse_error -> 
      raise (Error.Run_error ("Parse error at: " ^ Info.string_of_t (Lexer.info lexbuf)))
      
let writer v =
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
  let etest filename copt = Misc.filename_extension filename = "meta" in
  let encoding = {
    Surveyor.description = "ASCII view format";
    Surveyor.encoding_test = etest;
    Surveyor.reader = reader;
    Surveyor.writer = writer;
  }
  in
    Surveyor.register_encoding "meta" encoding
