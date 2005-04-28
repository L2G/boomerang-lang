(* meta.ml - meta (abstract value) *)

let reader s =
  try 
    let lexbuf = Lexing.from_string s in    
      (Parser.ext_view Lexer.token lexbuf) 
  with
      _ -> assert false (* FIXME: put a meaningful error message here *)
	
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
