(* meta.ml - meta (abstract value) *)

let reader s =
  let lexbuf = Lexing.from_string s in        
    try 
      (Metay.view Metal.token lexbuf) 
    with Parsing.Parse_error -> 
      raise (Error.Run_error ("Parse error in view at: " ^ Info.string_of_t (Metal.info lexbuf)))
	
let writer = V.string_of_t

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
