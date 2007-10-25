
(* parse an AST from a lexbuf *)
let parse_lexbuf lexbuf = 
  try Rparser.modl Lexer.main lexbuf 
  with Parsing.Parse_error ->
    Rparser.syntax_error 
      (Lexer.info lexbuf) 
      (fun () -> Util.format "@[syntax error@]")

let m_check n m_str ast= 
  if n = m_str then ()
  else
    sort_error 
      (info_of_module ast)
      (fun () -> Util.format "@[module %s must appear in a file named %s.src or %s.boom.@]"
         m_str (String.uncapitalize m_str))

(* end-to-end compilation of files *)
let compile_lexbuf lexbuf n = 
  let ast = parse_lexbuf lexbuf in
  let m_str = string_of_id (id_of_modl ast) in 
  let _ = m_check n m_str ast in
  let ast' = check_module ast in 
  let _ = compile_module ast' in 
    ()

let compile_boom fn n = 
  let boom_buf = Src2fcl.fcl_of_src fn in 
  let _ = Lexer.setup fn in          
  let lexbuf = Lexing.from_string boom_buf in 
  let _ = compile_lexbuf lexbuf n in
    Lexer.finish ()

let compile_boom_str s n = 
  let _ = Lexer.setup "<string constant>" in    
  let lexbuf = Lexing.from_string (Src2fcl.fcl_of_src_str s) in
  let _ = compile_lexbuf lexbuf n in 
    Lexer.finish ()

let compile_src_str s n = compile_boom_str (Src2fcl.fcl_of_src_str s) n

let compile_file fn n = compile_boom fn n 

(* ugly hack!! this thunk forces loading of this module when used via
   harmony.cmxa (which is occasionally dynamically linked) *)
let init () = 
  Registry.compile_file_impl := compile_file;
  Registry.compile_boom_str_impl := compile_boom_str;
