open Bsyntax
open Bcompiler

(* parse an AST from a lexbuf *)
let parse_lexbuf lexbuf = 
  try Bparser.modl Blexer.main lexbuf 
  with Parsing.Parse_error ->
    raise 
      (Error.Harmony_error 
          (fun () -> Util.format "@[%s: Syntax error @\n@]"
            (Info.string_of_t (Blexer.info lexbuf))))

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
  let m_str = string_of_id (id_of_module ast) in 
  let _ = m_check n m_str ast in
  let ast' = check_module ast in 
  let _ = compile_module ast' in 
    ()

let compile_boom fn n = 
  let boom_buf = Src2fcl.fcl_of_src fn in
  let _ = Blexer.setup fn in
  let lexbuf = Lexing.from_string boom_buf in 
  let _ = compile_lexbuf lexbuf n in
    Blexer.finish ()

let compile_boom_str s n = 
  let _ = Blexer.setup "<string constant>" in    
  let lexbuf = Lexing.from_string (Src2fcl.fcl_of_src_str s) in
  let _ = compile_lexbuf lexbuf n in 
  Blexer.finish ()

let compile_src_str s n = compile_boom_str (Src2fcl.fcl_of_src_str s) n

let compile_file fn n = compile_boom fn n 

(* ugly hack!! this thunk forces loading of this module when used via
   harmony.cmxa (which is occasionally dynamically linked) *)
let init () = 
  Bregistry.compile_file_impl := compile_file;
  Bregistry.compile_boom_str_impl := compile_boom_str;
