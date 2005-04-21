
let debug_err = prerr_string

let _ = 
  let fchan = open_in "test.fcl" in
  let lexbuf = Lexing.from_channel fchan in
  let ast1 = Parser.modl Lexer.token lexbuf in
  let _ = debug_err ("AST1:\n\n" ^ (Pretty.string_of_modl ast1)) in
  let ast2 = Checker.sc_modl ast1 in
  let _ = debug_err ("\nAST2:\n\n" ^ (Pretty.string_of_modl ast2)) in
  let e = Compiler.get_ev (Compiler.compile_module ast2) in
  let _ = debug_err ("\n\nRESULT: " ^ (Pretty.string_of_env e) ^ "\n") in
    ()
