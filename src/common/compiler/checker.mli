
val init_varcounter : unit -> unit

val nicetypesoftheprog : 
  Syntax.prog -> 
    ((string * Syntax.typeschema) list * Syntax.prog) * 
       (Syntax.typedef * Syntax.typeschema)

val types_to_string : 
  (Syntax.typedef * Syntax.typeschema) -> string

(* used in Compiler.compile_view to figure out which AstVars are names
and which are really variables *)

val annot_view :
  Syntax.expr -> Syntax.expr
