val compileprog : Syntax.typedef * Syntax.prog -> Lens.t
val compile_focal : string -> Syntax.arg
val compile_view_lexbuf : Lexing.lexbuf -> V.t
val compile_view : string -> V.t

