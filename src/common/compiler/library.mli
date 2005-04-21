val compile_view_impl : (string -> V.t) ref

val get_lens_library : unit -> (string * Syntax.typeschema * Syntax.arg) list

type test_suite = unit -> unit
val get_tests : unit -> (test_suite)
  
type test_desc = 
  | GetTest of (Syntax.arg list) * V.t * (V.t -> V.t -> bool) * string * V.t
  | PutTest of (Syntax.arg list) * V.t * (V.t option) * (V.t -> V.t -> bool) * string * V.t
  | GetFailTest of (Syntax.arg list) * V.t 
  | PutFailTest of (Syntax.arg list) * V.t * (V.t option)
      
val test_get_eq : Syntax.arg list -> string -> string -> test_desc
val test_put_eq : Syntax.arg list -> string -> string option -> string -> test_desc
val test_get_fail : Syntax.arg list -> string -> test_desc
val test_put_fail : Syntax.arg list -> string -> string option -> test_desc

val compute_lens_opt : Syntax.arg -> (Syntax.arg list) -> Lens.t option
val compute_lens : Syntax.arg -> (Syntax.arg list) -> Lens.t

val register_and_test : (string * Syntax.typeschema * Syntax.arg) -> test_desc list -> unit
val register : (string * Syntax.typeschema * Syntax.arg) -> unit

val lookup : string          -> (Syntax.typeschema * Syntax.arg) option
val lookup_required : string -> (Syntax.typeschema * Syntax.arg)

