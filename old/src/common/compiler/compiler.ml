(**********************)
(* Compiler for Focal *)
(**********************)

open Lens
open Error
open Syntax
open Library

(* backpatch definitions of tracepoint, id from the library *)

(** recursive functions are automatically tracepointed below **)
let tracepoint_def:(arg option ref) = ref None
let tracepoint n l =
  let td = 
    match (!tracepoint_def) with
      | None -> 
	  let (_,e) = lookup_required "tracepoint" in
	    (tracepoint_def := Some e); e
      | Some e -> e in
  let f  = (funOfArg td) in
  let f' = (funOfArg (f (N n))) in
    lensOfArg (f' (L l))
      
let id_def:(arg option ref) = ref None
let id () = 
  lensOfArg(
    match (!id_def) with 
      | None -> 
	  let (_,e) = lookup_required "id" in
	    (id_def := Some e); e
      | Some e   -> e)


(* Library of base functions *)
let primlib () = List.map (function (s,_,arg) -> (s,arg)) (get_lens_library ())

(*****************)
(* Main compiler *)
(*****************)
let compileexpr = 
(* Conversions of the views, predicates and bijections *)
  let rec ast2view b viewenv varenv= function
      [] -> 
	if b then
	  V.empty_list
	else
	  V.empty
    | (ast1,ast2)::q -> 
	match compile viewenv varenv ast2 with
	    V v ->
	      if b 
	      then V.cons v (ast2view b viewenv varenv q)
		(* V.set (V.set (V.empty) "*h" (Some  v)) "*t" (Some (ast2view b viewenv varenv q)) *)
	      else let s =
		match compile viewenv varenv ast1 with
		    N n -> n
		  | _ -> raise (Run_error ("You don't use a correct Name",getInfo ast1))
	      in
	      V.set (ast2view b viewenv varenv q) s (Some v)
	  | _ -> raise (Run_error ("You don't use a correct View",getInfo ast2))
	      
  and ast2pred viewenv varenv l n = match l with
      [] -> false
    | (ast,_)::q -> 
	let s =
	  match compile viewenv varenv ast with
	      N n1 -> n1
	    | _ -> raise (Run_error ("You don't use a correct Name",getInfo ast))
	in n = s || (ast2pred viewenv varenv q n)
		 
  (* takes an env for views and variables and a prog and returns a lens in a unsable abstract syntax *)	 
  and compile viewenv varenv= function
      AstVar (x,i) -> 
	if List.mem_assoc x (varenv) then List.assoc x (varenv)
	else raise (Run_error ("Unbound variable "^x,i))
    | AstApp (e1,e2,i) -> 
	begin
	  match compile viewenv varenv e1 with
	      F f -> f (compile viewenv varenv e2)
	    | _ -> raise (Run_error ("You don't apply a function",i))
	end
    | AstView (l,s,b,_) -> 
	if List.mem_assoc s viewenv then
	  begin
	    match (List.assoc s viewenv) with
		T View -> V (ast2view b viewenv varenv l)
	      | T Predicate -> P (ast2pred viewenv varenv l)
	      | T (Undef _) -> V (ast2view b viewenv varenv l)
	      | _ -> V (V.empty)
	  end
	else
	  V (ast2view b viewenv varenv l)
    | AstMap (l,i) -> 
	let lmap = List.map 
		     (function (AstName(s,_),exp) -> 
			(s,compile viewenv varenv exp)
			| _ -> raise (Run_error ("The tree is not a name!",i))) l in
	  M (function s -> if List.mem_assoc s lmap then lensOfArg (List.assoc s lmap) else id ())
    | AstFun (s,e,_) -> F (function x -> compile viewenv ((s,x)::varenv) e)
    | AstName (s, _) -> N s
    | AstLetrec(ldef,ex,_) ->
	let temp = Hashtbl.create 1 in
	let collect = List.map (function (f,_,_,_) ->
				  (f, L (tracepoint f (Lens.named ~hashtable:temp f)))) in
	let newenv = (collect ldef) in
	  List.iter (function (f,_,e,_) -> 
		       Hashtbl.add temp f (Lens.memoize (lensOfArg (compile viewenv (newenv@varenv) e)))) ldef;
	  compile viewenv (newenv@varenv) ex
    | AstLet(x,_,ex1,ex2,_) -> 
	let ex1' = compile viewenv varenv ex1 in
	  compile viewenv ((x,ex1')::varenv) ex2
  in
    compile

(* Compile a definition *)
let compiledef viewenv varenv= function
    Deflet (f,_,e,_) -> [f,compileexpr viewenv varenv e]@varenv
  | Defletrec (ldef) -> 
      let temp = Hashtbl.create 1 in
      let collect = List.map (function (f,_,_,_)->(f,L (tracepoint f (Lens.named ~hashtable:temp f)))) in
      let newenv = (collect ldef)@varenv in
      List.iter (function (f,_,e,_) -> 
		   Hashtbl.add temp f (Lens.memoize (lensOfArg (compileexpr viewenv newenv e)))) ldef;
      newenv

(* Compile a hole program using the output of the typechecker *)
let compileprog (viewenv,(deflist,expr)) =
  let rec compildeflist varenv = function
      [] -> varenv
    | d::q -> compildeflist (compiledef viewenv varenv d) q in
  let finalenv = compildeflist (primlib ()) deflist in
  match (compileexpr viewenv finalenv expr) with
      L l -> l
    | _ -> raise (Run_error ("The tree is not a lens!",bogusInfo))

(* Complete compilation *)    
let compile_focal s =
  try 
    Lexer.lineno := 0 ;
    Lexer.linestart := 1;
    let lexbuf = Lexing.from_string s in
    let ast    = Parser.prog Lexer.token lexbuf in
    let (viewenv,(deflist,expr)),_  = Checker.nicetypesoftheprog ast in 
    let rec compildeflist varenv = function
	[] -> varenv
      | d::q -> compildeflist (compiledef viewenv varenv d) q in
    let finalenv = compildeflist (primlib ()) deflist in
      compileexpr viewenv finalenv expr
  with e ->
    (Format.print_string ("ERROR while compiling:\n" ^ s); Format.print_newline ();
     raise e)
    
(* Helper function for compiling views from strings in FOCAL syntax *)
let compile_view_lexbuf lexbuf = 
    let astv = Parser.view Lexer.token lexbuf in
    let astv_annot = Checker.annot_view astv in
      match (compileexpr [] [] astv_annot) with
	  V v -> v
	| _   -> raise (Run_error ("The tree is not a view!", bogusInfo))    

let compile_view s = 
  Lexer.lineno := 0 ;
  Lexer.linestart := 1;
  let lexbuf = Lexing.from_string s in
    try
      compile_view_lexbuf lexbuf	
    with e ->
      (Format.print_string ("ERROR while compiling view:\n" ^ s); Format.print_newline ();
       raise e) 
   
(* backpatch horribleness *)
let _ = Library.compile_view_impl := compile_view
