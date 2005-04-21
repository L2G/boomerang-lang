(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - the actual compiler                            *)
(*                                                              *)
(* $Id: compiler.ml,v 1.4 2005/04/21 03:27:42 jnfoster Exp $    *)
(*                                                              *)
(****************************************************************)

(* We assume that the checker has already been run:
   - annotating functions and bindings with sorts, 
   - flattening TName, TStar, and TBangs
   - rewriting multi-param functions to lambdas 
   - rewriting let-defs to simple no-param binders
*)

                   (* opens *)   (* mapping from qns to values *)  
type compile_env = Value.qn list * Value.env
let empty = ([], Value.empty)
let get_ev cev = let (_,ev) = cev in ev
let put_ev cev ev = let (os,_) = cev in (os,ev)
					  
let lookup cev q : (Value.rtv option) =
  let (os,ev) = cev in
  let rec lookup_aux os q2 =     
    match Value.lookup ev q2 with
      |	None ->
	  begin match os with
	    | []       -> None
	    | o::orest -> lookup_aux orest (Value.dot (Some o) q) (* N.B., use q, not q2 here *)		  
	  end
      | Some r -> Some r
  in
    lookup_aux os q
let lookup_qid cev qx = lookup cev (Value.qn_of_qid qx)
let lookup_id cev x = lookup cev ([], Value.n_of_id x)  

let update cev q r = put_ev cev (Value.update (get_ev cev) q r)
let update_id cev x r = update cev (Value.qn_of_id x) r
let overwrite cev q r = put_ev cev (Value.overwrite(get_ev cev) q r)
let overwrite_id cev x r = overwrite cev (Value.qn_of_id x) r

let opens cev qs = let (os,ev) = cev in (qs@os,ev)

(* expressions *)
(* compile_exp :: compile_env -> Syntax.exp -> Value.rtv *)
let rec compile_exp cev e0 : Value.rtv = match e0 with
  | Syntax.EVar(i,q) -> 
      begin
        match (lookup_qid cev q) with
	  | Some r -> r
	  | None -> raise (Error.Run_error(("Unbound variable " 
					    ^ (Pretty.string_of_qid q)
					    ^ "."), i))
      end
	
  | Syntax.EFun(i,xs,so,e) ->
      begin
	match xs,so with 
	  | [p], Some s  -> 
	      let ps = Syntax.sort_of_param p in
	      let f (v:Value.t) : Value.t =  
		let fcev = update_id cev (Syntax.id_of_param p) (ps,v) in
		  Value.t_of_rtv (compile_exp fcev e)
	      in
	      let fs = Syntax.SArrow(i,ps,s) in
		(fs, Value.F(f))
	  | _     -> raise (Error.Run_error(("Bogus function"), i))
      end
	
  | Syntax.EApp(i,e1,e2) ->
      begin
	match Value.t_of_rtv (compile_exp cev e1) with 	  
	    Value.F(f) -> 
	      let r = compile_exp cev e2 in
		(Value.sort_of_rtv r, f (Value.t_of_rtv r))
	  | _   -> raise (Error.Run_error("Left-hand side of application is not a function.", i))
      end

  | Syntax.ELet(i,bs,e) -> compile_exp (compile_bindings cev None bs) e
      
  | Syntax.EName(i,id) -> (Syntax.SName(i), Value.N (Syntax.name_of_id id))

  | Syntax.EType(i,t) -> (Syntax.SType(i), Value.T (compile_typeexp cev t))

  | Syntax.EView(i,ks) -> (Syntax.SView(i), Value.V (compile_viewbinds cev ks))
      
(* types *)
(* compile_exp :: compile_env -> Syntax.typeexp -> Type.t *)
and compile_typeexp cev = 
  let 
      type2cstate = function 
	| Syntax.TExp(_,Syntax.EVar(i,q)) -> (Value.qn_of_qid q,[],[])
	| t -> raise (Error.Run_error(("Unflattened type "
				 ^ Pretty.string_of_typeexp t),
				Syntax.info_of_typeexp t))
  in function
    | Syntax.TEmpty(i)       -> Type.Empty
	
    | Syntax.TName(i,e,t)    ->
	let n =
	  match Value.t_of_rtv (compile_exp cev e) with
	      Value.N n -> n
	    | _     -> raise (Error.Run_error("Name expected.", i))
	in
	  Type.Name(n, type2cstate t)
	    
    | Syntax.TBang(i,xl,t)   -> 
	Type.Bang(List.map Syntax.name_of_id xl, type2cstate t)
	  
    | Syntax.TStar(i,xl,t)   -> 
	Type.Star(List.map Syntax.name_of_id xl, type2cstate t)

    | Syntax.TCat(i,ts)      -> 
	Type.Cat(List.map(compile_typeexp cev) ts) 
	  
    | Syntax.TUnion(i,ts)    -> 
	Type.Union(List.map(compile_typeexp cev) ts) 
	  
    | Syntax.TDiff(i,t1,t2)  -> 
	raise(Error.Run_error(("Diff not implemented."), i))
	  
    | Syntax.TInter(i,t1,t2) -> 
	raise(Error.Run_error(("Inter not implemented."), i))
	  
    | Syntax.TExp(i,e) ->
	begin
	  match Value.t_of_rtv (compile_exp cev e) with
	    | Value.T t -> t
	    | _     -> raise (Error.Run_error("Type expected.",i))
	end
	  
(* views *)
(* compile_viewbinds :: env -> (Info.t * Syntax.exp * Syntax.exp) list -> V.t *)
and compile_viewbinds ev = function
  | [] -> V.empty
      
  | (i,ne,ve)::rest ->
      let n =
	match Value.t_of_rtv (compile_exp ev ne) with
	  | Value.N n -> n
	  | _ -> raise (Error.Run_error("Expected a name.", i))
      in
      let v = match Value.t_of_rtv (compile_exp ev ve) with
	| Value.V v -> v
	| _ -> raise (Error.Run_error("Expected a view.", i))
      in
      let vrest = compile_viewbinds ev rest in	
	match (V.get vrest n) with
	    (* simple sanity check *)
	  | Some _ -> raise (Error.Run_error("Repeated name in view.", i))
	  | None   -> V.set vrest n (Some v)
	      
(* bindings *)
and compile_bindings cev mo bs = 
  (* add all the bindings in a recursion group to the environment *)
  let bev : compile_env = 
    List.fold_left 
      (fun bev (Syntax.BDef(_,f,_,_,_)) -> 
	 update bev (Value.dot_id mo f) Value.dummy_rtv) 
      cev 
      bs 
  in
  (* and backpatch... *)
  let _ = List.map
    (fun bi -> match bi with
       | (Syntax.BDef(i,f,[],so,e)) ->
	   let r = compile_exp bev e in
	   let s = Value.sort_of_rtv r in
	   let v = Value.t_of_rtv r in	     
	     overwrite bev (Value.dot_id mo f) (s, Value.memoize v)
       | (Syntax.BDef(i,f,_,so,e)) -> assert false)
    bs
  in    
    bev
      
(* type bindings *)
let compile_typebindings cev mo ts =   
  (* add to environment and backpatch *)
  let tev = 
    List.fold_left 
      (fun tev (x,_,_) -> update_id tev (Value.dot_id mo x) Value.dummy_rtv) 
      cev 
      ts 
  in
    List.fold_left 
      (fun cev ti -> 
	 match ti with
	   | (x,[],t) -> 
	       overwrite_id 
		 tev 
		 (Value.dot_id mo x) 
		 (Syntax.SType(Info.bogus),Value.T(compile_typeexp tev t))
	   | (x,_,t)  -> assert false)
      cev
      ts
      
(* declarations *)
let rec compile_decl ev mo = function
  | Syntax.DLet(_,bs)   -> compile_bindings ev mo bs
  | Syntax.DType(_,ts)  -> compile_typebindings ev mo ts
  | Syntax.DMod(i,n,ds) -> compile_module_aux ev n ds

and compile_module_aux cev m ds =
  let qnm = Value.qn_of_id m in
    List.fold_left (fun ev di -> compile_decl (opens cev [qnm]) (Some qnm) di) cev ds
      
(* modules *)
let compile_module = function
  | Syntax.MDef(i,m,nctx,ds) ->
      let init_ev = 
	opens 
	  (put_ev empty (Value.get_library ()))
	  (Value.qn_of_id m::(List.map Value.qn_of_qid nctx)) 
      in
	compile_module_aux 
	  init_ev
	  m 
	  ds
	
	
(**** OLD STUFF ****)
(* (\* backpatch definitions of tracepoint, id from the library *\) *)

(* (\** recursive functions are automatically tracepointed below **\) *)
(* let tracepoint_def:(arg option ref) = ref None *)
(* let tracepoint n l = *)
(*   let td =  *)
(*     match (!tracepoint_def) with *)
(*       | None ->  *)
(* 	  let (_,e) = lookup_required "tracepoint" in *)
(* 	    (tracepoint_def := Some e); e *)
(*       | Some e -> e in *)
(*   let f  = (funOfArg td) in *)
(*   let f' = (funOfArg (f (N n))) in *)
(*     lensOfArg (f' (L l)) *)
      
(* let id_def:(arg option ref) = ref None *)
(* let id () =  *)
(*   lensOfArg( *)
(*     match (!id_def) with  *)
(*       | None ->  *)
(* 	  let (_,e) = lookup_required "id" in *)
(* 	    (id_def := Some e); e *)
(*       | Some e   -> e) *)


(* (\* Library of base functions *\) *)
(* let primlib () = List.map (function (s,_,arg) -> (s,arg)) (get_lens_library ()) *)

(* (\*****************\) *)
(* (\* Main compiler *\) *)
(* (\*****************\) *)
(* let compileexpr =  *)
(* (\* Conversions of the views, predicates and bijections *\) *)
(*   let rec ast2view b viewenv varenv= function *)
(*       [] ->  *)
(* 	if b then *)
(* 	  V.empty_list *)
(* 	else *)
(* 	  V.empty *)
(*     | (ast1,ast2)::q ->  *)
(* 	match compile viewenv varenv ast2 with *)
(* 	    V v -> *)
(* 	      if b  *)
(*  	      then V.cons v (ast2view b viewenv varenv q) *)
(* 		(\* V.set (V.set (V.empty) "*h" (Some  v)) "*t" (Some (ast2view b viewenv varenv q)) *\) *)
(* 	      else let s = *)
(* 		match compile viewenv varenv ast1 with *)
(* 		    N n -> n *)
(* 		  | _ -> raise (Run_error ("You don't use a correct Name",getInfo ast1)) *)
(* 	      in *)
(* 	      V.set (ast2view b viewenv varenv q) s (Some v) *)
(* 	  | _ -> raise (Run_error ("You don't use a correct View",getInfo ast2)) *)
	      
(*   and ast2pred viewenv varenv l n = match l with *)
(*       [] -> false *)
(*     | (ast,_)::q ->  *)
(* 	let s = *)
(* 	  match compile viewenv varenv ast with *)
(* 	      N n1 -> n1 *)
(* 	    | _ -> raise (Run_error ("You don't use a correct Name",getInfo ast)) *)
(* 	in n = s || (ast2pred viewenv varenv q n) *)
		 
(*   (\* takes an env for views and variables and a prog and returns a lens in a unsable abstract syntax *\)	  *)
(*   and compile viewenv varenv= function *)
(*       AstVar (x,i) ->  *)
(* 	if List.mem_assoc x (varenv) then List.assoc x (varenv) *)
(* 	else raise (Run_error ("Unbound variable "^x,i)) *)
(*     | AstApp (e1,e2,i) ->  *)
(* 	begin *)
(* 	  match compile viewenv varenv e1 with *)
(* 	      F f -> f (compile viewenv varenv e2) *)
(* 	    | _ -> raise (Run_error ("You don't apply a function",i)) *)
(* 	end *)
(*     | AstView (l,s,b,_) ->  *)
(* 	if List.mem_assoc s viewenv then *)
(* 	  begin *)
(* 	    match (List.assoc s viewenv) with *)
(* 		T View -> V (ast2view b viewenv varenv l) *)
(* 	      | T Predicate -> P (ast2pred viewenv varenv l) *)
(* 	      | T (Undef _) -> V (ast2view b viewenv varenv l) *)
(* 	      | _ -> V (V.empty) *)
(* 	  end *)
(* 	else *)
(* 	  V (ast2view b viewenv varenv l) *)
(*     | AstMap (l,i) ->  *)
(* 	let lmap = List.map  *)
(* 		     (function (AstName(s,_),exp) ->  *)
(* 			(s,compile viewenv varenv exp) *)
(* 			| _ -> raise (Run_error ("The tree is not a name!",i))) l in *)
(* 	  M (function s -> if List.mem_assoc s lmap then lensOfArg (List.assoc s lmap) else id ()) *)
(*     | AstFun (s,e,_) -> F (function x -> compile viewenv ((s,x)::varenv) e) *)
(*     | AstName (s, _) -> N s *)
(*     | AstLetrec(ldef,ex,_) -> *)
(* 	let temp = Hashtbl.create 1 in *)
(* 	let collect = List.map (function (f,_,_,_) -> *)
(* 				  (f, L (tracepoint f (Lens.named ~hashtable:temp f)))) in *)
(* 	let newenv = (collect ldef) in *)
(* 	  List.iter (function (f,_,e,_) ->  *)
(* 		       Hashtbl.add temp f (Lens.memoize (lensOfArg (compile viewenv (newenv@varenv) e)))) ldef; *)
(* 	  compile viewenv (newenv@varenv) ex *)
(*     | AstLet(x,_,ex1,ex2,_) ->  *)
(* 	let ex1' = compile viewenv varenv ex1 in *)
(* 	  compile viewenv ((x,ex1')::varenv) ex2 *)
(*   in *)
(*     compile *)

(* (\* Compile a definition *\) *)
(* let compiledef viewenv varenv= function *)
(*     Deflet (f,_,e,_) -> [f,compileexpr viewenv varenv e]@varenv *)
(*   | Defletrec (ldef) ->  *)
(*       let temp = Hashtbl.create 1 in *)
(*       let collect = List.map (function (f,_,_,_)->(f,L (tracepoint f (Lens.named ~hashtable:temp f)))) in *)
(*       let newenv = (collect ldef)@varenv in *)
(*       List.iter (function (f,_,e,_) ->  *)
(* 		   Hashtbl.add temp f (Lens.memoize (lensOfArg (compileexpr viewenv newenv e)))) ldef; *)
(*       newenv *)

(* (\* Compile a hole program using the output of the typechecker *\) *)
(* let compileprog (viewenv,(deflist,expr)) = *)
(*   let rec compildeflist varenv = function *)
(*       [] -> varenv *)
(*     | d::q -> compildeflist (compiledef viewenv varenv d) q in *)
(*   let finalenv = compildeflist (primlib ()) deflist in *)
(*   match (compileexpr viewenv finalenv expr) with *)
(*       L l -> l *)
(*     | _ -> raise (Run_error ("The tree is not a lens!",bogusInfo)) *)

(* (\* Complete compilation *\)     *)
(* let compile_focal s = *)
(*   try  *)
(*     Lexer.lineno := 0 ; *)
(*     Lexer.linestart := 1; *)
(*     let lexbuf = Lexing.from_string s in *)
(*     let ast    = Parser.prog Lexer.token lexbuf in *)
(*     let (viewenv,(deflist,expr)),_  = Checker.nicetypesoftheprog ast in  *)
(*     let rec compildeflist varenv = function *)
(* 	[] -> varenv *)
(*       | d::q -> compildeflist (compiledef viewenv varenv d) q in *)
(*     let finalenv = compildeflist (primlib ()) deflist in *)
(*       compileexpr viewenv finalenv expr *)
(*   with e -> *)
(*     (Format.print_string ("ERROR while compiling:\n" ^ s); Format.print_newline (); *)
(*      raise e) *)
    
(* (\* Helper function for compiling views from strings in FOCAL syntax *\) *)
(* let compile_view_lexbuf lexbuf =  *)
(*     let astv = Parser.view Lexer.token lexbuf in *)
(*     let astv_annot = Checker.annot_view astv in *)
(*       match (compileexpr [] [] astv_annot) with *)
(* 	  V v -> v *)
(* 	| _   -> raise (Run_error ("The tree is not a view!", bogusInfo))     *)

(* let compile_view s =  *)
(*   Lexer.lineno := 0 ; *)
(*   Lexer.linestart := 1; *)
(*   let lexbuf = Lexing.from_string s in *)
(*     try *)
(*       compile_view_lexbuf lexbuf	 *)
(*     with e -> *)
(*       (Format.print_string ("ERROR while compiling view:\n" ^ s); Format.print_newline (); *)
(*        raise e)  *)
   
(* backpatch horribleness *)

(* let _ = Library.compile_view_impl := compile_view *)
