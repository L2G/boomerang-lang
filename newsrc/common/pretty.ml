(***********************************************************)
(* The Harmony Project                                     *)
(* harmony@lists.seas.upenn.edu                            *)
(*                                                         *)
(* pretty.ml - pretty printing functions                   *)
(*                                                         *)
(* $Id: pretty.ml,v 1.4 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                         *)
(***********************************************************)

(* debugging *)
let debug_flag = true
let debug s = if debug_flag then (prerr_string (s ^ "\n"))

(* generic pretty-printing helper functions *)
let concat sep list = 
  if (list = []) 
  then "" 
  else
    List.fold_right 
      (fun h t -> if (t = sep) then h else (h ^ sep ^ t))
      list
      sep
      
let enclose p1 s p2 = p1 ^ s ^ p2
let braces s = enclose "(" s ")"
let brackets s = enclose "[" s "]" 
let curlybraces s = enclose "{" s "}" 


(* abstract syntax *)
(* info *)
let string_of_info ((l1,c1),(l2,c2)) = 
  Printf.sprintf "line %d, char %d, to line %d, char %d " l1 c1 l2 c2
    
(* sorts *)
let rec string_of_sort = function
    Syntax.SName(_)        -> "name"
  | Syntax.SLens(_)        -> "lens"
  | Syntax.SType(_)        -> "type"
  | Syntax.SView(_)        -> "view"
  | Syntax.SArrow(_,s1,s2) -> braces (string_of_sort s1 
				      ^ "->" 
				      ^ string_of_sort s2)
      
(* parsed syntax *)
(* identifiers *)
let string_of_id (_,i) = i
let string_of_qid (q,i) = concat "." (List.map string_of_id q@[string_of_id i])

(* params *)
let string_of_param (Syntax.PDef(_,i,s)) = braces(string_of_id i ^ ":" ^ string_of_sort s)

(* expressions *)
let rec string_of_exp = function 
    Syntax.EVar(_,q)       -> string_of_qid q
  | Syntax.EFun(_,ps,so,e) -> ("fun " 
			       ^ (concat " " (List.map string_of_param ps))
			       ^ (match so with 
				    | None -> "" 
				    | Some s -> " : " ^ string_of_sort s)
			       ^ " -> "
			       ^ (string_of_exp e))
  | Syntax.EApp(_,e1,e2)    -> (string_of_exp e1) ^ " " ^ (string_of_exp e2)
  | Syntax.ELet(i,bs,e)     -> ("let " 
				^ (string_of_bindings bs)
				^ " in "
				^ (string_of_exp e))
  | Syntax.EName(_,i)       -> string_of_id i
  | Syntax.EType(_,t)       -> string_of_typeexp t
  | Syntax.EView(_,vbs)     -> curlybraces (concat ", " 
					      (List.map (fun (_,n,v) -> 
							   (string_of_exp n) 
							   ^ "=" 
							   ^ (string_of_exp v))
						 vbs))

and string_of_typeexp = function 
  | Syntax.TEmpty(_)     -> "empty"
  | Syntax.TName(_,n,t) -> concat "" [string_of_exp n; curlybraces (string_of_typeexp t)]
  | Syntax.TBang(_,f,t)  ->
      concat ""
    	[ "!"
     	; braces (concat " " (List.map string_of_id f))
    	; curlybraces (string_of_typeexp t)]
  | Syntax.TStar(_,f,t)  ->
      begin
	match f,t with
	    [], Syntax.TEmpty(_) -> "{}"
	  | _             ->
	      concat ""
    		[ "*"
    		; braces (concat " " (List.map string_of_id f))
    		; curlybraces (string_of_typeexp t)]
      end
  | Syntax.TCat(_,cs) -> concat "." (List.map string_of_typeexp cs)
  | Syntax.TUnion(_,ts) -> braces (concat " | " (List.map string_of_typeexp ts))
  | Syntax.TDiff(_,t1,t2) -> concat " - " (List.map string_of_typeexp [t1;t2])
  | Syntax.TInter(_,t1,t2) -> concat " & " (List.map string_of_typeexp [t1;t2])
  | Syntax.TExp(_,e)     -> string_of_exp e

and string_of_binding (Syntax.BDef(_,x,ps,so,e)) = 
  concat ""
    [string_of_id x
    ; concat " " (" " :: (List.map string_of_param ps))
    ; (match so with 
	 | None -> ""
	 | Some s -> (" : " ^ string_of_sort s))
    ; " = "
    ; string_of_exp e]
and string_of_bindings bs = concat " and " (List.map string_of_binding bs)

and string_of_typebinding (x,xs,t) = 
  concat ""
    [ string_of_id x
    ; concat ", " (List.map string_of_id xs)
    ; string_of_typeexp t]

and string_of_typebindings ts = concat " and " (List.map string_of_typebinding ts)

and string_of_decl = function
  | Syntax.DLet(i,bs) -> "let " ^ (string_of_bindings bs)			
  | Syntax.DType(i,ts) -> "type " ^ (string_of_typebindings ts)
  | Syntax.DMod(_,i,ds) -> concat "" 
      (["module "
       ; string_of_id i
       ; " =\nn"]
       @ (List.map (fun di -> (string_of_decl di) ^ "\n") ds))

let string_of_modl (Syntax.MDef(_,id,qs,ds)) = 
  concat ""
    (["module "
     ; string_of_id id
     ; " =\n"
     ; (match qs with
	  | [] -> ""
	  | _  -> ("\nopen " ^ (concat "\n" (List.map string_of_qid qs)) ^ "\nin"))
     ] @ (List.map (fun di -> (string_of_decl di) ^ "\n") ds))

(* names / qualified names *)
let string_of_n n = n
let string_of_qn (qs,n) = concat "." (qs@[n])

(* types *)
let string_of_cstate (x,is,ds) = 
  concat ""
    (string_of_qn x::
       if (is = [] && ds = []) then []
       else
	 [ "&"; curlybraces (concat " " (List.map string_of_qn is))
	 ; "-"; curlybraces (concat " " (List.map string_of_qn ds))
	 ])
let rec string_of_type = function
  | Type.Empty -> "empty"
  | Type.Name(n,cx) -> concat "" [string_of_n n; curlybraces (string_of_cstate cx)]
  | Type.Bang(f,cx)  ->
      concat ""
    	[ "!"
    	; braces (concat " " f)
    	; curlybraces (string_of_cstate cx)]
  | Type.Star(f,cx)  ->
	concat ""
    	  [ "*"
    	  ; braces (concat " " f)
    	  ; curlybraces (string_of_cstate cx)]
  | Type.Cat(cs)   -> concat "." (List.map string_of_type cs)
  | Type.Union(ts)  -> braces (concat " | " (List.map string_of_type ts))

(* values *)
let rec string_of_value = function
    Value.N(n) -> n
  | Value.L(l) -> "<lens>"
  | Value.T(t) -> string_of_type t
  | Value.V(t) -> "<view>"      
  | Value.F(f) -> "<fun>"

(* random helpers *)
and string_of_env ev = 
  curlybraces 
    (Value.fold 
       (fun q r acc -> 
	  let (s,v) = !r in
	    (concat "" [ string_of_qn q
		       ; "->"
		       ; string_of_value v
		       ; ":"
		       ; string_of_sort s
		       ; if (acc = "") then "" else ", "
		       ; acc]))
       ev
       "")
