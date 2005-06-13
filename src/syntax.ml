(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.ml - Focal abstract syntax                            *)
(****************************************************************)
(* $Id$ *)

let ( @ ) = Safelist.append  (* redefine @ to use stack-safe append *)
let sprintf = Printf.sprintf (* import selectively *)

(* identifiers *)
type i = Info.t
type id = i * string
type qid = id list * id

let get_qualifiers (is,_) = is

let mk_id i s = (i,s)
let mk_qid is i = (is,i)

(* functions on identifiers *)
(* equality: ignore parsing Info.t *)
let id_compare (_,x1) (_,x2) = compare x1 x2
let id_equal i1 i2 = (id_compare i1 i2 = 0)
let qid_compare (qs1,x1) (qs2,x2) = 
  let rec ids_compare xs1 xs2 = match xs1,xs2 with
      [],[] -> 0
    | _,[]  -> 1
    | [],_  -> -1
    | (x1::t1),(x2::t2) -> 
	let hd_compare = id_compare x1 x2 in
	  if (hd_compare <> 0) then hd_compare 
	  else ids_compare t1 t2
  in
    ids_compare (qs1@[x1]) (qs2@[x2])
let qid_equal q1 q2 = (qid_compare q1 q2 = 0)
	
let qid_prefix q1 q2 = 
  let (is1,i1) = q1 in
  let (is2,i2) = q2 in 
  let il1 = is1 @ [i1] in
  let il2 = is2 @ [i2] in
    ((Safelist.length il1) <= (Safelist.length il2)) 
    && (Safelist.for_all 
	  (fun (i1,i2) -> id_equal i1 i2)
	  (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))
	
let string_of_id (_,i) = i
let string_of_qid (q,i) = Misc.concat_list "." (Safelist.map string_of_id q@[string_of_id i])

let qid_hash q = Hashtbl.hash (string_of_qid q)

(* utility functions *)
let qid_of_id id = [],id
let id_of_string i s = (i,s)
let dot (qs1,x1) (qs2,x2) = (qs1@(x1::qs2),x2)

(* constants *)
let native_prelude i = Safelist.map (mk_id i) ["Native"; "Prelude"]
let mk_pre_qid i s = mk_qid (native_prelude i) (mk_id i s)
let compose2_qid i = mk_pre_qid i "compose2"
let get_qid i = mk_pre_qid i "get"
let create_qid i = mk_pre_qid i "create"
let put_qid i = mk_pre_qid i "put"
let sync_qid i = mk_pre_qid i "_sync" (* can't write "sync" because it's a token :( *)
let cons_qid i = mk_pre_qid i "Cons"
let nil_qid i = mk_pre_qid i "Nil"
let type_of_view_qid i = mk_pre_qid i "type_of_view"

(* abstract syntax *)

(* sorts *)
type sort = 
    SName 
  | SLens  
  | SSchema   
  | STree    
  | SArrow of sort * sort
      
(* parameters *)
type param = PDef of i * id * sort

(* expressions *)
type exp = 
    EApp of i  * exp * exp
  | EAtom of i * exp * exp 
  | EBang of i * exp list * exp
  | ECat of i * exp list 
  | ECons of i * exp * exp 
  | EFun of i * param list * sort option * exp 
  | ELet of i * binding list * exp
  | EMap of i * (exp * exp) list
  | EName of i * id
  | ENil of i
  | EStar of i * exp list * exp
  | EUnion of i * exp list
  | EVar of i * qid
      
(* bindings *)
and binding = BDef of i * id * param list * sort option * exp

(* declarations *)
type decl = 
    DLet of i * binding list 
  | DMod of i * id * decl list 
  | DTest of i * exp * exp option 
      
(* modules *)
type modl = MDef of i * id * qid list * decl list
  
(* accessor functions *)
let name_of_id (_,x) = x
let id_of_binding (BDef(_,x,_,_,_)) = x

(* read off info fields *)
let info_of_list (e2i:'a -> Info.t) (i:Info.t) (l: 'a list) : Info.t = Safelist.fold_left (fun i ei -> Info.merge_inc (e2i ei) i) i l
let info_of_id = function (i,_) -> i
let info_of_qid = function (_,id) -> info_of_id id
let info_of_exp = function
  | EApp(i,_,_)   -> i
  | EAtom(i,_,_)  -> i
  | EBang(i,_,_)  -> i
  | ECat(i,_)     -> i
  | ECons(i,_,_)  -> i
  | EFun(i,_,_,_) -> i
  | ELet(i,_,_)   -> i
  | EName(i,_)    -> i
  | ENil(i)       -> i
  | EMap(i,_)     -> i
  | EStar(i,_,_)  -> i
  | EUnion(i,_t)  -> i
  | EVar(i,_)     -> i

let info_of_binding (BDef(i,_,_,_,_)) = i
let info_of_bindings (i:Info.t) (bs:binding list) : Info.t = info_of_list info_of_binding i bs

(* read off pieces of parameters *)
let id_of_param = function PDef(_,x,_) -> x
let sort_of_param = function PDef(_,_,s) -> s

(* pretty printing stuff *)
(* sorts *)
let rec string_of_sort = function
    SName         -> "name"
  | SLens         -> "lens"
  | SSchema         -> "schema"
  | STree         -> "tree"
  | SArrow(s1,s2) -> 
      sprintf "(%s -> %s)" 
	(string_of_sort s1) 
	(string_of_sort s2)

(* params *)
let string_of_param (PDef(_,i,s)) = 
  Misc.parens(sprintf "%s:%s" 
		(string_of_id i) 
		(string_of_sort s))

(* expressions *)
let rec string_of_exp = function 
  | EApp(_,e1,e2)    -> 
      Misc.parens (sprintf "%s %s" 
		     (string_of_exp e1) 
		     (string_of_exp e2))
  | EAtom(_,n,e)     -> 
      sprintf "%s=%s" 
	(string_of_exp n) 
	(string_of_exp e)
  | EBang(_,f,e)  ->
      sprintf "!%s = %s"
    	(if f = [] then "" 
	 else 
	   Misc.parens
	     (Misc.concat_list " " 
		(Safelist.map string_of_exp f)))
	(string_of_exp e)
  | ECat(_,es) -> 
      Misc.curlybraces
	(Misc.concat_list "," 
	   (Safelist.map string_of_exp es))
  | ECons(_,e1,e2) ->
      sprintf "%s::%s"
	(string_of_exp e1)
	(string_of_exp e2)
  | EFun(_,ps,so,e) -> 
      sprintf "fun %s%s -> %s"
	(Misc.concat_list " " (Safelist.map string_of_param ps))
	(match so with 
	     None -> "" 
	   | Some s -> sprintf " : %s" (string_of_sort s))
	(string_of_exp e)
  | EMap(_,ms) -> 
      Misc.curlybraces 
	(Misc.concat_list ", " 
	   (Safelist.map (fun (e1,e2) -> 
			    sprintf "%s -> %s"
			      (string_of_exp e1)
			      (string_of_exp e2))
	      ms))
  | ELet(_,bs,e) ->
      sprintf "let %s in %s" 
	(string_of_bindings bs)
	(string_of_exp e)
  | EName(_,n) -> string_of_id n
  | ENil(_) -> Misc.brackets ""
  | EStar(_,f,e)  ->
      sprintf "!%s = %s"
    	(if f = [] then "" 
	 else 
	   Misc.parens
	     (Misc.concat_list " " 
		(Safelist.map string_of_exp f)))
	(string_of_exp e)
  | EUnion(_,es) ->
      Misc.parens 
	(Misc.concat_list " | " 
	   (Safelist.map string_of_exp es))
  | EVar(_,x) -> string_of_qid x

and string_of_binding (BDef(_,x,ps,so,e)) = 
  Misc.concat_list ""
    [string_of_id x
    ; Misc.concat_list " " (" " :: (Safelist.map string_of_param ps))
    ; (match so with 
	 | None -> ""
	 | Some s -> (" : " ^ string_of_sort s))
    ; " = "
    ; string_of_exp e]
and string_of_bindings bs = Misc.concat_list " and " (Safelist.map string_of_binding bs)

and string_of_decl = function
  | DLet(i,bs) -> "let " ^ (string_of_bindings bs)			
  | DMod(_,i,ds) -> Misc.concat_list "" 
      (["module "
       ; string_of_id i
       ; " =\n"]
       @ (Safelist.map (fun di -> (string_of_decl di) ^ "\n") ds))
  | DTest(_,e1,e2o) -> Misc.concat_list " "
      (["test"
       ; string_of_exp e1
       ; "="
       ; match e2o with None -> "error" | Some e2 -> string_of_exp e2
       ])

let id_of_modl (MDef(_,m,_,_)) = m
let info_of_module (MDef(i,_,_,_)) = i

let string_of_module (MDef(_,id,qs,ds)) = 
  Misc.concat_list ""
    (["module "
     ; string_of_id id
     ; " =\n"
     ; (match qs with
	  | [] -> ""
	  | _  -> ("\nopen " ^ (Misc.concat_list "\n" (Safelist.map string_of_qid qs)) ^ "\nin"))
     ] @ (Safelist.map (fun di -> (string_of_decl di) ^ "\n") ds))

