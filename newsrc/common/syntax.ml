(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.ml - Focal abstract syntax                            *)
(*                                                              *)
(* $Id: syntax.ml,v 1.2 2005/04/11 18:24:47 jnfoster Exp $     *)
(*                                                              *)
(****************************************************************)

(* identifiers *)
type i = Info.t
type id = i * string
type qid = id list * id

(* abstract syntax *)

(* sorts *)
type sort = 
    SName of i
  | SLens of i
  | SType of i 
  | SView of i  
  | SArrow of i * sort * sort

(* parameters *)
type param = PDef of i * id * sort

(* expressions *)
type exp = 
    EVar of i * qid
  | EFun of i * param list * sort option * exp 
  | EMap of i * (id * exp) list
  | EApp of i * exp * exp
  | ELet of i * binding list * exp
  | EName of i * id
  | EType of i * typeexp
  | EView of i * viewbind list

(* types *)
and typeexp = 
    TEmpty of i
  | TName of i * exp * typeexp 
  | TBang of i * except_list * typeexp
  | TStar of i * except_list * typeexp
  | TCat of i * typeexp list 
  | TUnion of i * typeexp list
  | TDiff of i * typeexp * typeexp
  | TInter of i * typeexp * typeexp
  | TExp of i * exp 
      
and except_list = id list

and viewbind = (i * exp * exp)

(* bindings *)
and binding = BDef of i * id * param list * sort option * exp

type typebinding = (id * id list * typeexp)

(* declarations *)
type decl = 
    DLet of i * binding list 
  | DType of i * typebinding list 
  | DMod of i * id * decl list  (* inner modules can't open name spaces, for now *)
      
(* modules *)
type modl = MDef of i * id * qid list * decl list

(* simple constants *)
let emptyView i = EView(i,[])
let emptyViewType i = TStar(i,[],TEmpty i)

(* utility functions *)
let qid_of_id id = [],id
let qid_of_string i s = qid_of_id (i,s)
  
(* accessor functions *)
let get_info_id = function (i,_) -> i
let get_info_qid = function (_,id) -> get_info_id id
let name_of_id = snd

(* read off info fields *)
let info_of_exp = function
    ELet(i,_,_) -> i
  | EFun(i,_,_,_) -> i
  | EApp(i,_,_) -> i
  | EVar(i,_) -> i
  | EName(i,_) -> i
  | EMap(i,_) -> i
  | EView(i,_) -> i
  | EType(i,_) -> i

let info_of_typeexp = function
    TEmpty(i) -> i
  | TExp(i,_) -> i
  | TStar(i,_,_) -> i
  | TBang(i,_,_) -> i
  | TInter(i,_,_) -> i
  | TDiff(i,_,_) -> i
  | TName(i,_,_) -> i
  | TCat(i,_) -> i
  | TUnion(i,_t) -> i

(* read off pieces of parameters *)
let id_of_param = function PDef(_,x,_) -> x
let sort_of_param = function PDef(_,_,s) -> s

(* convert a view, type to a list view and list type *)
let list_view ve = ve (* FIXME: actually do this *)
let list_type vt = vt



