(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.mli - interface for Focal abstract syntax             *)
(****************************************************************)
(* $Id$ *)

(** {2 Identifiers, qualified identifiers, annotated with parsing information } **)
type id 
type qid 

val mk_id : Info.t -> string -> id
val mk_qid : id list -> id -> qid
val get_qualifiers : qid -> id list

val id_compare : id -> id -> int
val id_equal : id -> id -> bool
val qid_compare : qid -> qid -> int
val qid_equal : qid -> qid -> bool
val qid_prefix : qid -> qid -> bool  

val qid_hash : qid -> int 

val qid_of_id : id -> qid
val id_of_string : Info.t -> string -> id
val dot : qid -> qid -> qid

(** {2 Datatypes for Focal abstract syntax } **)
type sort = 
    SName of Info.t
  | SLens of Info.t
  | SType of Info.t 
  | SView of Info.t  
  | SArrow of Info.t * sort * sort
  | STOper of Info.t * sort * sort

type param = PDef of Info.t * id * sort

type exp = 
    EVar of Info.t * qid
  | EFun of Info.t * param list * sort option * exp 
  | EMap of Info.t * (Info.t * exp * exp) list
  | EApp of Info.t * exp * exp
  | ELet of Info.t * binding list * exp
  | EName of Info.t * id
  | EType of Info.t * typeexp
  | EView of Info.t * (Info.t * exp * exp) list 
  | EConsView of Info.t * exp * exp
and typeexp = TT of ptypeexp | NT of ptypeexp 
and ptypeexp = 
    TEmpty of Info.t
  | TAny of Info.t
  | TVar of Info.t * qid
  | TApp of Info.t * ptypeexp * ptypeexp
  | TFun of Info.t * id list * sort * ptypeexp (* only used internally *)
  | TName of Info.t * exp * ptypeexp 
  | TBang of Info.t * exp list * ptypeexp
  | TStar of Info.t * exp list * ptypeexp
  | TCat of Info.t * ptypeexp list 
  | TUnion of Info.t * ptypeexp list
and binding = BDef of Info.t * id * param list * sort option * exp

type typebinding = (id * id list * typeexp)

type decl = 
    DLet of Info.t * binding list 
  | DType of Info.t * typebinding list 
  | DMod of Info.t * id * decl list 
  | DTestGet of Info.t * exp * exp * exp option 
  | DTestPut of Info.t * exp * (exp * exp option) * exp option
      
type modl = MDef of Info.t * id * qid list * decl list

(** {3 Utility functions } **)

val emptyView : Info.t -> exp
val emptyViewType : Info.t -> ptypeexp
val name_of_id : id -> string
val id_of_binding : binding -> id
val id_of_typebinding : typebinding -> id
val id_of_modl : modl -> id
val id_of_param : param -> id
val sort_of_param : param -> sort

(** {3 Info getters } **)

val info_of_list : ('a -> Info.t) -> Info.t -> 'a list -> Info.t
val info_of_id : id -> Info.t
val info_of_qid : qid -> Info.t
val info_of_exp : exp -> Info.t
val info_of_ptypeexp : ptypeexp -> Info.t
val info_of_typeexp : typeexp -> Info.t
val info_of_sort : sort -> Info.t
val info_of_binding : binding -> Info.t
val info_of_typebinding : typebinding -> Info.t
val info_of_bindings : Info.t -> binding list -> Info.t
val info_of_typebindings : Info.t -> typebinding list -> Info.t
val info_of_module : modl -> Info.t

(** {3 Pretty printers } **)

val string_of_id : id -> string
val string_of_qid : qid -> string
val string_of_sort : sort -> string
val string_of_param : param -> string
val string_of_exp : exp -> string
val string_of_typeexp : typeexp -> string
val string_of_ptypeexp : ptypeexp -> string
val string_of_binding : binding -> string
val string_of_bindings : binding list -> string
val string_of_typebinding : typebinding -> string
val string_of_typebindings : typebinding list -> string
val string_of_decl : decl -> string
val string_of_module : modl -> string
