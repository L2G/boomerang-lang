(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.mli - interface for Focal abstract syntax             *)
(****************************************************************)
(* $Id$ *)

(** Focal abstract syntax

    (See [parser.mly] for concrete syntax.)
*)

(** {2 Identifiers, qualified identifiers, annotated with parsing information } *)
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


(** constants *)
val native_prelude : Info.t -> id list
val compose2_qid : Info.t -> qid 
val get_qid : Info.t -> qid 
val create_qid : Info.t -> qid
val put_qid : Info.t -> qid 
val sync_qid : Info.t -> qid
val cons_qid : Info.t -> qid
val nil_qid : Info.t -> qid
val type_of_tree_qid : Info.t -> qid

(** {2 Datatypes for Focal abstract syntax } *)
type sort = 
    SName 
  | SLens 
  | SSchema  
  | STree   
  | SArrow of sort * sort

type param = PDef of Info.t * id * sort

type exp = 
    EApp of Info.t * exp * exp
  | EAtom of Info.t * exp * exp 
  | EBang of Info.t * exp list * exp
  | ECat of Info.t * exp list 
  | ECons of Info.t * exp * exp 
  | EFun of Info.t * param list * sort option * exp 
  | ELet of Info.t * binding list * exp
  | EMap of Info.t * (exp * exp) list
  | EName of Info.t * id
  | ENil of Info.t 
  | EStar of Info.t * exp list * exp
  | EUnion of Info.t * exp list
  | EVar of Info.t * qid

and binding = BDef of Info.t * id * param list * sort option * exp

type decl = 
    DLet of Info.t * binding list 
  | DMod of Info.t * id * decl list 
  | DTest of Info.t * exp * exp option 
      
type modl = MDef of Info.t * id * qid list * decl list

(** {3 Utility functions } *)

val name_of_id : id -> string
val id_of_binding : binding -> id
val id_of_modl : modl -> id
val id_of_param : param -> id
val sort_of_param : param -> sort

(** {3 Info getters } *)

val info_of_list : ('a -> Info.t) -> Info.t -> 'a list -> Info.t
val info_of_id : id -> Info.t
val info_of_qid : qid -> Info.t
val info_of_exp : exp -> Info.t
val info_of_binding : binding -> Info.t
val info_of_bindings : Info.t -> binding list -> Info.t
val info_of_module : modl -> Info.t

(** {3 Pretty printers } *)

val string_of_id : id -> string
val string_of_qid : qid -> string
val string_of_sort : sort -> string
val string_of_param : param -> string
val string_of_exp : exp -> string
val string_of_binding : binding -> string
val string_of_bindings : binding list -> string
val string_of_decl : decl -> string
val string_of_module : modl -> string
