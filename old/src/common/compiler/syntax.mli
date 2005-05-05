type expr = 
  | AstVar of string * Error.info
  | AstApp of expr * expr * Error.info
  | AstView of ((expr * expr) list) * string * bool * Error.info
  | AstMap of ((expr * expr) list) * Error.info
  | AstFun of string*expr*Error.info
  | AstLet of (string* (string list)*expr*expr * Error.info)
  | AstLetrec of ((string* (string list)*expr*Error.info) list * expr * Error.info)
  | AstName of string * Error.info

type defn = 
    Deflet of string * (string list) * expr * Error.info
  | Defletrec of (string * (string list) * expr * Error.info) list

val emptyView : Error.info -> expr

type prog = (defn list) * expr
type arg =
    V of V.t 
  | N of Name.t 
  | P of Prd.t
  | L of Lens.t
  | M of (Name.t -> Lens.t)
  | F of (arg -> arg)
  | A of (arg*arg)
  | X of string
  | Sc of (V.t ->bool)

type lensdef
type lensprog

val lensOfArg : arg -> Lens.t
val funOfArg : arg -> (arg -> arg)
val schemaOfArg : arg -> (V.t -> bool)
val nameOfArg : arg -> Name.t
val predOfArg : arg -> Prd.t
val viewOfArg : arg -> V.t

val getInfo : expr -> Error.info
val setInfo : Error.info -> expr -> expr

type exprtype =
    Vari of string
  | Lens
  | Arrow of exprtype * exprtype
  | Predicate
  | View
  | Maparg
  | Name
  | Schema
  | Undef of string

type typeschema =
    T of exprtype
  | For_all of string*typeschema

type typedef = (string * typeschema) list

(* FEATURE TREE TYPES *)
(* common definitions *)
type name = string
type state = string

(* PRE-TYPES (i.e., parsed type expressions) *)
type ptyp = 
    PTEmpty of Error.info
  | PTEmptyView of Error.info
  | PTVar of state * Error.info
  | PTAny of bool * (name list) * ptyp * Error.info
  | PTAll of bool * (name list) * ptyp * Error.info
  | PTName of bool * name * ptyp * Error.info
  | PTCat of ptyp * ptyp * Error.info
  | PTUnion of ptyp * ptyp * Error.info
  | PTDiff of ptyp * ptyp * Error.info
  | PTInter of ptyp * ptyp * Error.info
      
type pdef = (state * Error.info * ptyp)
type pctx = pdef list

(* INTERNAL TYPES *)

(* compound type states: 
 *   state * intersection list * difference list
 *)
type cstate = state * (state list) * (state list)

(* types *)
type typ =
    TAny of (name list) * cstate * Error.info
  | TAll of (name list) * cstate * Error.info
  | TName of name * cstate * Error.info
  | TCat of typ list * Error.info
  | TUnion of typ list * Error.info
  | TEmpty of Error.info
  | TEmptyView of Error.info
      
type def = (state * Error.info * typ) 
type ctx = def list

(* UTILITY FUNCTIONS *)
val t2info : typ -> Error.info
val cs2s : cstate -> state
val lookup_opt : string -> (string * Error.info *'a) list -> 'a option
val lookup : string -> (string * Error.info *'a) list -> 'a
val add : string -> Error.info -> 'a -> (string * Error.info * 'a) list -> (string * Error.info * 'a) list
