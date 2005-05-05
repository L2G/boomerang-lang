(** Abstract Syntax for the interpreter/debugger of Focal **)
open Error

(* After lexing and parsing *)
type expr = 
  | AstVar of string * info
  | AstApp of expr * expr * info
  | AstView of ((expr * expr) list) * string * bool * info
  | AstMap of ((expr * expr) list) * info
  | AstFun of string*expr*info
  | AstLet of (string* (string list)*expr*expr * info)
  | AstLetrec of ((string* (string list)*expr*info) list * expr * info)
  | AstName of string * info

type defn = 
    Deflet of string * (string list) * expr * info
  | Defletrec of (string * (string list) * expr * info) list

type prog = (defn list) * expr

let emptyView i = AstView([],"",false,i)

(* Final abstract syntax ?? *)
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

(* type lensdef = Name.t * (Name.t list) * lens *)
type lensdef = Name.t * arg

type lensprog = lensdef list * arg

(* Coercions *)
let lensOfArg = function
  | L l -> l
  | _ -> raise (Run_error ("Coercion to lens failed",bogusInfo))

let funOfArg = function 
  | F f -> f
  | _ -> raise (Run_error ("Coercion to fun failed",bogusInfo))

let schemaOfArg = function 
  | Sc s -> s
  | _ -> raise (Run_error ("Coercion to schema failed",bogusInfo))

let nameOfArg = function 
  | N n -> n
  | _ -> raise (Run_error ("Coercion to name failed",bogusInfo))

let predOfArg = function 
  | P p -> p
  | _ -> raise (Run_error ("Coercion to predicate failed",bogusInfo))

let viewOfArg = function 
  | V v -> v
  | _ -> raise (Run_error ("Coercion to view failed",bogusInfo))

let getInfo = function
  | AstVar(_,i)     -> i
  | AstApp(_,_,i)   -> i
  | AstView (_,_,_,i) -> i
  | AstMap (_,i)    -> i
  | AstName(_,i)     -> i
  | AstLet(_,_,_,_,i)     -> i
  | AstLetrec(_,_,i) -> i
  | AstFun(_,_,i)     -> i

let setInfo info = function
  | AstVar(x,_)       -> AstVar(x,info)
  | AstApp(x,y,_)     -> AstApp(x,y,info)
  | AstView (x,y,z,_) -> AstView (x,y,z,info)
  | AstMap (x,_)      -> AstMap (x,info) 
  | AstName(x,_)      -> AstName(x,info) 
  | AstLet(x,y,z,a,_) -> AstLet(x,y,z,a,info)
  | AstLetrec(x,y,_)  -> AstLetrec(x,y,info)
  | AstFun(x,y,_)     -> AstFun(x,y,info) 

(** Types used by Focal **)
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

type typedef =
 (string * typeschema) list

(* FEATURE TREE TYPES *)
(* Feature Tree Types - abstract syntax *)

(* common definitions *)
type name = string
type state = string

(* PRE-TYPES (i.e., parsed type expressions) *)
type ptyp = 
  | PTEmpty of info  
  | PTEmptyView of info
  | PTVar of state * info
  | PTAny of bool * (name list) * ptyp * info
  | PTAll of bool * (name list) * ptyp * info
  | PTName of bool * name * ptyp * info
  | PTCat of ptyp * ptyp * info
  | PTUnion of ptyp * ptyp * info
  | PTDiff of ptyp * ptyp * info
  | PTInter of ptyp * ptyp * info
      
type pdef = (state * info * ptyp)
type pctx = pdef list

(* TYPES *)

(* compound type states: 
 *   state * intersection list * difference list *)
type cstate = state * (state list) * (state list)

(* types *)
type typ =
    TAny of (name list) * cstate * info
  | TAll of (name list) * cstate * info
  | TName of name * cstate * info
  | TCat of typ list * info
  | TUnion of typ list * info
  | TEmpty of info
  | TEmptyView of info
       
type def = (state * info * typ) 
type ctx = def list

(* UTILITY FUNCTIONS *)
let t2info = function
    TEmpty(i)     -> i
  | TEmptyView(i) -> i
  | TName(_,_,i)  -> i
  | TAny(_,_,i)   -> i
  | TAll(_,_,i)   -> i
  | TCat(_,i)     -> i
  | TUnion(_,i)   -> i

let lookup_opt x env = 
  let f (y,_,t) tt = if (x=y) then Some t else tt in
    List.fold_right f env None
      
let lookup x env = 
  match (lookup_opt x env) with
      None -> raise (Error.Type_error ("missing definition for " ^ x, Error.bogusInfo))
    | Some t -> t

let add x i t env = env@[(x,i,t)]

let cs2s (x,_,_) = x
