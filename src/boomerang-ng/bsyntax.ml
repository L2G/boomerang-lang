(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.ml - Focal abstract syntax                            *)
(****************************************************************)
(* $Id: syntax.ml 2049 2006-07-31 16:35:19Z jnfoster $ *)

(* imports *)
let ( @ ) = Safelist.append 
let sprintf = Printf.sprintf

(* parsing info *)
type i = Info.t

(* identifiers and qualified identifiers *)
type id = i * string
type qid = id list * id

let mk_prelude_qid s = 
  let i = Info.M (sprintf "%s built-in" s) in 
  ([i,"Prelude"],(i,s))

let info_of_id (i,_) = i
let string_of_id (_,x) = x
let id_compare (_,x1) (_,x2) = compare x1 x2
let id_equal i1 i2 = (id_compare i1 i2 = 0)
let qid_of_id id = [],id
let qid_dot (qs1,x1) (qs2,x2) = (qs1@(x1::qs2),x2)
let qid_compare (qs1,x1) (qs2,x2) = 
  let rec ids_compare xs1 xs2 = match xs1,xs2 with
    | [],[] -> 0
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

let qid_qualifiers (qs,_) = qs

let string_of_qid (qs,i) = 
  Printf.sprintf "%s%s"
    (Safelist.fold_left 
       (fun acc qi -> Printf.sprintf "%s%s." acc (string_of_id qi)) 
       ""
       qs)    
    (string_of_id i)

(* sorts, parameters, expressions *)
type sort = 
    | SString                  (* strings *)
    | SRegexp                  (* regular expressions *)
    | SLens                    (* lenses *)
    | SFunction of sort * sort (* funtions *)

and param = Param of i * id * sort

and binding = Bind of i * id * sort option * exp

and exp = 
    (* lambda calculus *)
    | EApp of i  * exp * exp
    | EVar of i * qid
    | EFun of i * param * sort option * exp 
    | ELet of i * binding * exp

    (* regular operations *)
    | EString of i * Bstring.t
    | ECSet of i * bool * (Bstring.sym * Bstring.sym) list 
    | EUnion of i * exp * exp
    | ECat of i * exp * exp 
    | EStar of i * exp
    | ECompose of i * exp * exp
    | EDiff of i * exp * exp
    | EInter of i * exp * exp

   (* boomerang expressions *)
    | EMatch of i * Bstring.t * qid
    | ETrans of i * exp * exp

(* declarations *)
type test_result =
    | TestValue of exp
    | TestError
    | TestShow

type decl = 
    | DLet of i * binding  
    | DMod of i * id * decl list 
    | DTest of i * exp * test_result

(* modules *)
type modl = Mod of i * id * qid list * decl list

let (^>) s1 s2 = SFunction(s1,s2)

let info_of_exp = function    
  | EApp (i,_,_) -> i
  | EVar (i,_) -> i
  | EFun (i,_,_,_) -> i
  | ELet (i,_,_) -> i
  | EString (i,_) -> i
  | ECSet (i,_,_) -> i
  | EUnion (i,_,_) -> i
  | ECat (i,_,_) -> i
  | ETrans (i,_,_) -> i
  | EStar (i,_) -> i
  | ECompose (i,_,_) -> i
  | EDiff (i,_,_) -> i
  | EInter (i,_,_) -> i
  | EMatch (i,_,_) -> i
      
let info_of_module = function
  | Mod(i,_,_,_) -> i

let id_of_module = function
  | Mod(_,x,_,_) -> x

let sort_of_param = function
  | Param(_,_,s) -> s

let id_of_param = function
  | Param(_,x,_) -> x

(* string_of_sort : s -> string
 *
 * [string_of_sort s] produces a string representing [s] 
 *)
let rec string_of_sort = function
  | SString -> "string"
  | SRegexp -> "regexp"
  | SLens -> "lens"
  | SFunction(s1,s2) -> 
      sprintf "(%s -> %s)" (string_of_sort s1) (string_of_sort s2)

let string_of_param p = string_of_id (id_of_param p)

