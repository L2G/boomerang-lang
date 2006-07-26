(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(********************************************************************)
(* $Id$ *)

open Syntax

(*type l_chk = Schema.t -> Schema.t*)
type ('a, 'b) lens_checker =     
  | BIJ of ('a -> 'b) * ('b -> 'a) (* we need both directions for bijective lenses to check invert *)
  | VWB of ('a -> 'b)
  | WB of ('a -> 'b)

type t = 
  | N of Name.t                                                (* names *)
  | L of (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker (* lenses *)
  | S of Schema.t                                              (* schemas *)
  | P of Db.Relation.Pred.t                                    (* relational predicates *)
  | FD of Dbschema.Relschema.Fd.Set.t                          (* functional dependencies *)
  | V of V.t                                                   (* trees *)
  | M of ((V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker) Name.Map.t (* fmaps *)
  | F of Syntax.sort * (t -> t)                                (* functions *)
  | D of Syntax.sort * Syntax.qid                              (* dummies *)

let equal v w =
  match v, w with
  | N n1, N n2 -> n1 = n2
  | S s1, S s2 -> Schema.equivalent s1 s2
  | V v1, V v2 -> V.equal v1 v2
  | M m1, M m2 -> raise (Error.Harmony_error (fun () ->
      Util.format "Finite map values cannot be compared for equality."))
  | L _, L _ -> raise (Error.Harmony_error (fun () ->
      Util.format "Lens values cannot be compared for equality."))
  | F _, F _ -> raise (Error.Harmony_error (fun () ->
      Util.format "Function values cannot be compared for equality."))
  | D _, D _ -> raise (Error.Harmony_error (fun () ->
      Util.format "Dummy values cannot be compared for equality."))
  | _, _ -> false

let sort_of_t = function
   N _    -> Syntax.SName    
 | L _    -> Syntax.SLens
 | S _    -> Syntax.SSchema
 | P _    -> Syntax.SPred
 | FD _   -> Syntax.SFD
 | V _    -> Syntax.SView
 | M _    -> Syntax.SMap
 | F(s,_) -> s
 | D(s,_) -> s

(* pretty print *)
let rec format_t = function
    N(n)   -> Util.format "@[%s]" (Misc.whack n)
  | S(s)   -> Schema.format_t s
  | P(pred) -> Db.Relation.Pred.format_t pred
  | FD(fds) -> Dbschema.Relschema.Fd.Set.format_t fds
  | V(v)   -> V.format_t v
  | L(l,c) -> Util.format "@[%s]" "<lens>"
  | M(_)   -> Util.format "@[%s]" "<fmap>"
  | F(s,_) -> 
      Util.format "@[%s" "<";
      Syntax.format_sort s;
      Util.format " fun%s]" ">"
  | D(s,q) -> 
      Util.format "@[%s" "<";
      Syntax.format_sort s;
      Util.format " dummy for %s%s]" 
        (Syntax.string_of_qid q) ">"

(*random helpers *)
(* utility functions for parsing *)      
(* FIXME: these could live somewhere else, but dependencies make it
   difficult *)
let parse_qid s =     
  let lexbuf = Lexing.from_string s in
    Lexer.setup "qid constant";
    let q = 
      try 
        Parser.qid Lexer.main lexbuf 
      with Parsing.Parse_error -> 
        raise (Error.Harmony_error
                 (fun () -> Util.format "%s: syntax error in qualfied identifier." 
                    (Info.string_of_t (Lexer.info lexbuf))))
    in 
      Lexer.finish ();
      q

let parse_sort s =
  let lexbuf = Lexing.from_string s in
    Lexer.setup "sort constant";
    let (i,st) = 
      try 
        Parser.sort Lexer.main lexbuf 
      with Parsing.Parse_error -> 
        raise (Error.Harmony_error
                 (fun () -> Util.format "%s: syntax error in sort." 
                    (Info.string_of_t (Lexer.info lexbuf))))
    in
      Lexer.finish ();
      st

(* errors *)
let focal_type_error i es v = 
  raise (Error.Harmony_error
           (fun () ->
             Util.format "%s: run-time sort error:@ " (Info.string_of_t i);
             format_t v;
             Util.format "@ does not have sort@ ";
             Syntax.format_sort es
           ))

let get_schema i v = 
  match v with 
      S s -> s 
    | V (V.Tree t) -> Schema.treeschema (Treeschema.t_of_tree t)
    | _ -> focal_type_error i Syntax.SSchema v

let get_name i v = 
  match v with 
      N n -> n 
    | _ -> focal_type_error i Syntax.SName v

let get_v i v = 
  match v with 
      V v -> v 
    | _ -> focal_type_error i Syntax.SView v

let get_tree i v = 
  match v with 
      V (V.Tree t) -> t 
    | V (V.Db _) ->
        raise (Error.Harmony_error
           (fun () ->
             Util.format "%s: run-time error:@ " (Info.string_of_t i);
             Util.format "expected a tree but found@ ";
             format_t v
           ))
    | _ -> focal_type_error i Syntax.SView v

let get_lens i v = 
  match v with
      L(l,c) -> (l,c)
    | _ -> focal_type_error i (Syntax.SLens) v

let get_pred i v =
  match v with
  | P p -> p
  | _ -> focal_type_error i (Syntax.SPred) v

let get_fds i v =
  match v with
  | FD fds -> fds
  | _ -> focal_type_error i (Syntax.SFD) v

let get_fmap i v = 
  match v with 
      M(fm) -> fm
    | _ -> focal_type_error i Syntax.SMap v

let mk_sfun return_sort msg f = 
  F (SSchema ^> return_sort, fun v -> f (get_schema (Info.M msg) v))

let mk_checker = mk_sfun SSchema

let mk_nfun return_sort msg f = 
  F (SName ^> return_sort, fun v -> f (get_name (Info.M msg) v))

let mk_vfun return_sort msg f = 
  F (SView ^> return_sort, fun v -> f (get_v (Info.M msg) v))

let mk_lfun return_sort msg f = 
  F (SLens ^> return_sort, fun v -> f (get_lens (Info.M msg) v))

let mk_pfun return_sort msg f =
  F (SPred ^> return_sort, fun v -> f (get_pred (Info.M msg) v))

let mk_fdfun return_sort msg f =
  F (SPred ^> return_sort, fun v -> f (get_fds (Info.M msg) v))

let mk_ffun arg_sort return_sort msg f =
  F (arg_sort ^> return_sort,
      function F (_,ff) -> f ff
             | v        -> focal_type_error (Info.M msg) arg_sort v)

let coerce_lens_checker (desc : string) (lc : ('a, 'b) lens_checker) f f' g g'
: ('c, 'd) lens_checker =
  let i = Info.M desc in
  match lc with
  | BIJ (ck1, ck2) ->
      BIJ ((fun s -> g i (ck1 (f' i s))), (fun s -> f i (ck2 (g' i s))))
  | VWB ck -> VWB (fun s -> g i (ck (f' i s)))
  | WB ck -> WB (fun s -> g i (ck (f' i s)))

let tree i = Schema.treeschema
let db i = Schema.dbschema

let v_of_tree_lens (l, lc) =
  Lens.v_of_tree l,
  coerce_lens_checker "v_of_tree_lens"
    lc tree Schema.treeschema_of tree Schema.treeschema_of

let v_of_db_lens (l, lc) =
  Lens.v_of_db l,
  coerce_lens_checker "v_of_db_lens"
    lc db Schema.dbschema_of db Schema.dbschema_of

let tree_of_v_lens (l, lc) =
  Lens.tree_of_v l,
  coerce_lens_checker "tree_of_v_lens"
    lc Schema.treeschema_of tree Schema.treeschema_of tree

let db_of_v_lens (l, lc) =
  Lens.db_of_v l,
  coerce_lens_checker "db_of_v_lens"
    lc Schema.dbschema_of db Schema.dbschema_of db

let mk_fmfun return_sort msg f = 
  F (SMap ^> return_sort, fun m -> f (get_fmap (Info.M msg) m))

let is_dummy = function D _ -> true | _ -> false

(* dummy value generator *)
let rec dummy s q = D(s,q) 

(* MEMOIZATION *)

let memoize = function
    F(s,f) -> 
      let module M = Memo.Make(
	struct 
	  type arg = t
	  type res = t
	  let format_arg = format_t 
	  let format_res = format_t
	  let hash = function
	    | V(V.Tree(t))   -> Tree.hash t
	    | S(Schema.T(s)) -> Treeschema.hash s
	    | o              -> Hashtbl.hash o
	  let equal = (==)
	  let name = "Value.memoize"
	  let init_size = 1
	  let f = f
	end) in 
	F(s,M.memoized)
  | v -> v
