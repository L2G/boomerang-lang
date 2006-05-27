(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(********************************************************************)
(* $Id$ *)

type l_chk = Schema.t -> Schema.t
type lens_checker =     
    BIJ of l_chk * l_chk (* we need both directions for bijective lenses to check invert *)
  | VWB of l_chk
  | WB of l_chk

type t = 
    N of Name.t                           (* names *)
  | L of (V.t, V.t) Lens.t * lens_checker (* lenses *)
  | S of Schema.t                         (* schemas *)
  | V of V.t                              (* trees *)
  | F of Syntax.sort * (t -> t)           (* functions *)
  | D of Syntax.sort * Syntax.qid         (* dummies *)

let sort_of_t = function
   N _    -> Syntax.SName    
 | L _    -> Syntax.SLens
 | S _    -> Syntax.SSchema
 | V _    -> Syntax.STree
 | F(s,_) -> s
 | D(s,_) -> s

(* pretty print *)
let rec format_t = function
    N(n)   -> Format.printf "@[%s]" (Misc.whack n)
  | S(s)   -> Schema.format_t s
  | V(v)   -> V.format_t v
  | L(l,c)   -> Format.printf "@[%s]" "<lens>"
  | F(s,_) -> 
      Format.printf "@[%s" "<";
      Syntax.format_sort s;
      Format.printf " fun%s]" ">"
  | D(s,q) -> 
      Format.printf "@[%s" "<";
      Syntax.format_sort s;
      Format.printf " dummy for %s%s]" 
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
		 (fun () -> Format.printf "%s: syntax error in qualfied identifier." 
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
		 (fun () -> Format.printf "%s: syntax error in sort." 
		    (Info.string_of_t (Lexer.info lexbuf))))
    in
      Lexer.finish ();
      st

(* errors *)
let focal_type_error i es v = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "%s: run-time sort error"
              (Info.string_of_t i)))
            
let get_schema i v = 
  match v with 
      S s -> s 
    | V v -> Schema.t_of_tree v
    | _ -> focal_type_error i Syntax.SSchema v
	
let get_name i v = 
  match v with 
      N n -> n 
    | _ -> focal_type_error i Syntax.SName v

let get_tree i v = 
  match v with 
      V vi -> vi 
    | _ -> focal_type_error i Syntax.STree v
	
let get_lens i v = 
  match v with
      L(l,c) -> (l,c)
    | _ -> focal_type_error i (Syntax.SLens) v

let mk_schema_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SSchema,return_sort),
    fun v -> f (get_schema (Info.M msg) v))
let mk_sfun s = mk_schema_fun (parse_sort s)

let mk_checker = mk_sfun "schema"
  
let mk_name_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SName,return_sort),
    fun v -> f (get_name (Info.M msg) v))
let mk_nfun s = mk_name_fun (parse_sort s)

let mk_tree_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.STree,return_sort),
    fun v -> f (get_tree (Info.M msg) v))
let mk_vfun s = mk_tree_fun (parse_sort s)

let mk_lens_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SLens,return_sort),
    fun v -> f (get_lens (Info.M msg) v))
let mk_lfun s = mk_lens_fun (parse_sort s)

let mk_fun_fun arg_sort return_sort msg f =
  F(Syntax.SArrow(arg_sort,return_sort),
    function F (_,ff) -> f ff
      | v         -> focal_type_error (Info.M msg) arg_sort v)
let mk_ffun a s = mk_fun_fun (parse_sort a) (parse_sort s)

let is_dummy = function D _ -> true | _ -> false

(* dummy value generator *)
let rec dummy s q = D(s,q) 

(* MEMOIZATION *)
type thist = t (* HACK! *)
module H =
  Hashtbl.Make(
    struct
      type t = thist
      let equal = (==)                                (* Use physical equality test *)
      let hash o = Hashtbl.hash (Obj.magic o : int)   (* Hash on physical addr *)
    end)
    
let memoize v = match v with 
  | F(s,f) -> F (s, 
		 let memotable = H.create 1 in
		   (fun x -> try
		      H.find memotable x
		    with Not_found -> begin 
		      let fx = f x in
			H.add memotable x fx;
			fx
		    end)) 
  | _ -> v
