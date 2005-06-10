(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.ml - internal run-time representation of Focal structures  *)
(*                                                                  *)
(********************************************************************)
(* $Id$ *)
  
type t = 
    N of Name.t                 (* names *)
  | L of (V.t, V.t) Lens.t      (* lenses *)      
  | T of ty                     (* types *)
  | V of V.t                    (* views *)
  | F of Syntax.sort * (t -> t)               (* functions *)
and ty = 
    Empty of Info.t
  | Any of Info.t
  | Var of Info.t * Syntax.qid * thunk
  | App of Info.t * t * t * thunk
  | Atom of Info.t * string * ty
  | Star of Info.t * (string list) * ty
  | Bang of Info.t * (string list) * ty
  | Cat of Info.t * ty list 
  | Union of Info.t * ty list 
and thunk = unit -> t

let sort_of_t = function
   N _    -> Syntax.SName    
 | L _    -> Syntax.SLens
 | T _    -> Syntax.SType
 | V _    -> Syntax.SView
 | F(s,_) -> s

(* pretty print *)
let rec string_of_t = function
    N(n) -> "N " ^ n
  | T(t) -> "T " ^ (string_of_ty t)
  | V(v) -> "V " ^ (V.string_of_t v)
  | L(l) -> "L <lens>"
  | F(s,_) -> Printf.sprintf "F <%s fun>" (Syntax.string_of_sort s)
      
and string_of_ty = function
    Empty(_) -> "Empty"
  | Any(_) -> "Any"
  | Var (_,x,_)    -> Syntax.string_of_qid x
  | App(_,t1,t2,_) -> Printf.sprintf "<delayed application %s %s>" (string_of_t t1) (string_of_t t2)
  | Atom(_,n,t) -> Printf.sprintf "%s = %s" n (string_of_ty t)
  | Bang(_,f,t)  -> 
      Printf.sprintf "!%s = %s"
	(if f = [] then "" else "\\" ^ Misc.parens (Misc.concat_list ", " f))
	(string_of_ty t)  
  | Star(_,f,t)  ->
      Printf.sprintf "*%s = %s"
	(if f = [] then "" else "\\" ^ Misc.parens (Misc.concat_list ", " f))
	(string_of_ty t)
  | Cat(_,ts)   -> Misc.curlybraces (Misc.concat_list ", " (Safelist.map string_of_ty ts))
  | Union(_,ts)  -> Misc.parens (Misc.concat_list " | " (Safelist.map string_of_ty ts))

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
	raise (Error.Compile_error((Lexer.info lexbuf), 
				 Lexer.filename (), 
				 "syntax error"))
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
	raise (Error.Compile_error((Lexer.info lexbuf), 
				 Lexer.filename (), 
				 "syntax error"))	  
    in
      Lexer.finish ();
      st

(* errors *)

let focal_type_error i es v = 
  raise (Error.Fatal_error 
	   (Printf.sprintf "Run-time sort error at %s: expected %s, found %s in %s"
	      (Info.string_of_t i)
	      (Syntax.string_of_sort es)
	      (Syntax.string_of_sort (sort_of_t v))
	      (string_of_t v)))

(* [type_of_view v] yields the singleton [Value.ty] containing [v] *)
let rec type_of_view v =
  V.fold 
    (fun k vk t -> 
       match t with 
	   Cat(i,ts) -> Cat(i,Atom(i,k,(type_of_view vk))::ts)
	 | _         ->
	     raise (Error.Fatal_error("in type_of_view: reached an ill-formed type")))
    v
    (Cat(Info.M "coerced type",[]))


(* FIXME: say what we found if it's not expected *)	   
let get_type i v = 
  match v with 
      T t -> t 
    | V v -> type_of_view v
    | _ -> focal_type_error i Syntax.SType v
	
let get_name i v = 
  match v with 
      N n -> n 
    | _ -> focal_type_error i Syntax.SName v

let get_view i v = 
  match v with 
      V vi -> vi 
    | _ -> focal_type_error i Syntax.SView v
	
let get_lens i v = 
  match v with
      L l -> l
    | _ -> focal_type_error i Syntax.SLens v

let mk_type_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SType,return_sort),
    fun v -> f (get_type (Info.M msg) v))
let mk_tfun s = mk_type_fun (parse_sort s)
  
let mk_name_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SName,return_sort),
    fun v -> f (get_name (Info.M msg) v))
let mk_nfun s = mk_name_fun (parse_sort s)

let mk_view_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SView,return_sort),
    fun v -> f (get_view (Info.M msg) v))
let mk_vfun s = mk_view_fun (parse_sort s)

let mk_lens_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SLens,return_sort),
    fun v -> f (get_lens (Info.M msg) v))
let mk_lfun s = mk_lens_fun (parse_sort s)

let mk_fun_fun arg_sort return_sort msg f =
  F(Syntax.SArrow(arg_sort,return_sort),
    function F (_,ff) -> f ff
      | v         -> focal_type_error (Info.M msg) arg_sort v)
let mk_ffun a s = mk_fun_fun (parse_sort a) (parse_sort s)

(* (\* coerce: precondition is s1 <: s2 *\) *)
(* let coerce s1 s2 =  *)
(*   let err s1 s2 =  *)
(*     focal_type_error  *)
(*       s1  *)
(*       (Printf.sprintf "in coercion from %s to %s"  *)
(* 	 (Syntax.string_of_sort s1)  *)
(* 	 (Syntax.string_of_sort s2))  *)
(*   in match s1,s2 with *)
(*       _ when s1 = s2                  -> (fun v -> x) *)
(*     | SView,SType                     -> (fun V vi -> T (type_of_view vi)  *)
(* 					  | _ -> err s1 s2) *)
(*     | SArrow(s11,s12),SArrow(s21,s22) -> (fun F f -> *)
(* 					    let c f' = fun x -> (coerce s12 s22) (f' ((coerce s22 s11) x)) in *)
(* 					      F (c f) *)
(* 					  | _ -> err s1 s2) *)
(*     | _ -> err s1 s2 *)	
(* let rec lift_fun s f msg = match s1 with *)
(*     SName          -> F(function N _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | SLens          -> F(function L _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | SType          -> F(function T _ as v -> f v *)
(* 			  | V vi -> f (T (type_of_view vi)) *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | SView          -> F(function V _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | SArrow(s1,s2)  -> F(function F _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)

let info_of_ty = function
    Var(i,_,_)   -> i
  | Any(i)       -> i
  | Empty(i)     -> i
  | App(i,_,_,_) -> i
  | Atom(i,_,_)  -> i
  | Star(i,_,_)  -> i
  | Bang(i,_,_)  -> i
  | Cat(i,_)     -> i
  | Union(i,_)   -> i      

(* dummy value generator *)
let rec dummy ?(msg="") s = match s with 
    Syntax.SName -> N "_"
  | Syntax.SLens -> 
      let error _ = 
	flush stdout; flush stderr;
	prerr_string (Printf.sprintf "Fatal error: dummy %s was not overwritten.\n" msg);
	flush stderr; 
	assert false
      in
	L (Lens.native error error)
  | Syntax.SType -> T (Empty(Info.M "dummy type"))
  | Syntax.SView -> V (V.empty)
  | Syntax.SArrow(_,rs) -> F (s, fun _ -> dummy ~msg:msg rs)

(* MEMOIZATION *)
type thist = t (* HACK! *)
module H =
  Hashtbl.Make(
    struct
      type t = thist
      let equal = (==)                                (* Use physical equality test *)
      let hash o = Hashtbl.hash (Obj.magic o : int)   (* Hash on physical addr *)
    end)
    
let memoize v =
  match v with 
      N _ -> v
    | T _ -> v
    | V _ -> v
    | L l -> L (Lens.memoize_lens l) 
    | F(s,f) -> F (s, 
		   let memotable = H.create 1 in
		     (fun x -> try
			H.find memotable x
		      with Not_found -> begin 
			let fx = f x in
			  H.add memotable x fx;
			  fx
		      end))
	
