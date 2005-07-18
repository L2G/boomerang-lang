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
  | S of Schema.t               (* schemas *)
  | V of V.t                    (* trees *)
  | F of Syntax.sort * (t -> t) (* functions *)

let sort_of_t = function
   N _    -> Syntax.SName    
 | L _    -> Syntax.SLens
 | S _    -> Syntax.SSchema
 | V _    -> Syntax.STree
 | F(s,_) -> s

(* pretty print *)
let rec string_of_t t =   
  match string_of_t_aux [] t with
      _,[],s -> s
    | _,ds,s -> s
(* 	Printf.sprintf "%s\nwhere\n\t%s"
	  s
	  (Misc.concat_list "\n\t" ds)
*)
and string_of_t_aux printed = function
    N(n)   -> [],[],n
  | S(s)   -> [],[],Schema.string_of_t s
  | V(v)   -> [],[],(V.string_of_t v)
  | L(l)   -> [],[],"<lens>"
  | F(s,_) -> [],[],Printf.sprintf "<%s fun>" (Syntax.string_of_sort s)


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
		 (fun () -> Format.printf "%s: syntax error." 
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
		 (fun () -> Format.printf "%s: syntax error." 
		    (Info.string_of_t (Lexer.info lexbuf))))
    in
      Lexer.finish ();
      st

(* errors *)

let focal_type_error i es v = 
    raise (Error.Harmony_error
	     (fun () -> Format.printf "%s: run-time sort error; expected %s, found %s in %s."
		(Info.string_of_t i)
		(Syntax.string_of_sort es)
		(Syntax.string_of_sort (sort_of_t v))
		(string_of_t v)))
      
(* [schema_of_tree v] yields the singleton [Value.ty] containing [v] *)
let rec schema_of_tree v =
  let i = Info.M "schema coerced from view" in
    Schema.mk_cat 
      i
      (V.fold  
         (fun k vk ts -> (Schema.mk_atom i k (schema_of_tree vk))::ts)
         v
         [])

let get_schema i v = 
  match v with 
      S s -> s 
    | V v -> schema_of_tree v
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
      L l -> l
    | _ -> focal_type_error i Syntax.SLens v

let mk_schema_fun return_sort msg f = 
  F(Syntax.SArrow(Syntax.SSchema,return_sort),
    fun v -> f (get_schema (Info.M msg) v))
let mk_sfun s = mk_schema_fun (parse_sort s)
  
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
(*     | STree,SType                     -> (fun V vi -> T (type_of_tree vi)  *)
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
(* 			  | V vi -> f (T (type_of_tree vi)) *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | STree          -> F(function V _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)
(*   | SArrow(s1,s2)  -> F(function F _ as v -> f v *)
(* 			  | _ -> focal_type_error s msg) *)


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
	L (Lens.native error (fun _ -> error))
  | Syntax.SSchema -> S (Schema.mk_any (Info.M "dummy type"))
  | Syntax.STree   -> V (V.empty)
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
    | S _ -> v
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
	
