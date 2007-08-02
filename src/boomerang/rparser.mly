%{ 
(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* pparser.mly : parser                                      *)
(*************************************************************)
(* $Id$ *)

let sprintf = Printf.sprintf

(* ----- module and type imports and abbreviations ----- *)
module RS = Rstring
module L = Rlenses 
module V = Rvalue
module Renv = Rvalue.Renv
type 'a env = 'a Renv.t

let sprintf = Printf.sprintf
let (@) = Safelist.append

(* ----- helper functions for extracting / merging Info.t ------ *)
(* i: V.t -> Info.t 
 * 
 * [i v] returns the parsing info associated to [v] 
 *)
let i = V.info_of_t 

(* m: Info.t -> Info.t -> Info.t
 * 
 * [m i1 i2] merges the parsing info [i1] and [i2] into a single
 * parsing info representing the range spanned by [i1] and [i2].
 *)
let m = Info.merge_inc 

(* ----- helper functions for parsing cset ----- *)
(* parse_cset: string -> FS.elt list *)
let parse_cset s = 
   let err () = raise (Parsing.Parse_error) in 
   let i = ref 0 in
   let l = String.length s in
   let eos () = !i = l in
   let test c = not (eos ()) && s.[!i] = c in
   let accept c = let r = test c in if r then incr i; r in
   let get () = 
     let do_get () = let r = s.[!i] in incr i; r in 
       if accept '\\' then 
           match do_get () with
           | '^' -> (RS.of_char '^')
           | '-' -> (RS.of_char '-')
           | 'b' -> (RS.of_char '\008')
           | 'n' -> (RS.of_char '\010')
           | 'r' -> (RS.of_char '\013')
           | 't' -> (RS.of_char '\009')
           | '\\' -> (RS.of_char '\\')
           | _   -> err () 
       else RS.of_char (do_get ()) in 
   let next () = if eos () then err () else get () in 
   let rec go acc = 
     if eos () then Safelist.rev acc
     else 
       let acc' = 
         if accept '-' then err ()
         else
           let c1 = next () in 
             if accept '-' then 
               (c1,next ())::acc 
             else (c1,c1)::acc in 
       go acc' in 
     go []


(* ----- helper functions for sort checking ----- *)
(* sort_error: Info.t -> string -> string -> string -> 'a
 * 
 * [sort_error i msg s1 s2] raises an exception that prints 
 * an appropriate error message when sort-checking has failed. 
 *)
let id () = ()
let sort_error i msg ?(more = id) s1 s2 = 
  raise (Error.Harmony_error
           (fun () -> 
              Util.format "@[%s: sort error %s@ %s expected@ but %s found@ @]"
                (Info.string_of_t i) 
                msg s1 s2;
	      more ();
	      Util.format "@]"))

(* expect_sort: Info.t -> string -> V.s -> V.s -> unit
 * 
 * [expect_sort i msg expected_sort found_sort] checks that
 * [found_sort] is a subsort of [expected_sort]. if not, then it raises
 * an exception, printing the location [i] and message [msg].  
 *)
let rec expect_sort i msg expected_sort found_sort =
  if not (V.subsort found_sort expected_sort) then
    sort_error i msg 
      (V.string_of_sort expected_sort)
      (V.string_of_sort found_sort)

(* check_precise_lens_type: Info.t -> V.t -> V.t -> 
 *   (V.t env -> V.t) option -> 
 *   (V.t env -> V.t) option -> 
 *   V.t env -> 
 *   string * string
 * 
 * [check_precise_lens_type i c_found a_found c_compo a_compo ve] 
 * checks that the actual sorts [c_found] and [a_found] meet the 
 * specifications described by [c_compo] and [a_compo]. a specification 
 * is an optional (function that compiles to) a regexp. 
 * 
 * the function returns the strings given by the specification to the 
 * caller. 
 * 
 *)
let check_precise_lens_type i c_found a_found c_compo a_compo ve = 
  let (c_ok,c_why),c_str = match c_compo with
    | None -> (true,id),"?"
    | Some c_comp -> 
	let c_expected = V.get_r (c_comp ve) i in
	  (L.check_rx_equiv c_expected c_found, L.string_of_r c_expected) in
  let (a_ok,a_why),a_str = match a_compo with
    | None -> (true,id),"?"
    | Some a_comp -> 
	let a_expected = V.get_r (a_comp ve) i in
	  (L.check_rx_equiv a_expected a_found, L.string_of_r a_expected) in 
    if not (c_ok && a_ok) then 
      sort_error i "lens type annotation failed"
        ~more:(fun() -> c_why(); a_why())
        (sprintf "%s <-> %s" c_str a_str)
        (sprintf "%s <-> %s"
           (L.string_of_r c_found)
           (L.string_of_r a_found));
    (c_str,a_str)
      

(* ----- helper functions for disambiguating overloaded operators ----- *)

(* lens operators are overladed; for example, concatenation is defined
   for C-lenses, S-lenses, K-lenses, and R-lenses. therefore, when the
   parser encounters a DOT and two expressions, it needs to decide
   which concatenation operator to use. the following collection of
   functions facilitate disambiguating these overloaded operators. we
   represent the various cases using a "merge list" with type

   (V.s * V.S * (V.s -> V.S -> V.s) * (V.S -> V.S -> V.t -> V.t -> V.t) list)

   given two values (v1,v2) with sorts (s1,s2) the semantics of a
   binary operator is given by *first* matching element
   (s1',s2',sort_merge,value_merge) where s1 is a subsort of s1' and
   similarly for s2, and the sort of the result is given by
   [sort_merge s1 s2] and the value by [value_merge v1 v2].
*)

(* binary_op_error: Info.t -> string -> string -> string 
 * 
 * [binary_op_error i msg s1 s2] is called when no match
 * is found in the merge list.
 *)
let binary_op_error i msg s1 s2 = 
  raise (Error.Harmony_error
            (fun () -> 
              Util.format "@[%s: sort error %s@ no case for %s and %s@]"
                (Info.string_of_t i) 
                msg s1 s2)) 


(* binary_op_sort: Info.t -> string -> V.s -> V.s -> 
 *   (V.s * V.S * (V.s -> V.S -> V.s) * (V.S -> V.S -> V.t -> V.t -> V.t) list) -> 
 *   V.s
 * 
 * [binary_op_sort i msg s1 s2 merge] calculates the sort resulting
 * from applying the binary operation specified by the merge list
 * [merge] to values with sort [s1] and [s2].
 * 
 *)
let binary_op_sort i msg s1 s2 merge = 
  let so = Safelist.fold_left (fun acc (s1',s2',f,_) -> 
    if acc = None && V.subsort s1 s1' && V.subsort s2 s2' then Some (f s1 s2)
    else acc)
    None merge in     
    match so with 
        None -> binary_op_error i msg
          (V.string_of_sort s1)
          (V.string_of_sort s2)
      | Some s -> s 
          
(* binary_op_value: Info.t -> string -> V.t -> V.t -> 
 *   (V.s * V.S * (V.s -> V.S -> V.s) * (V.S -> V.S -> V.t -> V.t -> V.t) list) -> 
 *   V.s
 * 
 * [binary_op_sort i msg v1 v2 merge] calculates the value resulting
 * from applying the binary operation specified by the merge list
 * [merge] to values [v1] and [v2].
 * 
 *)
let binary_op_value i msg v1 v2 merge = 
  let s1 = V.sort_of_t v1 in 
  let s2 = V.sort_of_t v2 in         
  let vo = Safelist.fold_left (fun acc (s1',s2',_,f) -> 
    if acc = None && V.subsort s1 s1' && V.subsort s2 s2' then Some (f i v1 v2)
    else acc)
    None merge in 
    match vo with 
        None -> binary_op_error i msg
          (V.string_of_sort s1)
          (V.string_of_sort s2)
      | Some v -> v 
          
(* do_binary_op: Info.t -> string -> 
 *   (Info.t * (V.s env -> V.s) * ((V.t * bool) env -> V.t)) -> 
 *   (Info.t * (V.s env -> V.s) * ((V.t * bool) env -> V.t)) -> 
 *   (V.s * V.S * (V.s -> V.S -> V.s) * (V.S -> V.S -> V.t -> V.t -> V.t) list) -> 
 *   (Info.t * (V.s env -> V.s) * ((V.t * bool) env -> V.t)) 
 * 
 * [do_binary_op i op_msg exp1 exp2 merge] compiles the expression
 * for doing the operation specified by merge list [merge] on
 * [exp1] and [exp2].
 *)
let do_binary_op op_msg exp1 exp2 merge = 
  let i1,chk1,comp1 = exp1 in 
  let i2,chk2,comp2 = exp2 in 
  let i = m i1 i2 in 
    (i,
    (fun se -> 
      let s1 = chk1 se in 
      let s2 = chk2 se in 
        binary_op_sort i op_msg s1 s2 merge),
    (fun ve -> 
      let v1 = comp1 ve in 
      let v2 = comp2 ve in 
        binary_op_value i op_msg v1 v2 merge))

(* --- merge lists for the binary operations on lenses ---*)
let concat_merge = 
  [ (V.SString, V.SString,
    (fun _ _ -> V.SString),
    (fun i v1 v2 -> V.S(i,RS.append (V.get_s v1 i) (V.get_s v2 i))))
  ; (V.SRegexp, V.SRegexp,
    (fun _ _ -> V.SRegexp),
    (fun i v1 v2 -> V.R(i,L.rx_seq (V.get_r v1 i) (V.get_r v2 i))))
  ; (V.SCanonizer, V.SCanonizer,
    (fun _ _ -> V.SCanonizer),
    (fun i v1 v2 -> V.CN(i, L.Canonizer.concat i (V.get_cn v1 i) (V.get_cn v2 i))))
  ; (V.SDLens, V.SDLens, 
    (fun _ _ -> V.SDLens),
    (fun i v1 v2 -> V.DL(i,L.DLens.concat i (V.get_dl v1 i) (V.get_dl v2 i))))
  ]

let swap_merge = 
  [ (V.SDLens, V.SDLens, 
    (fun _ _ -> V.SDLens),
    (fun i v1 v2 -> V.DL(i,L.DLens.swap i (V.get_dl v1 i) (V.get_dl v2 i))))
  ]

let union_merge = 
  [ (V.SRegexp, V.SRegexp,
    (fun _ _ -> V.SRegexp),
    (fun i v1 v2 -> V.R(i,L.rx_alt (V.get_r v1 i) (V.get_r v2 i))))
  ; (V.SCanonizer, V.SCanonizer,
    (fun _ _ -> V.SCanonizer),
    (fun i v1 v2 -> V.CN(i, L.Canonizer.union i (V.get_cn v1 i) (V.get_cn v2 i))))
  ; (V.SDLens, V.SDLens, 
    (fun _ _ -> V.SDLens),
    (fun i v1 v2 -> V.DL(i,L.DLens.union i (V.get_dl v1 i) (V.get_dl v2 i))))
  ]

let compose_merge = 
  [ (V.SDLens, V.SDLens, 
    (fun _ _ -> V.SDLens),
    (fun i v1 v2 -> V.DL(i,L.DLens.compose i (V.get_dl v1 i) (V.get_dl v2 i))))
  ]


let minus_merge = 
  [ (V.SRegexp, V.SRegexp, 
    (fun _ _ -> V.SRegexp),
    (fun i v1 v2 -> V.R(i,L.rx_diff (V.get_r v1 i) (V.get_r v2 i)))) ]

let inter_merge = 
  [ (V.SRegexp, V.SRegexp, 
    (fun _ _ -> V.SRegexp),
    (fun i v1 v2 -> V.R(i,L.rx_inter (V.get_r v1 i) (V.get_r v2 i)))) ]
      
(* ----- helper functions for checking and compiling functions ----- *)
(* mk_fun_chker : 
 *   Info.t -> 
 *   (string * V.s) list -> 
 *   (V.s, (V.t Renv.t -> V.t, V.t Renv.t -> V.t) alternative) option -> 
 *   (V.s Renv.t -> V.t) -> 
 *   V.s
 * 
 * [mk_fun_chker i params sorto chk_body] constructs the function sort
 * from [params] and [chk_body], and optionally checks that the body
 * has the sort given by [sorto].
 * 
 * [sorto] is an alternative option where the Left alternative is just
 * a sort and the right alternative is a pair of functions from
 * environments to V.t-wrapped L.rx.ts standing for precise lens types
 * C <-> A. for these precise types, we only check that the body has
 * some lens sort and defer precise checking of the lens until the
 * function is applied to arguments.
 *)
let mk_fun_chker i params sorto chk_body =
  (fun se -> 
    (* first, construct 
     *   - an environment, [se'], from [se] that indludes the 
     *     parameters and their sorts 
     * 
     *   - a function, [mk_s], that takes the sort of the body
     *     and constructs the function sort from the sortrs of 
     *     the params. this function is needed because we cannot
     *     check the body until we've constructed [se'] above!
     *)
    let se',mk_s = 
      Safelist.fold_left (fun (se,mk_s) (_,xi,si) -> 
        (Renv.update se xi si,
        (fun s -> mk_s (V.SFunction(si,Some s)))))
        (se,(fun s -> s)) params in 

    (* type check the body in the new env *)
    let body_s = chk_body se' in 
      (* optionally, check that the body has its dedlared sort *)
      (match sorto with 
        | Some (Misc.Left Some (expected_sort)) -> 
            expect_sort i "" expected_sort body_s
        | Some (Misc.Right _) -> 
            (* defer precise checking until later *)
            if V.is_lens_sort body_s then () 
            else expect_sort i "" V.SDLens body_s
        | _ -> ());
      (* finally, return the function sort *)
      mk_s body_s)


(* mk_fun_cmper : 
 *   Info.t -> 
 *   (string * V.s) list -> 
 *   (V.s, (V.t Renv.t -> V.t, V.t Renv.t -> V.t) alternative) option -> 
 *   (V.t Renv.t -> V.t) -> 
 *   V.t
 * 
 * [mk_fun_cmper i params sorto cmp_body] constructs the function 
 * run-time value from [params] and [cmp_body], and optionally checks 
 * that the value has the sort given by [sorto].
 * 
 * as above, [sorto] is an alternative option. we do precise checking 
 * of lens types here. 
 * 
 * In the case of Right, the two functions c_comp and a_comp are optional.
 * We check only if there is a function. None corresponds to a question mark
 * in the concrete syntax.
 *)
let rec mk_fun_comper i f_name params sorto (comp_body : (V.t * bool) env -> V.t) = 
  (fun (ve: (V.t * bool) env)  -> match params with 
      | [] -> 
          (* if no parameters, then compile the body and optionally
           * do lens type checking *)
          let body_v = comp_body ve in 
            (match sorto with
              | Some(Misc.Right((_,c_compo),(_,a_compo))) -> 
                  let c_found,a_found = V.get_lens_sorts body_v in 
                    ignore (check_precise_lens_type i c_found a_found c_compo a_compo ve)
              | _ -> ());
              body_v 
      | (_,x,s)::rest -> 
          (* otherwise, if there are parameters, recursively make the
           * run-time value function and wrap it in a V.f *)
          V.F(i,s,(fun i v -> (mk_fun_comper i ((sprintf "%s %s") f_name x) rest sorto comp_body)
            (Renv.update ve x (v,false)))))

(* --- helper function for unit tests --- *)
(* test_fail: Info.t -> string -> string
 * 
 * [test_fail i expected_str found_str] raises an exception that prints
 * an appropriate error message indicating that the unit test failed 
 *)
let test_fail i expected_str found_str =
  let expected = RS.to_string expected_str in
  let found = RS.to_string found_str in
  let r = ref (-1) in 
  (* compute index where the two strings differ *)
  let rec loop ok = 
    try 
      if not ok then ()
      else 
        (incr r;
         loop (expected.[!r] = found.[!r])) 
    with Invalid_argument _ -> () in 
    loop true;    
    (* print error message *)
    Util.format "@[%s: test failed@\n  expected: [@["
      (Info.string_of_t i);
    L.nlify expected;
    Util.format "@]]@\n but found: [@[";
    L.nlify found;
    Util.format "@]]@\n";
    let m = max 0 (!r - 5) in
    let n = String.length expected in
    let l = min n (min 10 (n - !r + 5)) in
    Util.format "Differs at character %d, around \"%s\"@\n" 
      !r (String.sub expected m l)

(* type for differenciate ident of lenses to be taken "as is" or to be
   stared in the definitions of streaming lenses*)
type st_ident = 
    ST_as_is of string
  | ST_star of string


%}

%token <Info.t> EOF
%token <Info.t> STRING REGEXP DLENS KLENS SLENS RLENS CANONIZER
%token <Info.t * string> STR IDENT CSET NSET
%token <Info.t * int> INT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE LANGLEBAR BARRANGLE RANGLE   
%token <Info.t> ARROW DARROW LONGARROW LONGDARROW
%token <Info.t> CREATE BEGIN END FUN LET IN TEST INTO SEMI COMMA DOT EQUAL COLON BACKSLASH  
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS HAT TILDE AMPERSAND QMARK 
%token <Info.t> MATCH WITH GET PUT DOTGET DOTPUT DOTCREATE ERROR

%start top
%type <Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t -> Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t> top
%type <Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env> dedls
%type <Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)> exp
%%

  /* rather than producing an AST, this parser computes V.ts directly. 
      * 
      * therefore, parsing a top-level dedlaration produces a function that 
	  * takes two envs, one for sorts and another for values, and produces 
	    * two envs of the same kinds, updated with any bindings introduced in 
	    * that dedlaration. 
	    * 
	    * parsing an expression produces an Info.t, a "checking" function from 
		* sort envs to sorts, and a "compiling" function from value envs to 
		  * values. 
		  * 
		  * these are reflected in the type dedlarations above.
  */

  /* --------- DEDLARATIONS ---------- */
  top: 
		    | dedls EOF                               { $1 }

			dedls:      
		    | LET IDENT param_list opt_sort EQUAL exp dedls 
			{ (fun (se,ve) -> 
			     let i2,x2 = $2 in 
			     let i6,chk6,comp6 = $6 in                                                    
			     let i = m $1 i6 in 
			     let se' = Renv.update se x2 ((mk_fun_chker i $3 $4 chk6) se) in 
			     let ve' = Renv.update ve x2 (((mk_fun_comper i x2 $3 $4 comp6) ve), true) in 
			       $7 (se',ve')) }

		      /* --- unit tests --- */    
		    | TEST exp eq_arrow QMARK dedls                  
			{ (fun (se,ve) ->
			     let i2,chk2,comp2 = $2 in 
			       expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
			       let s2 = V.get_s (comp2 ve) i2 in 
				 Util.format "@[Test Result: @[";
				 L.nlify_str s2;
				 Util.format "@]@]@\n";
				 Util.flush ();
				 $5 (se,ve)) }
			
		    | TEST exp eq_arrow exp dedls                 
			{  (fun (se,ve) ->
			      let i2,chk2,comp2 = $2 in 
			      let i4,chk4,comp4 = $4 in 
			      let i = m i2 i4 in 
				expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
				expect_sort i4 "in test dedlaration:" V.SString (chk4 se);
				let s2 = V.get_s (comp2 ve) i2 in 
				let s4 = V.get_s (comp4 ve) i4 in 
				  if (RS.to_string s2) <> (RS.to_string s4) then test_fail i s4 s2;
				  $5 (se,ve)) }
			
		    | TEST exp eq_arrow ERROR dedls                
			{  (fun (se,ve) ->
			      let i2,chk2,comp2 = $2 in 
			      let i = m i2 $4 in 
				expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
				(try test_fail i (RS.of_string "error") (V.get_s (comp2 ve) i2)
				 with Error.Harmony_error _ -> ());
				$5 (se,ve)) }
			
		    | TEST composeexp GET PUT exp eq_darrow exp dedls 
			{ (fun (se,ve) -> 
			     let i2,chk2,comp2 = $2 in 
			     let i5,chk5,comp5 = $5 in 
			     let i7,chk7,comp7 = $7 in 
			     let i = m i2 i7 in 
			       expect_sort i2 "in test dedlaration:" V.SDLens (chk2 se);
			       expect_sort i5 "in test dedlaration:" V.SString (chk5 se);
			       expect_sort i7 "in test dedlaration:" V.SString (chk7 se);
			       let l2 = V.get_dl (comp2 ve) i2 in 
			       let s5 = V.get_s  (comp5 ve) i5 in 
			       let s7 = V.get_s (comp7 ve) i7 in 
			       let g = L.DLens.get l2 s5 in 
			       let c = L.DLens.rcreate_of_dl l2 s7 in  
				 if (RS.to_string g) <> (RS.to_string s7) then test_fail i s7 g;
				 if (RS.to_string c) <> (RS.to_string s5) then test_fail i s5 c;
				 $8 (se,ve)) }

		    | TEST exp COLON sort_spec dedls
			{ (fun (se,ve) -> 
			     let i2,chk2,comp2 = $2 in
			     let v2 = comp2 ve in 
			     let s2 = chk2 se in 
			       (match $4 with
				  | Misc.Left(None) -> 
				      if V.is_lens_sort s2 then 
					let c_found,a_found = V.get_lens_sorts v2 in 
					  Util.format "@[<2>Test Result (at %s): %s with type @[%s <-> %s@]@]@\n"                        
					    (Info.string_of_t i2)        
					    (V.string_of_sort s2)
					    (L.string_of_r c_found)
					    (L.string_of_r a_found)
				      else
					Util.format "@[<2>Test Result (at %s): @[%s@]@]@\n" 
					  (Info.string_of_t i2) 
					  (V.string_of_sort s2)
				  | Misc.Left(Some s) -> 
				      expect_sort i2 "in test type dedlaration:" s s2
				  | Misc.Right((_,c_compo),(_,a_compo)) -> 
				      if not (V.is_lens_sort s2) then 
					sort_error i2 "in test type dedlaration:"
					  "lens sort" (V.string_of_sort s2)
				      else
					let c_found,a_found = V.get_lens_sorts v2 in 
					let c_str,a_str = check_precise_lens_type i2 c_found a_found c_compo a_compo ve in 
					  if c_compo = None || a_compo = None then 
					    Util.format "@[<2>Test Result (at %s): %s with type @[%s <-> %s@]@]@\n"                        
					      (Info.string_of_t i2)        
					      (V.string_of_sort s2)
					      (if c_compo = None then L.string_of_r c_found else c_str)
					      (if a_compo = None then L.string_of_r a_found else a_str));          
			       $5 (se,ve)) }

		      /* --- empty decaration --- */      
		    |                                             
			{ (fun (se,ve) -> (se,ve)) }


		      /* --------- EXPRESSIONS ---------- */      
		      /* top-level expressions expressions */
			  exp:
		    | LET IDENT param_list opt_sort EQUAL exp IN exp 
			{ let i2,x2 = $2 in 
			  let i6,chk6,comp6 = $6 in 
			  let i8,chk8,comp8 = $8 in 
			  let i = m $1 i8 in 
			    (i,
			     (fun se -> chk8 (Renv.update se x2 ((mk_fun_chker i $3 $4 chk6) se))),
			     (fun ve -> comp8 (Renv.update ve x2 (((mk_fun_comper i x2 $3 $4 comp6) ve), true)))) }

		    | FUN param param_list ARROW exp      
			{ let i5,chk5,comp5 = $5 in 
			  let ps = $2::$3 in 
			  let i = m $1 i5 in 
			    (i,
			     (fun se -> (mk_fun_chker i ps None chk5) se),
			     (fun ve -> ((mk_fun_comper i "anonymous function" ps None comp5) ve))) }
			
		    | gpexp                               
			{ $1 }

		      /* "get put" expressions */
			gpexp: 
		    | composeexp get aexp               
			{ let i1,chk1,comp1 = $1 in 
			  let i3,chk3,comp3 = $3 in                                           
			  let i = m i1 i3 in 
			    (i, 
			     (fun se -> 
				expect_sort i1 "in get expression:" V.SDLens (chk1 se);
				expect_sort i3 "in get expression:" V.SString (chk3 se);
				V.SString),
			     (fun ve -> 
				let l1 = V.get_dl (comp1 ve) i1 in 
				let s3 = V.get_s (comp3 ve) i3 in                                                  
				  V.S (i,L.DLens.get l1 s3))) }

		    | composeexp put aexp INTO aexp        
			{ let i1,chk1,comp1 = $1 in 
			  let i3,chk3,comp3 = $3 in 
			  let i5,chk5,comp5 = $5 in 
			  let i = m i1 i5 in 
			    (i, 
			     (fun se -> 
				expect_sort i1 "in put expression:" V.SDLens (chk1 se);
				expect_sort i3 "in put expression:" V.SString (chk3 se);
				expect_sort i5 "in put expression:" V.SString (chk5 se);
				V.SString),
			     (fun ve ->                                                
				let l1 = V.get_dl (comp1 ve) i1 in 
				let s3 = V.get_s (comp3 ve) i3 in
				let s5 = V.get_s (comp5 ve) i5 in
				  V.S (i,L.DLens.rput_of_dl l1 s3 s5))) }
			
		    | composeexp create aexp               
			{ let i1,chk1,comp1 = $1 in 
			  let i3,chk3,comp3 = $3 in 
			  let i = m i1 i3 in 
			    (i, 
			     (fun se -> 
				expect_sort i1 "in create expression:" V.SDLens (chk1 se);
				expect_sort i3 "in create expression:" V.SString (chk3 se);
				V.SString),
			     (fun ve -> 
				let l1 = V.get_dl (comp1 ve) i1 in 
				let s2 = V.get_s (comp3 ve) i3 in                                                  
				  V.S (i,L.DLens.rcreate_of_dl l1 s2))) }
		    | composeexp                         
			{ $1 }

		      /* "compose" expressions */
			composeexp:
		    | composeexp SEMI bexp                
			{ do_binary_op "in compose expression" $1 $3 compose_merge }
			
		    | bexp
			{ $1 }

		      /* "bar" expressions */
			bexp:
		    | bexp2 BAR iexp 
			{ do_binary_op "in union expression" $1 $3 union_merge }

		    | mexp
			{ $1 }

			bexp2:
		    | bexp2 BAR iexp
			{ do_binary_op "in union expression" $1 $3 union_merge }

		    | iexp
			{ $1 }

		      /* "minus" expressions */
			mexp:
		    | mexp MINUS iexp
			{ do_binary_op "in minus expression" $1 $3 minus_merge }

		    | iexp 
			{ $1 }

		      /* "inter" expressions */
			iexp:
		    | iexp AMPERSAND sexp
			{ do_binary_op "in intersection expression" $1 $3 inter_merge }

		    | sexp 
			{ $1 }


		      /* "swap" expressions */
			sexp:
		    | sexp TILDE cexp                     
			{ do_binary_op "in swap expression" $1 $3 swap_merge }

		    | cexp
			{ $1 }

		      /* "concat" expressions */
			cexp:
		    | cexp DOT appexp                     
			{ do_binary_op "in concat expression" $1 $3 concat_merge }

		    | appexp                              
			{ $1 }

		      /* "application" expressions */
			appexp:
		    | appexp texp                         
			{ let i1,chk1,comp1 = $1 in 
			  let i2,chk2,comp2 = $2 in 
			  let i = m i1 i2 in 
			    (i, 
			     (fun se -> 
				match chk1 se with 
				  | V.SFunction(s11,Some s12) -> 
				      expect_sort i "in application expression:" s11 (chk2 se);
				      s12
				  | V.SFunction(s11,None) -> chk2 se
				  | s1 -> 
				      sort_error 
					i1
					"in application expression" 
					"function" 
					(V.string_of_sort s1)),
			     (fun ve -> 
				let f1 = V.get_f (comp1 ve) i1 in
				  f1 i2 (comp2 ve))) }

		    | texp                                
			{ $1 }

		      /* "translate" expressions */
			texp:
		    | texp DARROW rexp
			{ let i1,chk1,comp1 = $1 in 
			  let i3,chk3,comp3 = $3 in 
			  let i = m i1 i3 in 
			    (i, 
			     (fun se -> 
				expect_sort i1 "in set expression:" V.SRegexp (chk1 se);
				expect_sort i3 "in set expression:" V.SString (chk3 se);
				V.SDLens),
			     (fun ve -> 
				let f1 = V.get_f (fst (V.lookup i ve "set")) i in 
				let v1 = comp1 ve in 
				let f2 = V.get_f (f1 i1 v1) i in 
				let v3 = comp3 ve in 
				  f2 i3 v3)) }
		    | rexp                                
			{ $1 }

			
		      /* "repetition" expressions */
			rexp:
		    | aexp rep                            
			{ let i1,chk1,comp1 = $1 in 
			  let i2,(min,maxo) = $2 in     
			  let i = m i1 i2 in 
			    (i,
			     (fun se -> 
				let s1 = chk1 se in 
				  match s1 with
				    | V.SString | V.SRegexp -> V.SRegexp
				    | V.SCanonizer -> V.SCanonizer
				    | V.SDLens -> V.SDLens 
				    | s -> 
					sort_error i "in kleene-star expression:"
					  "string, regexp, or lens sort"
					  (V.string_of_sort s)), 
			     (fun ve -> 
				(* desugar finite repetitions *)            
            let mk_cat v1 v2 = binary_op_value i "in concat expresion" v1 v2 concat_merge in 
            let mk_union v1 v2 = binary_op_value i "in union expresion" v1 v2 union_merge in 
            let rec mk_cats l acc = function
                0 -> acc
              | 1 -> mk_cat l acc
              | n -> mk_cats l (mk_cat l acc) (pred n) in 
            let mk_star v1 = 
              match V.sort_of_t v1 with
                | V.SString | V.SRegexp -> 
                    V.R(i,L.rx_star (V.get_r v1 i))
                | V.SCanonizer -> 
                    V.CN(i,L.Canonizer.star i (V.get_cn v1 i))
                | V.SDLens -> 
                    V.DL(i,L.DLens.star i (V.get_dl v1 i))
                | s -> 
                    sort_error i "in kleene-star expression:"
                      "string, regexp, or lens sort"
                      (V.string_of_sort s) in 
            let epsilon = fst (V.lookup i ve "epsilon") in 
            let v1 = comp1 ve in 
              match min,maxo with 
                  0,None -> mk_star v1 
                | n,None -> mk_cats v1 (mk_star v1) n
                | 0,Some 0 -> epsilon 
                | 0,Some 1 -> mk_union epsilon v1
                | m,Some n -> 
                    if m > n then
                      raise (Error.Harmony_error (fun () -> 
                        Util.format "@[%s: error in repetition %d > %d"
                          (Info.string_of_t i) 
                          m n)) 
                    else if m=n then 
                      mk_cats v1 v1 (pred m)
                    else
                      let v0 = if m=0 then epsilon else mk_cats v1 v1 (pred m) in 
                      let k = n - m + 1 in 
                      let a = Array.make k v0 in 
                        a.(0) <- v0;
                        for i=1 to (pred k) do 
                          if i=1 && m=0 then a.(i) <- v1 
                          else a.(i) <- mk_cat v1 a.(pred i);
                        done;
                        Safelist.fold_left mk_union v0 (Safelist.tl (Array.to_list a)))) }
  | aexp                                
      { $1 }
            
/* "atomic" expressions */
aexp:
  | LANGLE IDENT RANGLE
      { let i2,x2 = $2 in 
        let i = m $1 $3 in 
          (i,
          (fun se -> 
            (*Util.format "FOUND MATCH %s@\n" (Info.string_of_t i);*)
            expect_sort i2 "in match expression:" V.SDLens (V.lookup i2 se x2);            
            V.SDLens),
          (fun ve ->             
            let v2 = fst (V.lookup i2 ve x2) in 
              V.DL(i,L.DLens.smatch i (RS.empty) (V.get_dl v2 i2)))) }

  | LANGLE IDENT COLON IDENT RANGLE
      { (* the first IDENT is the tag to be used for the match
	   the second is the name of the lense to be matched *)
	let i2,x2 = $2 in 
	let i4,x4 = $4 in
        let i = m $1 $5 in 
          (i,
          (fun se -> 
            (*Util.format "FOUND MATCH %s@\n" (Info.string_of_t i);*)
            expect_sort i4 "in match expression:" V.SDLens (V.lookup i4 se x4);            
            V.SDLens),
          (fun ve ->             
            let v4 = fst (V.lookup i4 ve x4) in 
              V.DL(i,L.DLens.smatch i (RS.of_string x2) (V.get_dl v4 i4)))) }


  | IDENT                               
      { let i,x = $1 in 
          (i,
          (fun se -> V.lookup i se x),
          (fun ve -> 
            let v1,is_decl = V.lookup i ve x in 
              (match is_decl,V.sort_of_t v1 with
                | true,V.SRegexp ->                     
                    let r = V.get_r v1 i in 
                      V.R(i,L.rx_set_str r x)
                | _ -> v1))) }

  | CSET                                
      { let i1,s1 = $1 in 
          (i1,
          (fun se -> V.SRegexp),
          (fun ve -> V.R(i1,L.rx_set (parse_cset s1)))) }

  | NSET                                
      { let i1,s1 = $1 in 
          (i1,
          (fun se -> V.SRegexp),
          (fun ve -> V.R(i1,L.rx_negset (parse_cset s1)))) }

  | STR 
      { let i,s = $1 in 
          (i, 
          (fun se ->  V.SString),
          (fun ve -> V.S(i,RS.of_string s))) }
      
  | LPAREN exp RPAREN                   
      { let (_,se,ve) = $2 in (m $1 $3, se,ve) }

  | BEGIN exp END                       
      { let (_,se,ve) = $2 in (m $1 $3, se,ve) }
  | stlens
      {$1}

/* ------- ST-LENSES -------- */
stlens:
  | LANGLEBAR stlenslist
      { let i1 = $1 in
	let i2,l = $2 in
	let i = m i1 i2 in
	  (i,
	  (fun se -> 
	     Safelist.iter
	       (function
		  | i,ST_star x 
		  | i,ST_as_is x -> expect_sort i "in match expression:" V.SDLens (V.lookup i se x))
	       l;
	     V.SStLens),
	  (fun ve -> 
	     let rec aux = function
	       | [] -> []
	       | (i, ST_as_is x)::t -> 
		   let v = fst (V.lookup i ve x) in
		     (i, L.StLens.S_dl (V.get_dl v i)) :: (aux t)
	       | (i, ST_star x)::t -> 
		   let v = fst (V.lookup i ve x) in
		     (i, L.StLens.S_sdl (V.get_dl v i)) :: (aux t) in
	     V.StL(i, L.StLens.of_list (aux l) i)))}


stlenslist:
  | endstlenslist
      {$1}
  | IDENT colstlenslist
      { let i1, x = $1 in
	let i2, l = $2 in
	(i2, (i1, ST_as_is x)::l)}
  | IDENT STAR colstlenslist
      { let i1, x = $1 in
	let i3, l = $3 in
	(i3, (i1, ST_star x)::l)}

colstlenslist:
  | endstlenslist
      {$1}
  | SEMI stlenslist
      {$2}

endstlenslist:
  | BARRANGLE
      { let i1 = $1 in
	  (i1, [])}


/* --------- SORTS ---------- */
sort: 
  | asort ARROW sort                    
      { V.SFunction($1,Some $3) }

  | asort                               
      { $1 }

asort:
  | STRING 
      { V.SString }

  | REGEXP 
      { V.SRegexp }

  | CANONIZER
      { V.SCanonizer }

  | DLENS  
      { V.SDLens }

  | LPAREN sort RPAREN                  
      { $2 }

opt_sort:
  |                                     
      { None }

  | COLON sort_spec
      { Some $2 }

sort_spec:
  | QMARK
      { Misc.Left (None) }

  | sort
      { Misc.Left (Some $1) }

  | qmark_or_exp DARROW qmark_or_exp
      { Misc.Right($1,$3) }
      
/* ---------- PRECISE LENS TYPES --------- */
/* either an ident or a regexp; returns an Info.t * V.t env -> V.t */
qmark_or_exp:
  | QMARK
      { (* '?' is used when you do not want to type check
	   one of the the types. For example if you wrote a 
	   definition of the abstract domain and want to verifiy
	   that your different lenses go to this abstract domain 
	   but you don't want to explicitly write the definition of
	   all the concrete domains *)
	let i = $1 in (i, None)}

  | rexp
      { let i1,chk2,comp1 = $1 in 
          (i1, Some (fun ve -> comp1 ve)) }

/* --------- PARAMETERS ---------- */
param_list:
  |                                     
      { [] }
  | param param_list                    
      { $1 :: $2 }

param: 
  | LPAREN IDENT COLON sort RPAREN      
      { let i,x = $2 in (i,x,$4) }

/* --------- REPETITIONS ---------- */
/* returns an (int * int option) */
rep: 
  | STAR                                
      { ($1, (0, None)) }

  | PLUS                                
      { ($1, (1, None)) }

  | QMARK                               
      { ($1, (0,Some 1)) }

  | LBRACE INT RBRACE                   
      { let i = m $1 $3 in let _,n = $2 in (i, (n,Some n)) }

  | LBRACE INT COMMA RBRACE             
      { let i = m $1 $3 in let _,n = $2 in (i, (n,None)) }

  | LBRACE INT COMMA INT RBRACE         
      { let i = m $1 $5 in let _,n2 = $2 in let _,n4 = $4 in (i, (n2, Some n4)) }

/* --------- MISC SYMBOLS ---------- */
eq_arrow:
  | EQUAL                               
      { $1 }

  | LONGARROW                           
      { $1 }

eq_darrow:
  | EQUAL                               
      { $1 }

  | LONGDARROW                          
      { $1 }

get:
  | GET                                 
      { $1 }

  | DOTGET                              
      { $1 }        

put:
  | PUT                                 
      { $1 }

  | DOTPUT                              
      { $1 }        

create:
  | CREATE                                 
      { $1 }

  | DOTCREATE                              
      { $1 }        

