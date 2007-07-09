type token =
  | EOF of (Info.t)
  | STRING of (Info.t)
  | REGEXP of (Info.t)
  | DLENS of (Info.t)
  | KLENS of (Info.t)
  | SLENS of (Info.t)
  | RLENS of (Info.t)
  | CANONIZER of (Info.t)
  | STR of (Info.t * string)
  | IDENT of (Info.t * string)
  | CSET of (Info.t * string)
  | NSET of (Info.t * string)
  | INT of (Info.t * int)
  | LBRACE of (Info.t)
  | RBRACE of (Info.t)
  | LBRACK of (Info.t)
  | RBRACK of (Info.t)
  | LPAREN of (Info.t)
  | RPAREN of (Info.t)
  | LANGLE of (Info.t)
  | RANGLE of (Info.t)
  | ARROW of (Info.t)
  | DARROW of (Info.t)
  | LONGARROW of (Info.t)
  | LONGDARROW of (Info.t)
  | CREATE of (Info.t)
  | BEGIN of (Info.t)
  | END of (Info.t)
  | FUN of (Info.t)
  | LET of (Info.t)
  | IN of (Info.t)
  | TEST of (Info.t)
  | INTO of (Info.t)
  | SEMI of (Info.t)
  | COMMA of (Info.t)
  | DOT of (Info.t)
  | EQUAL of (Info.t)
  | COLON of (Info.t)
  | BACKSLASH of (Info.t)
  | STAR of (Info.t)
  | RLUS of (Info.t)
  | BANG of (Info.t)
  | BAR of (Info.t)
  | PLUS of (Info.t)
  | MINUS of (Info.t)
  | HAT of (Info.t)
  | TILDE of (Info.t)
  | AMPERSAND of (Info.t)
  | QMARK of (Info.t)
  | MATCH of (Info.t)
  | WITH of (Info.t)
  | GET of (Info.t)
  | PUT of (Info.t)
  | DOTGET of (Info.t)
  | DOTPUT of (Info.t)
  | DOTCREATE of (Info.t)
  | ERROR of (Info.t)

open Parsing;;
# 1 "rparser.mly"
 
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
# 465 "rparser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
    0 (* EOF *);
  257 (* STRING *);
  258 (* REGEXP *);
  259 (* DLENS *);
  260 (* KLENS *);
  261 (* SLENS *);
  262 (* RLENS *);
  263 (* CANONIZER *);
  264 (* STR *);
  265 (* IDENT *);
  266 (* CSET *);
  267 (* NSET *);
  268 (* INT *);
  269 (* LBRACE *);
  270 (* RBRACE *);
  271 (* LBRACK *);
  272 (* RBRACK *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LANGLE *);
  276 (* RANGLE *);
  277 (* ARROW *);
  278 (* DARROW *);
  279 (* LONGARROW *);
  280 (* LONGDARROW *);
  281 (* CREATE *);
  282 (* BEGIN *);
  283 (* END *);
  284 (* FUN *);
  285 (* LET *);
  286 (* IN *);
  287 (* TEST *);
  288 (* INTO *);
  289 (* SEMI *);
  290 (* COMMA *);
  291 (* DOT *);
  292 (* EQUAL *);
  293 (* COLON *);
  294 (* BACKSLASH *);
  295 (* STAR *);
  296 (* RLUS *);
  297 (* BANG *);
  298 (* BAR *);
  299 (* PLUS *);
  300 (* MINUS *);
  301 (* HAT *);
  302 (* TILDE *);
  303 (* AMPERSAND *);
  304 (* QMARK *);
  305 (* MATCH *);
  306 (* WITH *);
  307 (* GET *);
  308 (* PUT *);
  309 (* DOTGET *);
  310 (* DOTPUT *);
  311 (* DOTCREATE *);
  312 (* ERROR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\011\000\011\000\011\000\011\000\007\000\
\007\000\016\000\016\000\017\000\017\000\019\000\019\000\018\000\
\018\000\020\000\020\000\021\000\021\000\022\000\022\000\023\000\
\023\000\024\000\024\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\026\000\026\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\005\000\005\000\009\000\009\000\
\009\000\028\000\028\000\004\000\004\000\010\000\025\000\025\000\
\025\000\025\000\025\000\025\000\006\000\006\000\008\000\008\000\
\012\000\012\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\007\000\005\000\005\000\005\000\008\000\005\000\000\000\
\008\000\005\000\001\000\003\000\005\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\002\000\001\000\003\000\
\001\000\002\000\001\000\003\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\000\000\002\000\001\000\001\000\
\003\000\001\000\001\000\000\000\002\000\005\000\001\000\001\000\
\001\000\003\000\004\000\005\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\079\000\000\000\000\000\040\000\
\037\000\038\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\033\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\070\000\
\069\000\000\000\000\000\077\000\000\000\000\000\075\000\074\000\
\076\000\078\000\000\000\000\000\000\000\000\000\063\000\064\000\
\065\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\041\000\073\000\036\000\
\042\000\000\000\000\000\045\000\046\000\048\000\049\000\050\000\
\051\000\047\000\000\000\000\000\000\000\059\000\056\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\012\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\000\000\054\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\003\000\005\000\004\000\000\000\000\000\066\000\000\000\
\000\000\000\000\000\000\010\000\000\000\052\000\043\000\058\000\
\057\000\072\000\071\000\000\000\013\000\000\000\067\000\062\000\
\002\000\000\000\000\000\068\000\000\000\006\000\009\000"

let yydgoto = "\002\000\
\005\000\006\000\034\000\032\000\068\000\043\000\035\000\132\000\
\085\000\033\000\019\000\051\000\020\000\052\000\053\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\058\000\087\000\088\000\089\000"

let yysindex = "\007\000\
\005\255\000\000\003\255\157\255\000\000\037\000\025\255\000\000\
\000\000\000\000\000\000\157\255\046\255\157\255\025\255\050\255\
\244\254\047\255\000\000\019\255\000\000\006\255\028\255\042\255\
\031\255\044\255\179\255\065\255\000\000\000\000\079\255\052\255\
\025\255\073\255\092\255\072\255\066\255\025\255\025\255\000\000\
\000\000\105\255\012\255\000\000\179\255\043\255\000\000\000\000\
\000\000\000\000\179\255\179\255\179\255\082\255\000\000\000\000\
\000\000\000\000\179\255\179\255\179\255\179\255\179\255\065\255\
\179\255\059\255\105\255\069\255\000\000\000\000\000\000\000\000\
\000\000\097\255\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\153\255\000\000\005\255\000\000\000\000\098\255\
\101\255\005\255\005\255\005\255\000\000\157\255\000\000\088\255\
\000\000\249\254\028\255\031\255\028\255\044\255\179\255\000\000\
\211\255\000\000\157\255\157\255\090\255\109\255\000\000\211\255\
\035\255\000\000\000\000\000\000\250\254\179\255\000\000\039\255\
\211\255\111\255\005\255\000\000\157\255\000\000\000\000\000\000\
\000\000\000\000\000\000\157\255\000\000\116\255\000\000\000\000\
\000\000\102\255\005\255\000\000\157\255\000\000\000\000"

let yyrindex = "\000\000\
\133\000\000\000\000\000\000\000\000\000\000\000\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\255\000\000\001\000\000\000\000\000\109\001\223\001\
\033\001\211\000\135\000\049\000\000\000\000\000\000\000\100\255\
\029\255\000\000\005\002\000\000\000\000\113\255\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\183\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\097\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\100\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\133\000\000\000\000\000\033\000\
\000\000\133\000\133\000\133\000\000\000\000\000\000\000\000\000\
\000\000\000\000\147\001\071\001\185\001\249\000\173\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\133\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\133\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\180\255\252\255\043\000\063\000\000\000\136\000\000\000\
\072\000\126\000\000\000\000\000\209\255\000\000\000\000\103\000\
\000\000\214\255\000\000\082\000\087\000\088\000\231\255\217\255\
\000\000\208\255\000\000\039\000"

let yytablesize = 810
let yytable = "\017\000\
\035\000\064\000\086\000\095\000\096\000\097\000\119\000\001\000\
\111\000\037\000\040\000\007\000\055\000\114\000\115\000\116\000\
\099\000\130\000\101\000\008\000\009\000\010\000\011\000\041\000\
\042\000\104\000\120\000\086\000\012\000\131\000\013\000\054\000\
\044\000\003\000\110\000\004\000\030\000\014\000\092\000\015\000\
\016\000\031\000\008\000\009\000\010\000\011\000\137\000\059\000\
\031\000\060\000\134\000\012\000\135\000\013\000\036\000\015\000\
\122\000\055\000\039\000\090\000\014\000\056\000\142\000\127\000\
\060\000\060\000\057\000\091\000\015\000\015\000\133\000\044\000\
\110\000\086\000\060\000\069\000\062\000\064\000\063\000\045\000\
\074\000\075\000\128\000\060\000\060\000\061\000\065\000\066\000\
\067\000\117\000\070\000\072\000\073\000\098\000\094\000\105\000\
\030\000\046\000\047\000\048\000\049\000\050\000\123\000\124\000\
\107\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\008\000\009\000\010\000\011\000\044\000\108\000\112\000\118\000\
\138\000\083\000\113\000\013\000\045\000\125\000\126\000\139\000\
\136\000\140\000\014\000\141\000\008\000\060\000\029\000\053\000\
\143\000\109\000\106\000\018\000\038\000\100\000\071\000\047\000\
\048\000\049\000\050\000\093\000\102\000\000\000\103\000\129\000\
\084\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\008\000\009\000\010\000\011\000\008\000\009\000\010\000\011\000\
\000\000\083\000\000\000\013\000\028\000\012\000\000\000\013\000\
\000\000\000\000\014\000\000\000\015\000\016\000\014\000\000\000\
\015\000\016\000\008\000\009\000\010\000\011\000\073\000\073\000\
\073\000\073\000\000\000\012\000\000\000\013\000\000\000\073\000\
\000\000\073\000\000\000\000\000\014\000\000\000\000\000\000\000\
\073\000\000\000\027\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\121\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\035\000\035\000\035\000\000\000\000\000\000\000\000\000\
\000\000\035\000\035\000\035\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\000\000\035\000\035\000\035\000\
\025\000\035\000\058\000\035\000\035\000\035\000\000\000\000\000\
\000\000\055\000\035\000\055\000\035\000\000\000\035\000\035\000\
\055\000\000\000\044\000\035\000\035\000\035\000\035\000\035\000\
\031\000\031\000\031\000\031\000\000\000\044\000\000\000\044\000\
\000\000\031\000\031\000\031\000\044\000\000\000\024\000\031\000\
\031\000\031\000\031\000\031\000\000\000\031\000\031\000\031\000\
\000\000\031\000\000\000\031\000\031\000\031\000\000\000\000\000\
\000\000\000\000\031\000\000\000\031\000\000\000\031\000\031\000\
\000\000\000\000\000\000\031\000\031\000\031\000\031\000\031\000\
\030\000\030\000\030\000\030\000\023\000\000\000\000\000\000\000\
\000\000\030\000\030\000\030\000\000\000\000\000\000\000\030\000\
\030\000\030\000\030\000\030\000\000\000\030\000\030\000\030\000\
\000\000\030\000\000\000\030\000\030\000\030\000\000\000\000\000\
\000\000\000\000\030\000\000\000\030\000\000\000\030\000\030\000\
\000\000\000\000\018\000\030\000\030\000\030\000\030\000\030\000\
\029\000\000\000\000\000\000\000\000\000\029\000\029\000\029\000\
\000\000\029\000\000\000\029\000\029\000\029\000\000\000\029\000\
\000\000\029\000\029\000\029\000\000\000\000\000\000\000\000\000\
\029\000\000\000\029\000\000\000\029\000\029\000\000\000\000\000\
\022\000\029\000\029\000\029\000\029\000\029\000\028\000\000\000\
\000\000\000\000\000\000\028\000\028\000\028\000\000\000\028\000\
\000\000\028\000\028\000\028\000\000\000\028\000\000\000\028\000\
\028\000\028\000\000\000\000\000\000\000\000\000\028\000\000\000\
\028\000\000\000\028\000\028\000\000\000\000\000\019\000\028\000\
\028\000\028\000\028\000\028\000\027\000\000\000\000\000\000\000\
\000\000\027\000\027\000\027\000\000\000\027\000\000\000\027\000\
\027\000\027\000\000\000\027\000\000\000\000\000\027\000\027\000\
\000\000\000\000\000\000\000\000\027\000\000\000\027\000\000\000\
\027\000\027\000\000\000\000\000\015\000\027\000\027\000\027\000\
\027\000\027\000\026\000\000\000\000\000\000\000\000\000\026\000\
\026\000\026\000\000\000\026\000\000\000\026\000\026\000\026\000\
\000\000\026\000\000\000\000\000\026\000\026\000\000\000\000\000\
\000\000\000\000\026\000\000\000\026\000\000\000\026\000\026\000\
\000\000\000\000\000\000\026\000\026\000\026\000\026\000\026\000\
\000\000\000\000\025\000\000\000\000\000\000\000\000\000\025\000\
\025\000\025\000\000\000\025\000\000\000\025\000\025\000\025\000\
\000\000\025\000\000\000\000\000\025\000\025\000\000\000\000\000\
\000\000\000\000\025\000\000\000\025\000\000\000\000\000\025\000\
\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\
\024\000\000\000\000\000\000\000\000\000\024\000\024\000\024\000\
\000\000\024\000\000\000\024\000\024\000\024\000\000\000\024\000\
\000\000\000\000\024\000\024\000\000\000\000\000\000\000\000\000\
\024\000\000\000\024\000\000\000\000\000\024\000\000\000\000\000\
\000\000\024\000\024\000\024\000\024\000\024\000\023\000\000\000\
\000\000\000\000\000\000\023\000\023\000\023\000\000\000\023\000\
\000\000\023\000\023\000\023\000\000\000\023\000\000\000\000\000\
\023\000\023\000\000\000\000\000\000\000\000\000\021\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\018\000\000\000\000\000\000\000\
\000\000\018\000\018\000\018\000\000\000\018\000\000\000\018\000\
\018\000\018\000\000\000\018\000\000\000\000\000\018\000\018\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\018\000\018\000\
\018\000\018\000\022\000\000\000\000\000\000\000\000\000\022\000\
\022\000\022\000\000\000\022\000\000\000\022\000\022\000\022\000\
\000\000\022\000\000\000\000\000\022\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\022\000\022\000\022\000\022\000\
\019\000\000\000\000\000\000\000\000\000\019\000\019\000\019\000\
\000\000\019\000\000\000\019\000\019\000\019\000\000\000\019\000\
\000\000\000\000\019\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\019\000\019\000\019\000\019\000\015\000\000\000\
\000\000\000\000\000\000\015\000\015\000\000\000\000\000\015\000\
\000\000\015\000\015\000\015\000\000\000\000\000\000\000\000\000\
\015\000\015\000"

let yycheck = "\004\000\
\000\000\027\000\042\000\051\000\052\000\053\000\014\001\001\000\
\085\000\014\000\023\001\009\001\000\000\090\000\091\000\092\000\
\059\000\024\001\061\000\008\001\009\001\010\001\011\001\036\001\
\037\001\065\000\034\001\067\000\017\001\036\001\019\001\013\001\
\000\000\029\001\083\000\031\001\000\000\026\001\043\000\028\001\
\029\001\017\001\008\001\009\001\010\001\011\001\123\000\042\001\
\000\000\021\001\012\001\017\001\014\001\019\001\009\001\023\001\
\105\000\039\001\009\001\048\001\026\001\043\001\139\000\112\000\
\036\001\037\001\048\001\056\001\036\001\037\001\118\000\025\001\
\121\000\113\000\047\001\033\000\046\001\103\000\035\001\033\001\
\038\000\039\000\048\001\036\001\037\001\044\001\022\001\009\001\
\037\001\094\000\018\001\020\001\027\001\012\001\052\001\037\001\
\000\000\051\001\052\001\053\001\054\001\055\001\107\000\108\000\
\036\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\025\001\021\001\021\001\032\001\
\125\000\017\001\022\001\019\001\033\001\036\001\018\001\132\000\
\018\001\014\001\026\001\030\001\000\000\021\001\000\000\036\001\
\141\000\075\000\067\000\004\000\015\000\060\000\051\001\052\001\
\053\001\054\001\055\001\045\000\062\000\255\255\063\000\113\000\
\048\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\008\001\009\001\010\001\011\001\
\255\255\017\001\255\255\019\001\000\000\017\001\255\255\019\001\
\255\255\255\255\026\001\255\255\028\001\029\001\026\001\255\255\
\028\001\029\001\008\001\009\001\010\001\011\001\008\001\009\001\
\010\001\011\001\255\255\017\001\255\255\019\001\255\255\017\001\
\255\255\019\001\255\255\255\255\026\001\255\255\255\255\255\255\
\026\001\255\255\000\000\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\000\000\033\001\022\001\035\001\036\001\037\001\255\255\255\255\
\255\255\029\001\042\001\031\001\044\001\255\255\046\001\047\001\
\036\001\255\255\018\001\051\001\052\001\053\001\054\001\055\001\
\008\001\009\001\010\001\011\001\255\255\029\001\255\255\031\001\
\255\255\017\001\018\001\019\001\036\001\255\255\000\000\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\035\001\036\001\037\001\255\255\255\255\
\255\255\255\255\042\001\255\255\044\001\255\255\046\001\047\001\
\255\255\255\255\255\255\051\001\052\001\053\001\054\001\055\001\
\008\001\009\001\010\001\011\001\000\000\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\035\001\036\001\037\001\255\255\255\255\
\255\255\255\255\042\001\255\255\044\001\255\255\046\001\047\001\
\255\255\255\255\000\000\051\001\052\001\053\001\054\001\055\001\
\018\001\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\027\001\255\255\029\001\030\001\031\001\255\255\033\001\
\255\255\035\001\036\001\037\001\255\255\255\255\255\255\255\255\
\042\001\255\255\044\001\255\255\046\001\047\001\255\255\255\255\
\000\000\051\001\052\001\053\001\054\001\055\001\018\001\255\255\
\255\255\255\255\255\255\023\001\024\001\025\001\255\255\027\001\
\255\255\029\001\030\001\031\001\255\255\033\001\255\255\035\001\
\036\001\037\001\255\255\255\255\255\255\255\255\042\001\255\255\
\044\001\255\255\046\001\047\001\255\255\255\255\000\000\051\001\
\052\001\053\001\054\001\055\001\018\001\255\255\255\255\255\255\
\255\255\023\001\024\001\025\001\255\255\027\001\255\255\029\001\
\030\001\031\001\255\255\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\255\255\255\255\042\001\255\255\044\001\255\255\
\046\001\047\001\255\255\255\255\000\000\051\001\052\001\053\001\
\054\001\055\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\042\001\255\255\044\001\255\255\046\001\047\001\
\255\255\255\255\255\255\051\001\052\001\053\001\054\001\055\001\
\255\255\255\255\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\042\001\255\255\044\001\255\255\255\255\047\001\
\255\255\255\255\255\255\051\001\052\001\053\001\054\001\055\001\
\018\001\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\027\001\255\255\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\037\001\255\255\255\255\255\255\255\255\
\042\001\255\255\044\001\255\255\255\255\047\001\255\255\255\255\
\255\255\051\001\052\001\053\001\054\001\055\001\018\001\255\255\
\255\255\255\255\255\255\023\001\024\001\025\001\255\255\027\001\
\255\255\029\001\030\001\031\001\255\255\033\001\255\255\255\255\
\036\001\037\001\255\255\255\255\255\255\255\255\042\001\255\255\
\044\001\255\255\255\255\255\255\255\255\255\255\255\255\051\001\
\052\001\053\001\054\001\055\001\018\001\255\255\255\255\255\255\
\255\255\023\001\024\001\025\001\255\255\027\001\255\255\029\001\
\030\001\031\001\255\255\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\255\255\255\255\042\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\051\001\052\001\053\001\
\054\001\055\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\255\255\255\255\044\001\255\255\255\255\255\255\
\255\255\255\255\255\255\051\001\052\001\053\001\054\001\055\001\
\018\001\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\027\001\255\255\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\051\001\052\001\053\001\054\001\055\001\018\001\255\255\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\027\001\
\255\255\029\001\030\001\031\001\255\255\255\255\255\255\255\255\
\036\001\037\001"

let yynames_const = "\
  "

let yynames_block = "\
  EOF\000\
  STRING\000\
  REGEXP\000\
  DLENS\000\
  KLENS\000\
  SLENS\000\
  RLENS\000\
  CANONIZER\000\
  STR\000\
  IDENT\000\
  CSET\000\
  NSET\000\
  INT\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  LPAREN\000\
  RPAREN\000\
  LANGLE\000\
  RANGLE\000\
  ARROW\000\
  DARROW\000\
  LONGARROW\000\
  LONGDARROW\000\
  CREATE\000\
  BEGIN\000\
  END\000\
  FUN\000\
  LET\000\
  IN\000\
  TEST\000\
  INTO\000\
  SEMI\000\
  COMMA\000\
  DOT\000\
  EQUAL\000\
  COLON\000\
  BACKSLASH\000\
  STAR\000\
  RLUS\000\
  BANG\000\
  BAR\000\
  PLUS\000\
  MINUS\000\
  HAT\000\
  TILDE\000\
  AMPERSAND\000\
  QMARK\000\
  MATCH\000\
  WITH\000\
  GET\000\
  PUT\000\
  DOTGET\000\
  DOTPUT\000\
  DOTCREATE\000\
  ERROR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 436 "rparser.mly"
                                            ( _1 )
# 905 "rparser.ml"
               : Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t -> Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Info.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'param_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_sort) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 440 "rparser.mly"
      ( (fun (se,ve) -> 
        let i2,x2 = _2 in 
        let i6,chk6,comp6 = _6 in                                                    
        let i = m _1 i6 in 
        let se' = Renv.update se x2 ((mk_fun_chker i _3 _4 chk6) se) in 
        let ve' = Renv.update ve x2 (((mk_fun_comper i x2 _3 _4 comp6) ve), true) in 
          _7 (se',ve')) )
# 924 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'eq_arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 450 "rparser.mly"
      ( (fun (se,ve) ->
        let i2,chk2,comp2 = _2 in 
          expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
          let s2 = V.get_s (comp2 ve) i2 in 
            Util.format "@[Test Result: @[";
            L.nlify_str s2;
            Util.format "@]@]@\n";
	    Util.flush ();
            _5 (se,ve)) )
# 943 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'eq_arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 461 "rparser.mly"
      (  (fun (se,ve) ->
        let i2,chk2,comp2 = _2 in 
        let i4,chk4,comp4 = _4 in 
        let i = m i2 i4 in 
          expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
          expect_sort i4 "in test dedlaration:" V.SString (chk4 se);
          let s2 = V.get_s (comp2 ve) i2 in 
          let s4 = V.get_s (comp4 ve) i4 in 
            if (RS.to_string s2) <> (RS.to_string s4) then test_fail i s4 s2;
            _5 (se,ve)) )
# 963 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'eq_arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 473 "rparser.mly"
      (  (fun (se,ve) ->
        let i2,chk2,comp2 = _2 in 
        let i = m i2 _4 in 
          expect_sort i2 "in test dedlaration:" V.SString (chk2 se);
          (try test_fail i (RS.of_string "error") (V.get_s (comp2 ve) i2)
            with Error.Harmony_error _ -> ());
          _5 (se,ve)) )
# 980 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'composeexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Info.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'eq_darrow) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 482 "rparser.mly"
      ( (fun (se,ve) -> 
        let i2,chk2,comp2 = _2 in 
        let i5,chk5,comp5 = _5 in 
        let i7,chk7,comp7 = _7 in 
        let i = m i2 i7 in 
          expect_sort i2 "in test dedlaration:" V.SRLens (chk2 se);
          expect_sort i5 "in test dedlaration:" V.SString (chk5 se);
          expect_sort i7 "in test dedlaration:" V.SString (chk7 se);
          let l2 = V.get_rl (comp2 ve) i2 in 
          let s5 = V.get_s  (comp5 ve) i5 in 
          let s7 = V.get_s (comp7 ve) i7 in 
          let g = L.RLens.get l2 s5 in 
          let c = L.RLens.create l2 s7 in  
            if (RS.to_string g) <> (RS.to_string s7) then test_fail i s7 g;
            if (RS.to_string c) <> (RS.to_string s5) then test_fail i s5 c;
            _8 (se,ve)) )
# 1009 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'sort_spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env) in
    Obj.repr(
# 500 "rparser.mly"
      ( (fun (se,ve) -> 
        let i2,chk2,comp2 = _2 in
        let v2 = comp2 ve in 
        let s2 = chk2 se in 
          (match _4 with
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
          _5 (se,ve)) )
# 1052 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    Obj.repr(
# 536 "rparser.mly"
      ( (fun (se,ve) -> (se,ve)) )
# 1058 "rparser.ml"
               : Rvalue.s env * (Rvalue.t * bool) env -> Rvalue.s env * (Rvalue.t * bool) env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Info.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'param_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'opt_sort) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Info.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    Obj.repr(
# 543 "rparser.mly"
      ( let i2,x2 = _2 in 
        let i6,chk6,comp6 = _6 in 
        let i8,chk8,comp8 = _8 in 
        let i = m _1 i8 in 
          (i,
          (fun se -> chk8 (Renv.update se x2 ((mk_fun_chker i _3 _4 chk6) se))),
          (fun ve -> comp8 (Renv.update ve x2 (((mk_fun_comper i x2 _3 _4 comp6) ve), true)))) )
# 1078 "rparser.ml"
               : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    Obj.repr(
# 552 "rparser.mly"
      ( let i5,chk5,comp5 = _5 in 
        let ps = _2::_3 in 
        let i = m _1 i5 in 
          (i,
          (fun se -> (mk_fun_chker i ps None chk5) se),
          (fun ve -> ((mk_fun_comper i "anonymous function" ps None comp5) ve))) )
# 1094 "rparser.ml"
               : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'gpexp) in
    Obj.repr(
# 560 "rparser.mly"
      ( _1 )
# 1101 "rparser.ml"
               : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'composeexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'get) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 565 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i3,chk3,comp3 = _3 in                                           
        let i = m i1 i3 in 
          (i, 
          (fun se -> 
            expect_sort i1 "in get expression:" V.SRLens (chk1 se);
            expect_sort i3 "in get expression:" V.SString (chk3 se);
            V.SString),
          (fun ve -> 
            let l1 = V.get_rl (comp1 ve) i1 in 
            let s3 = V.get_s (comp3 ve) i3 in                                                  
              V.S (i,L.RLens.get l1 s3))) )
# 1121 "rparser.ml"
               : 'gpexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'composeexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'put) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 579 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i3,chk3,comp3 = _3 in 
        let i5,chk5,comp5 = _5 in 
        let i = m i1 i5 in 
          (i, 
          (fun se -> 
            expect_sort i1 "in put expression:" V.SRLens (chk1 se);
            expect_sort i3 "in put expression:" V.SString (chk3 se);
            expect_sort i5 "in put expression:" V.SString (chk5 se);
            V.SString),
          (fun ve ->                                                
            let l1 = V.get_rl (comp1 ve) i1 in 
            let s3 = V.get_s (comp3 ve) i3 in
            let s5 = V.get_s (comp5 ve) i5 in
              V.S (i,L.RLens.put l1 s3 s5))) )
# 1146 "rparser.ml"
               : 'gpexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'composeexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'create) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 596 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i3,chk3,comp3 = _3 in 
        let i = m i1 i3 in 
          (i, 
          (fun se -> 
            expect_sort i1 "in create expression:" V.SRLens (chk1 se);
            expect_sort i3 "in create expression:" V.SString (chk3 se);
            V.SString),
          (fun ve -> 
            let l1 = V.get_rl (comp1 ve) i1 in 
            let s2 = V.get_s (comp3 ve) i3 in                                                  
              V.S (i,L.RLens.create l1 s2))) )
# 1166 "rparser.ml"
               : 'gpexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'composeexp) in
    Obj.repr(
# 609 "rparser.mly"
      ( _1 )
# 1173 "rparser.ml"
               : 'gpexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'composeexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 614 "rparser.mly"
      ( do_binary_op "in compose expression" _1 _3 compose_merge )
# 1182 "rparser.ml"
               : 'composeexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 617 "rparser.mly"
      ( _1 )
# 1189 "rparser.ml"
               : 'composeexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'iexp) in
    Obj.repr(
# 622 "rparser.mly"
      ( do_binary_op "in union expression" _1 _3 union_merge )
# 1198 "rparser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mexp) in
    Obj.repr(
# 625 "rparser.mly"
      ( _1 )
# 1205 "rparser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'iexp) in
    Obj.repr(
# 629 "rparser.mly"
      ( do_binary_op "in union expression" _1 _3 union_merge )
# 1214 "rparser.ml"
               : 'bexp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iexp) in
    Obj.repr(
# 632 "rparser.mly"
      ( _1 )
# 1221 "rparser.ml"
               : 'bexp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'iexp) in
    Obj.repr(
# 637 "rparser.mly"
      ( do_binary_op "in minus expression" _1 _3 minus_merge )
# 1230 "rparser.ml"
               : 'mexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iexp) in
    Obj.repr(
# 640 "rparser.mly"
      ( _1 )
# 1237 "rparser.ml"
               : 'mexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'iexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 645 "rparser.mly"
      ( do_binary_op "in intersection expression" _1 _3 inter_merge )
# 1246 "rparser.ml"
               : 'iexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 648 "rparser.mly"
      ( _1 )
# 1253 "rparser.ml"
               : 'iexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexp) in
    Obj.repr(
# 654 "rparser.mly"
      ( do_binary_op "in swap expression" _1 _3 swap_merge )
# 1262 "rparser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cexp) in
    Obj.repr(
# 657 "rparser.mly"
      ( _1 )
# 1269 "rparser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 662 "rparser.mly"
      ( do_binary_op "in concat expression" _1 _3 concat_merge )
# 1278 "rparser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 665 "rparser.mly"
      ( _1 )
# 1285 "rparser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texp) in
    Obj.repr(
# 670 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i2,chk2,comp2 = _2 in 
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
              f1 i2 (comp2 ve))) )
# 1311 "rparser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'texp) in
    Obj.repr(
# 691 "rparser.mly"
      ( _1 )
# 1318 "rparser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'texp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rexp) in
    Obj.repr(
# 696 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i3,chk3,comp3 = _3 in 
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
              f2 i3 v3)) )
# 1340 "rparser.ml"
               : 'texp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rexp) in
    Obj.repr(
# 711 "rparser.mly"
      ( _1 )
# 1347 "rparser.ml"
               : 'texp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rep) in
    Obj.repr(
# 717 "rparser.mly"
      ( let i1,chk1,comp1 = _1 in 
        let i2,(min,maxo) = _2 in     
        let i = m i1 i2 in 
          (i,
          (fun se -> 
            let s1 = chk1 se in 
              match s1 with
                | V.SString | V.SRegexp -> V.SRegexp
                | V.SCanonizer -> V.SCanonizer
                | V.SDLens -> V.SDLens 
                | V.SKLens -> V.SKLens
                | V.SSLens -> V.SSLens
                | V.SRLens -> V.SRLens
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
                    V.R(i,L.rx_rep (V.get_r v1 i) (0,None))
                | V.SCanonizer -> 
                    V.CN(i,L.Canonizer.star i (V.get_cn v1 i))
                | V.SDLens -> 
                    V.DL(i,L.DLens.star i (V.get_dl v1 i))
                | V.SKLens -> 
                    V.KL(i,L.KLens.star i (V.get_kl v1 i))
                | V.SSLens -> 
                    V.SL(i,L.SLens.star i (V.get_sl v1 i))
                | V.SRLens -> 
                    V.RL(i,L.RLens.star i (V.get_rl v1 i))
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
                        Safelist.fold_left mk_union v0 (Safelist.tl (Array.to_list a)))) )
# 1422 "rparser.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 786 "rparser.mly"
      ( _1 )
# 1429 "rparser.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 791 "rparser.mly"
      ( let i2,x2 = _2 in 
        let i = m _1 _3 in 
          (i,
          (fun se -> 
            Util.format "FOUND MATCH %s@\n" (Info.string_of_t i);
            expect_sort i2 "in match expression:" V.SKLens (V.lookup i2 se x2);            
            V.SSLens),
          (fun ve ->             
            let v2 = fst (V.lookup i2 ve x2) in 
            let u = V.uid_of_t v2 in 
              V.SL(i,L.SLens.smatch i x2 u (V.get_kl v2 i2)))) )
# 1448 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t * string) in
    Obj.repr(
# 804 "rparser.mly"
      ( let i,x = _1 in 
          (i,
          (fun se -> V.lookup i se x),
          (fun ve -> 
            let v1,is_decl = V.lookup i ve x in 
              (match is_decl,V.sort_of_t v1 with
                | true,V.SRegexp ->                     
                    let r = V.get_r v1 i in 
                      V.R(i,L.rx_set_str r x)
                | _ -> v1))) )
# 1464 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t * string) in
    Obj.repr(
# 816 "rparser.mly"
      ( let i1,s1 = _1 in 
          (i1,
          (fun se -> V.SRegexp),
          (fun ve -> V.R(i1,L.rx_set (parse_cset s1)))) )
# 1474 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t * string) in
    Obj.repr(
# 822 "rparser.mly"
      ( let i1,s1 = _1 in 
          (i1,
          (fun se -> V.SRegexp),
          (fun ve -> V.R(i1,L.rx_negset (parse_cset s1)))) )
# 1484 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t * string) in
    Obj.repr(
# 828 "rparser.mly"
      ( let i,s = _1 in 
          (i, 
          (fun se -> V.SString),
          (fun ve -> V.S(i,RS.of_string s))) )
# 1494 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 834 "rparser.mly"
      ( let (_,se,ve) = _2 in (m _1 _3, se,ve) )
# 1503 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t * (Rvalue.s env -> Rvalue.s) * ((Rvalue.t * bool) env -> Rvalue.t)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 837 "rparser.mly"
      ( let (_,se,ve) = _2 in (m _1 _3, se,ve) )
# 1512 "rparser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'asort) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sort) in
    Obj.repr(
# 842 "rparser.mly"
      ( V.SFunction(_1,Some _3) )
# 1521 "rparser.ml"
               : 'sort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'asort) in
    Obj.repr(
# 845 "rparser.mly"
      ( _1 )
# 1528 "rparser.ml"
               : 'sort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 849 "rparser.mly"
      ( V.SString )
# 1535 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 852 "rparser.mly"
      ( V.SRegexp )
# 1542 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 855 "rparser.mly"
      ( V.SCanonizer )
# 1549 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 858 "rparser.mly"
      ( V.SDLens )
# 1556 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 861 "rparser.mly"
      ( V.SKLens )
# 1563 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 864 "rparser.mly"
      ( V.SSLens )
# 1570 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 867 "rparser.mly"
      ( V.SRLens )
# 1577 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sort) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 870 "rparser.mly"
      ( _2 )
# 1586 "rparser.ml"
               : 'asort))
; (fun __caml_parser_env ->
    Obj.repr(
# 874 "rparser.mly"
      ( None )
# 1592 "rparser.ml"
               : 'opt_sort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sort_spec) in
    Obj.repr(
# 877 "rparser.mly"
      ( Some _2 )
# 1600 "rparser.ml"
               : 'opt_sort))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 881 "rparser.mly"
      ( Misc.Left (None) )
# 1607 "rparser.ml"
               : 'sort_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sort) in
    Obj.repr(
# 884 "rparser.mly"
      ( Misc.Left (Some _1) )
# 1614 "rparser.ml"
               : 'sort_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qmark_or_exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qmark_or_exp) in
    Obj.repr(
# 887 "rparser.mly"
      ( Misc.Right(_1,_3) )
# 1623 "rparser.ml"
               : 'sort_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 893 "rparser.mly"
      ( (* '?' is used when you do not want to type check
	   one of the the types. For example if you wrote a 
	   definition of the abstract domain and want to verifiy
	   that your different lenses go to this abstract domain 
	   but you don't want to explicitly write the definition of
	   all the concrete domains *)
	let i = _1 in (i, None))
# 1636 "rparser.ml"
               : 'qmark_or_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rexp) in
    Obj.repr(
# 902 "rparser.mly"
      ( let i1,chk2,comp1 = _1 in 
          (i1, Some (fun ve -> comp1 ve)) )
# 1644 "rparser.ml"
               : 'qmark_or_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 908 "rparser.mly"
      ( [] )
# 1650 "rparser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 910 "rparser.mly"
      ( _1 :: _2 )
# 1658 "rparser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'sort) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 914 "rparser.mly"
      ( let i,x = _2 in (i,x,_4) )
# 1669 "rparser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 920 "rparser.mly"
      ( (_1, (0, None)) )
# 1676 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 923 "rparser.mly"
      ( (_1, (1, None)) )
# 1683 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 926 "rparser.mly"
      ( (_1, (0,Some 1)) )
# 1690 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Info.t * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 929 "rparser.mly"
      ( let i = m _1 _3 in let _,n = _2 in (i, (n,Some n)) )
# 1699 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Info.t * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Info.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 932 "rparser.mly"
      ( let i = m _1 _3 in let _,n = _2 in (i, (n,None)) )
# 1709 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Info.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Info.t * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Info.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Info.t * int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 935 "rparser.mly"
      ( let i = m _1 _5 in let _,n2 = _2 in let _,n4 = _4 in (i, (n2, Some n4)) )
# 1720 "rparser.ml"
               : 'rep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 940 "rparser.mly"
      ( _1 )
# 1727 "rparser.ml"
               : 'eq_arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 943 "rparser.mly"
      ( _1 )
# 1734 "rparser.ml"
               : 'eq_arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 947 "rparser.mly"
      ( _1 )
# 1741 "rparser.ml"
               : 'eq_darrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 950 "rparser.mly"
      ( _1 )
# 1748 "rparser.ml"
               : 'eq_darrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 954 "rparser.mly"
      ( _1 )
# 1755 "rparser.ml"
               : 'get))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 957 "rparser.mly"
      ( _1 )
# 1762 "rparser.ml"
               : 'get))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 961 "rparser.mly"
      ( _1 )
# 1769 "rparser.ml"
               : 'put))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 964 "rparser.mly"
      ( _1 )
# 1776 "rparser.ml"
               : 'put))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 968 "rparser.mly"
      ( _1 )
# 1783 "rparser.ml"
               : 'create))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Info.t) in
    Obj.repr(
# 971 "rparser.mly"
      ( _1 )
# 1790 "rparser.ml"
               : 'create))
(* Entry top *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let top (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t -> Rvalue.s Rvalue.Renv.t * (Rvalue.t * bool) Rvalue.Renv.t)
