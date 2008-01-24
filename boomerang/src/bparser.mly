%{ 
(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/parser.mly                                                   *)
(* Boomerang parser                                                            *)
(* $Id$ *)
(*******************************************************************************)

(* ----- module and type imports and abbreviations ----- *)
module RS = Bstring
module L = Blenses 
open Bsyntax

let sprintf = Printf.sprintf
let (@) = Safelist.append

(* ----- helper functions for extracting / merging Info.t ------ *)

(* m: Info.t -> Info.t -> Info.t
 * 
 * [m i1 i2] merges the parsing info [i1] and [i2] into a single
 * parsing info representing the range spanned by [i1] and [i2].
 *)
let m = Info.merge_inc 
let me1 e1 i2 = m (info_of_exp e1) i2
let me2 i1 e2 = m i1 (info_of_exp e2)
let me e1 e2 = m (info_of_exp e1) (info_of_exp e2) 

let mk_var i = EVar(info_of_id i,qid_of_id i)

(* error *)
let syntax_error i msg = 
  raise 
    (Error.Harmony_error
        (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
          (Info.string_of_t i)
          msg))

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
          | '^' -> (RS.sym_of_char '^')
          | '-' -> (RS.sym_of_char '-')
          | 'b' -> (RS.sym_of_char '\008')
          | 'n' -> (RS.sym_of_char '\010')
          | 'r' -> (RS.sym_of_char '\013')
          | 't' -> (RS.sym_of_char '\009')
          | '\\' -> (RS.sym_of_char '\\')
          | _   -> err () 
      else RS.sym_of_char (do_get ()) in 
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
      
let mk_fun i params body = 
  Safelist.fold_right
    (fun p f -> (EFun(i,p,None,f)))
    params body 
    
let mk_fun_sorto i params bsorto = 
  Safelist.fold_right
    (fun p so -> match so with 
      | None -> None
      | Some s -> Some (SFunction(sort_of_param p,s)))
    params
    bsorto

let mk_assert = function
  | None,None -> (fun i e -> e)
  | Some c,None -> 
      (fun i e -> 
         EApp(i,EApp(i,EVar(i,mk_prelude_qid "assert_ctype"),c),e))
  | None,Some a -> 
      (fun i e -> 
         EApp(i,EApp(i,EVar(i,mk_prelude_qid "assert_atype"),a),e))
  | Some c, Some a -> 
      (fun i e -> 
         EApp(i,EApp(i,EApp(i,EVar(i,mk_prelude_qid "assert"),c),a),e))
%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> STRING REGEXP LENS CANONIZER UNIT
%token <Info.t * string> STR IDENT CSET NSET
%token <Info.t * int> INT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE LANGLEBAR BARRANGLE RANGLE   
%token <Info.t> ARROW DARROW LONGARROW LONGDARROW RANGLESLASH SLASHLANGLE
%token <Info.t> CREATE BEGIN END FUN LET IN TEST INTO MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH  
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS HAT TILDE AMPERSAND QMARK 
%token <Info.t> GET PUT DOTGET DOTPUT DOTCREATE ERROR

%start modl qid
%type <Bsyntax.modl> modl
%type <Bsyntax.qid> qid
%%

/* --------- MODULES ---------- */
modl: 
  | MODULE IDENT EQUAL opens decls EOF
      { Mod(m $1 $6,$2,$4,$5) }
opens:
  | OPEN qid opens  
      { $2::$3 }
  | { [] }

/* --------- DECLARATIONS ---------- */
decls:      
  | TYPE IDENT EQUAL dtsort_list decls
      { let i = m $1 $3 in 
        DType(i,$2,$4)::$5 }

  | LET IDENT param_list EQUAL exp decls
      { let i = me2 $1 $5 in 
        let f = mk_fun i $3 $5 in 
        let so = mk_fun_sorto i $3 None in 
        DLet(i,Bind(i,$2,so,f))::$6 }

  | LET IDENT param_list COLON decl_sort EQUAL exp decls
      { let i = me2 $1 $7 in 
        let s,ef = $5 in 
        let f = mk_fun i $3 (ef $4 $7) in 
        let so = mk_fun_sorto i $3 (Some s) in 
        DLet(i,Bind(i,$2,so,f))::$8 }

  | MODULE IDENT EQUAL decls END decls 
      { DMod(m $1 $5,$2,$4)::$6 }

  | TEST exp EQUAL test_res decls
      { let i,tr = $4 in 
        DTest(m $1 i,$2,tr)::$5 }

  | TEST exp COLON test_type decls
      { DTest(m $1 $3,$2,$4)::$5 }

  | TEST exp COLON ERROR decls 
      { DTest(m $1 $4,$2,TestError)::$5 }
      
  | { [] }


/* --------- TEST RESULTS --------- */      
test_res:
  | QMARK
      { ($1,TestShow) }
  | ERROR
      { ($1,TestError) }
  | exp 
      { (info_of_exp $1, TestValue $1) }

test_type:
  | QMARK 
      { TestSort None }
      
  | sort 
      { TestSort (Some $1) }
      
  | lens_type 
      { TestLensType $1 }

/* --------- EXPRESSIONS ---------- */      
exp:
  | LET IDENT param_list EQUAL exp IN exp
      { let i = m $1 $6 in 
        let f = mk_fun i $3 $5 in 
        let so = mk_fun_sorto i $3 None in 
        ELet(i,Bind(i,$2,so,f),$7) 
      }

  | LET IDENT param_list COLON decl_sort EQUAL exp IN exp
      { let i = m $1 $8 in 
        let s,ef = $5 in 
        let f = mk_fun i $3 (ef $4 $7) in 
        let so = mk_fun_sorto i $3 (Some s) in 
        ELet(i,Bind(i,$2,so,f),$9) 
      }

  | FUN param param_list ARROW exp
      { let i = me2 $1 $5 in 
        let f = mk_fun i $3 $5 in 
        EFun(i,$2,None,f) }
      
  | gpexp                               
      { $1 }

/* "get put" expressions -- snipped JNF*/
gpexp: 
  | cexp get aexp
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_prelude_qid "get"), $1), $3) }
  | cexp put aexp INTO aexp        
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EApp(i, EVar(i, mk_prelude_qid "put"), $1), $3), $5) }
  | cexp create aexp               
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_prelude_qid "create"), $1), $3) }
  | cexp
      { $1 } 

/* case expressions */
cexp:
  | MATCH composeexp WITH pat_list
      { ECase(m $1 $3,$2,$4) }

  | composeexp
      { $1 }

/* compose expressions */
composeexp:
  | composeexp SEMI pexp
      { ECompose(me $1 $3, $1, $3) }
      
  | pexp
      { $1 }

/* product expressions */
pexp:
  | pexp COMMA bexp
      { EPair(me $1 $3, $1, $3) }

  | bexp
      { $1 }
            
/* bar expressions */
bexp:
  | bexp BAR iexp 
      { EUnion(me $1 $3,$1,$3) }

  | mexp
      { $1 }

/* minus expressions */
mexp:
  | mexp MINUS iexp
      { EDiff(me $1 $3,$1,$3) }

  | iexp 
      { $1 }

/* inter expressions */
iexp:
  | iexp AMPERSAND sexp
      { EInter(me $1 $3, $1, $3) } 

  | sexp 
      { $1 }

/* swap expressions */
sexp:
  | sexp TILDE catexp
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_prelude_qid "swap"), $1), $3) }

  | catexp 
      { $1 }

/* concat expressions */
catexp:
  | catexp DOT texp                     
      { ECat(me $1 $3, $1, $3) } 

  | texp                              
      { $1 }

/* translate expressions */
texp:
  | texp DARROW appexp
      { ETrans(me $1 $3, $1, $3) }
  | appexp                                
      { $1 }

/* application expressions */
appexp:
  | appexp rexp                         
      { EApp(me $1 $2, $1, $2) } 

  | rexp
      { $1 }
      
/* repeated expressions */
rexp:
  | aexp rep                            
      { let i2,(min,maxo) = $2 in 
        let i = me1 $1 i2 in 
        let mk_cat e1 e2 = ECat(i,e1,e2) in 
        let mk_union e1 e2 = EUnion(i,e1,e2) in 
        let mk_star e1 = EStar(i,e1) in
        let epsilon = EString(i,RS.empty) in 
        let rec mk_cats l acc = function
          | 0 -> acc
          | 1 -> mk_cat l acc
          | n -> mk_cats l (mk_cat l acc) (pred n) in 
        match min,maxo with 
          | 0,None -> EStar(i,$1)
          | n,None -> mk_cats $1 (mk_star $1) n
          | 0,Some 0 -> epsilon 
          | 0,Some 1 -> mk_union epsilon $1
          | m,Some n -> 
              if m > n then
                syntax_error i 
                  (sprintf "error in repetition %d > %d" m n)
              else if m=n then 
                mk_cats $1 $1 (pred m)
              else (* n > m *)
                let rec aux (vi,us) i = 
                  if i=0 then us
                  else 
                    let vi1 = mk_cat $1 vi in 
                    aux (vi1, mk_union us vi1) (pred i) in 
                let v1 = 
                  if m=0 then epsilon
                  else mk_cats $1 $1 (pred m) in 
                aux (v1,v1) (n-m) }

  | aexp                                
      { $1 }

/* atomic expressions */
aexp:
  | LANGLE qid RANGLE
      { EMatch(m $1 $3,RS.empty, $2) }

  | LANGLE IDENT COLON qid RANGLE
      { EMatch(m $1 $3,RS.t_of_string (snd $2), $4) }

  | IDENT                               
      { mk_var $1 }

  | CSET                                
      { let i1,s1 = $1 in 
        ECSet(i1,true,parse_cset s1) }

  | NSET                                
      { let i1,s1 = $1 in 
        ECSet(i1,false,parse_cset s1) }

  | STR 
      { let i,s = $1 in 
        EString(i,RS.t_of_string s) }

  | LPAREN RPAREN
      { EUnit(m $1 $2) }
      
  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

/* --------- PATTERNS ---------- */
pat: 
  | IDENT IDENT ARROW aexp 
      { PVnt($1,Some $2),$4 }

  | IDENT ARROW aexp 
      { PVnt($1,None),$3 }

  | IDENT COMMA IDENT ARROW aexp 
      { PPar($1,$3),$5 }

  | LPAREN IDENT COMMA IDENT RPAREN ARROW aexp 
      { PPar($2,$4),$7 }

pat_list:
  | opt_bar pat pat_list2
      { $2::$3 }

pat_list2:
  | 
      { [] }

  | BAR pat pat_list2
      { $2::$3 }

opt_bar:
  | 
      { () }

  | BAR
      { () }

/* --------- SORTS ---------- */
sort: 
  | psort ARROW sort
      { SFunction($1,$3) }

  | psort
      { $1 }

/* product sorts */
psort:
  | psort STAR asort
      { SProduct($1,$3) }

  | asort
      { $1 }

/* atomic sorts */
asort:
  | STRING 
      { SString }

  | REGEXP 
      { SRegexp }

  | LENS  
      { SLens }

  | CANONIZER
      { SCanonizer }

  | UNIT
      { SUnit }

  | IDENT
      { SVar (qid_of_id $1) }
      
  | LPAREN sort RPAREN
      { $2 }

/* sort test specifications */
decl_sort:
  | sort 
      { ($1, (fun i e -> e)) }

  | lens_type
      { (SLens, mk_assert $1) }

/* lens types */
lens_type:
  | LBRACE qmark_or_exp DARROW qmark_or_exp RBRACE
      { ($2,$4) }

qmark_or_exp:
   | QMARK
       { None }

   | appexp
       { Some $1 }

/* data type sorts */
dtsort:
  | IDENT
      { ($1,None) }

  | IDENT OF sort
      { ($1,Some $3) }

dtsort_list:
  | dtsort dtsort_list2
      { $1 :: $2 }

dtsort_list2:
  | 
      { [] }
  
  | BAR dtsort dtsort_list2
      { $2 :: $3 }

/* --------- QUALIFIED IDENTIFIERS ---------- */
qid:
  | IDENT                                    { qid_of_id ($1) }
  | qid DOT IDENT                            { qid_dot $1 (qid_of_id $3) }

/* --------- PARAMETERS ---------- */
param_list:
  | param param_list
      { $1 :: $2 }

  | 
      { [] }

param: 
  | LPAREN IDENT COLON sort RPAREN
      { Param(fst $2,$2,$4) }

/* --------- REPETITIONS ---------- */
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

