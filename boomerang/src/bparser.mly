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
(* $Id$                                                                        *)
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
  Safelist.fold_left
    (fun so p -> match so with 
      | None -> None
      | Some s -> Some (SFunction(sort_of_param p,s)))
    bsorto
    params

%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN 
%token <Info.t> STRING REGEXP LENS
%token <Info.t * string> STR IDENT CSET NSET
%token <Info.t * int> INT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE LANGLEBAR BARRANGLE RANGLE   
%token <Info.t> ARROW DARROW LONGARROW LONGDARROW RANGLESLASH SLASHLANGLE
%token <Info.t> CREATE BEGIN END FUN LET IN TEST INTO SEMI COMMA DOT EQUAL COLON BACKSLASH  
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS HAT TILDE AMPERSAND QMARK 
%token <Info.t> MATCH WITH GET PUT DOTGET DOTPUT DOTCREATE ERROR

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
  | LET IDENT param_list opt_sort EQUAL exp decls
      { let i = me2 $1 $6 in 
        let f = mk_fun i $3 $6 in 
        let so = mk_fun_sorto i $3 $4 in 
        DLet(i,Bind(i,$2,so,f))::$7 }

  | MODULE IDENT EQUAL decls END decls 
      { DMod(m $1 $5,$2,$4)::$6 }

  | TEST exp eq_arrow test_res decls
      { let i,tr = $4 in 
        DTest(m $1 i,$2,tr)::$5 }
      
  | { [] }


/* --------- TEST RESULTS --------- */      
test_res:
  | QMARK
      { ($1,TestShow) }
  | ERROR
      { ($1,TestError) }
  | exp 
      { (info_of_exp $1, TestValue $1) }

/* --------- EXPRESSIONS ---------- */      
exp:
  | LET IDENT param_list opt_sort EQUAL exp IN exp
      { let i = m $1 $7 in 
        let f = mk_fun i $3 $6 in 
        let so = mk_fun_sorto i $3 $4 in 
        ELet(i,Bind(i,$2,so,f),$8) }

  | FUN param param_list ARROW exp
      { let i = me2 $1 $5 in 
        let f = mk_fun i $3 $5 in 
        EFun(i,$2,None,f) }
      
  | gpexp                               
      { $1 }

/* "get put" expressions -- snipped JNF*/
gpexp: 
  | composeexp get aexp
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_prelude_qid "get"), $1), $3) }
  | composeexp put aexp INTO aexp        
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EApp(i, EVar(i, mk_prelude_qid "put"), $1), $3), $5) }
  | composeexp create aexp               
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_prelude_qid "create"), $1), $3) }
  | composeexp
      { $1 } 

/* compose expressions */
composeexp:
  | composeexp SEMI bexp                
      { ECompose(me $1 $3, $1, $3) }
      
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
  | iexp AMPERSAND cexp
      { EInter(me $1 $3, $1, $3) } 

  | cexp 
      { $1 }


/* swap expressions -- snipped JNF */

/* concat expressions */
cexp:
  | cexp DOT texp                     
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
                  else aux (mk_cat $1 vi, mk_union vi us) (pred i) in 
                let v1 = 
                  if m=0 then epsilon 
                  else mk_cats $1 $1 (pred m) in 
                let us = aux (v1,v1) (n-m) in 
                us }

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
      
  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

/* --------- SORTS ---------- */
sort: 
  | asort ARROW sort                    
      { SFunction($1,$3) }

  | asort                               
      { $1 }

opt_sort:
  | COLON sort 
      { Some $2 }
  | 
      { None }

asort:
  | STRING 
      { SString }

  | REGEXP 
      { SRegexp }

  | LENS  
      { SLens }

  | LPAREN sort RPAREN                  
      { $2 }

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

