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

let mp2 i1 p2 = m i1 (info_of_pat p2)
let mp p1 p2 = m (info_of_pat p1) (info_of_pat p2)

(* helpers for building ASTs *)
let mk_qid_var x = EVar(info_of_qid x,x,None)
let mk_var x = mk_qid_var (qid_of_id x)
let mk_prelude_var x = mk_qid_var (mk_native_prelude_qid x)

let mk_str i s = EString(i,RS.t_of_string s)
let mk_bin_op i o e1 e2 = EApp(i,EApp(i,o,e1,None),e2,None) 
let mk_cat i e1 e2 = mk_bin_op i (mk_prelude_var "poly_concat") e1 e2
let mk_iter i e1 min maxo =  
  EApp(i,
    EApp(i,
      EApp(i,
        mk_prelude_var "poly_iter",
        e1,
        None),
      mk_str i (string_of_int min),
      None),
    mk_str i (match maxo with None -> "" | Some max -> string_of_int max),
    None)

let mk_star i e1 = mk_iter i e1 0 None

let mk_union i e1 e2 = mk_bin_op i (mk_prelude_var "poly_union") e1 e2
let mk_diff i e1 e2 = mk_bin_op i (mk_prelude_var "diff") e1 e2
let mk_inter i e1 e2 = mk_bin_op i (mk_prelude_var "inter") e1 e2
let mk_compose i e1 e2 = mk_bin_op i (mk_prelude_var "compose") e1 e2
let mk_swap i e1 e2 = mk_bin_op i (mk_prelude_var "poly_swap") e1 e2
let mk_set i e1 e2 = mk_bin_op i (mk_qid_var (mk_prelude_qid "set")) e1 e2
let mk_match i x q = mk_bin_op i (mk_qid_var (mk_prelude_qid "dmatch")) (EString(i,RS.t_of_string x)) (mk_qid_var q)
let mk_sim_match i t q = mk_bin_op i (mk_qid_var (mk_prelude_qid "smatch")) (EString(i,RS.t_of_string (string_of_float t))) (mk_qid_var q)
let mk_rx i e = EApp(i,mk_prelude_var "str",e,None)

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

let parse_qid i qstr = 
  let err () = raise (Parsing.Parse_error) in 
  let j = ref 0 in
  let l = String.length qstr in
  let eos () = !j = l in
  let get () = let r = qstr.[!j] in incr j; r in 
  let next () = if eos () then err () else get () in 
  let rec go (acc,x) = 
    if eos () then (Safelist.rev acc,(i,x))
    else 
      let c1 = next () in 
      if c1 = '.' then go ((i,x)::acc,"")
      else go (acc,x ^ (String.make 1 c1)) in 
  go ([],"")

let mk_fun i params body sorto = 
  Safelist.fold_right
    (fun p (f,so) -> 
       let f' = EFun(i,p,so,f,None) in 
       let so' = match so with 
         | None -> None 
         | Some s -> Some (SFunction(sort_of_param p,s)) in 
       (f',so'))
    params (body,sorto)

let mk_assert = function
  | None,None -> (fun i e -> e)
  | Some c,None -> 
      (fun i e -> 
         EApp(i,EApp(i,EVar(i,mk_native_prelude_qid "assert_ctype",None),c,None),e,None))
  | None,Some a -> 
      (fun i e -> 
         EApp(i,EApp(i,EVar(i,mk_native_prelude_qid "assert_atype",None),a,None),e,None))
  | Some c, Some a -> 
      (fun i e -> 
         EApp(i,EApp(i,EApp(i,EVar(i,mk_native_prelude_qid "assert",None),c,None),a,None),e,None))

let rec fixup_pat i = function
  | PVnt(_,_,Some _) -> syntax_error i "illegal pattern"
  | PVnt(i,x,None) -> PVar(i,x,None)
  | PPar(i,p1,p2)  -> PPar(i,fixup_pat i p1, fixup_pat i p2)
  | p -> p 


let check_pat i p params = match p,params with 
  | PVar _,_ -> ()
  | _,_::_ -> syntax_error i "illegal pattern" 
  | _ -> ()  

%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> STRING REGEXP LENS CANONIZER UNIT
%token <Info.t * string> STR RXSTR UIDENT LIDENT QIDENT VIDENT CSET NSET
%token <Info.t * int> INT
%token <Info.t * float> FLOAT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE   
%token <Info.t> ARROW DARROW EQARROW
%token <Info.t> BEGIN END FUN LET IN TEST MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH SLASH
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS UNDERLINE HAT TILDE AMPERSAND QMARK 
%token <Info.t> GET PUT CREATE INTO
%token <Info.t> ERROR

%start modl uid qid
%type <Bsyntax.modl> modl
%type <Bsyntax.id> uid
%type <Bsyntax.qid> qid
%%

/* --------- MODULES ---------- */
modl: 
  | MODULE UIDENT EQUAL opens decls EOF
      { Mod(m $1 $6,$2,$4,$5) }
opens:
  | OPEN UIDENT opens  
      { $2::$3 }
  | { [] }

/* --------- DECLARATIONS ---------- */
decls:      
  | TYPE svar_list LIDENT EQUAL dtsort_list decls
      { let i = m $1 $4 in 
        DType(i,$2,$3,$5)::$6 }

  | LET ppat param_list EQUAL exp decls
      { let i = me2 $1 $5 in 
        let p = fixup_pat i $2 in
        let () = check_pat i p $3 in 
        let f,so = mk_fun i $3 $5 None in 
        DLet(i,Bind(i,p,so,f))::$6 }

  | LET ppat param_list COLON decl_sort EQUAL exp decls
      { let i = me2 $1 $7 in 
        let p = fixup_pat i $2 in 
        let () = check_pat i p $3 in 
        let s,ef = $5 in 
        let f,so = mk_fun i $3 (ef $4 $7) (Some s) in 
        DLet(i,Bind(i,p,so,f))::$8 }

  | MODULE UIDENT EQUAL decls END decls 
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
  | LET ppat param_list EQUAL exp IN exp
      { let i = m $1 $6 in 
        let p = fixup_pat i $2 in 
        let () = check_pat i p $3 in 
        let f,so = mk_fun i $3 $5 None in 
        ELet(i,Bind(i,p,so,f),$7,None) 
      }

  | LET ppat param_list COLON decl_sort EQUAL exp IN exp
      { let i = m $1 $8 in 
        let p = fixup_pat i $2 in 
        let () = check_pat i p $3 in 
        let s,ef = $5 in 
        let f,so = mk_fun i $3 (ef $4 $7) (Some s) in 
        ELet(i,Bind(i,p,so,f),$9,None) 
      }

  | FUN param param_list ARROW exp
      { let i = me2 $1 $5 in 
        let f,_ = mk_fun i $3 $5 None in 
        EFun(i,$2,None,f,None) }

  | FUN param param_list COLON asort ARROW exp
      { let i = me2 $1 $7 in 
        let f,so = mk_fun i $3 $7 (Some $5) in 
        EFun(i,$2,so,f,None) }
      
  | gpexp                               
      { $1 }

/* "get put" expressions -- snipped JNF*/
gpexp: 
  | cexp GET aexp
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_native_prelude_qid "get",None), $1,None), $3,None) }
  | cexp PUT aexp INTO aexp        
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EApp(i, EVar(i, mk_native_prelude_qid "put",None), $1,None), $3,None), $5,None) }
  | cexp CREATE aexp               
      { let i = me $1 $3 in 
        EApp(i, EApp(i, EVar(i, mk_native_prelude_qid "create",None), $1,None), $3,None) }
  | cexp
      { $1 } 

/* case expressions */
cexp:
  | MATCH composeexp WITH branch_list
      { let i4,pl = $4 in 
        ECase(m $1 i4,$2,pl,None) }

  | composeexp
      { $1 }

/* compose expressions */
composeexp:
  | composeexp SEMI pexp
      { mk_compose (me $1 $3) $1 $3 }
      
  | pexp
      { $1 }

/* product expressions */
pexp:
  | pexp COMMA bexp
      { EPair(me $1 $3, $1, $3,None) }

  | bexp
      { $1 }

/* bar expressions */
bexp:
  | bexp BAR mexp 
      { mk_union (me $1 $3) $1 $3 }

  | mexp
      { $1 }

/* minus expressions */
mexp:
  | mexp MINUS iexp
      { mk_diff (me $1 $3) $1 $3 }

  | iexp 
      { $1 }

/* inter expressions */
iexp:
  | iexp AMPERSAND sexp
      { mk_inter (me $1 $3) $1 $3 }

  | sexp 
      { $1 }

/* swap expressions */
sexp:
  | sexp TILDE catexp 
      { mk_swap (me $1 $3) $1 $3 }

  | catexp 
      { $1 }
  
/* concat expressions */
catexp:
  | catexp DOT texp                     
      { mk_cat (me $1 $3) $1 $3 }

  | texp                              
      { $1 }

/* set expressions */
texp:
  | texp DARROW appexp
      { mk_set (me $1 $3) $1 $3 }
  | appexp                                
      { $1 }

/* application expressions */
appexp:
  | appexp rexp                         
      { EApp(me $1 $2, $1, $2,None) } 

  | rexp
      { $1 }
      
/* repeated expressions */
rexp:
  | aexp rep                            
      { let i2,(min,maxo) = $2 in 
        let i = me1 $1 i2 in 
        mk_iter i $1 min maxo }

  | aexp                                
      { $1 }

/* atomic expressions */
aexp:
  | LANGLE qid RANGLE
      { mk_match (m $1 $3) "" $2 }

  | LANGLE LIDENT COLON qid RANGLE
      { mk_match (m $1 $5) (string_of_id $2) $4 }

  | LANGLE TILDE qid RANGLE 
      { mk_sim_match (m $1 $4) 1.0 $3 }

  | LANGLE TILDE LBRACK FLOAT RBRACK qid RANGLE 
      { let _,f = $4 in 
        mk_sim_match (m $1 $7) f $6 }

  | qid 
      { EVar(info_of_qid $1,$1,None) }

  | CSET                                
      { let i1,s1 = $1 in 
        ECSet(i1,true,parse_cset s1) }

  | NSET                                
      { let i1,s1 = $1 in 
        ECSet(i1,false,parse_cset s1) }

  | STR 
      { let i,s = $1 in 
        mk_str i s }

  | RXSTR
      { let i,s = $1 in 
        mk_rx i (mk_str i s) }

  | LPAREN RPAREN
      { EUnit(m $1 $2) }
      
  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

/* --------- PATTERNS ---------- */
branch: 
  | pat ARROW mexp 
      { let i = m (info_of_pat $1) (info_of_exp $3) in 
        (i,$1,$3) }

pat:      
  | UIDENT ppat
      { let i1,_ = $1 in 
        let i = mp2 i1 $2 in 
        PVnt(i, qid_of_id $1,Some $2) }

  | QIDENT ppat
      { let (i,qs) = $1 in 
        PVnt(i,parse_qid i qs,Some $2) }

  | ppat
      { $1 }

ppat:
  | ppat COMMA apat
      { let i = mp $1 $3 in 
        PPar(i,$1,$3) }

  | apat
      { $1 }

apat:
  | UNDERLINE 
      { PWld $1 }

  | LPAREN RPAREN
      { PUnt (m $1 $2) }

  | UIDENT
      { let i,_ = $1 in 
        PVnt(i,qid_of_id $1,None) }
      
  | LIDENT
      { let i,_ = $1 in 
        PVar(i,qid_of_id $1,None) }

  | QIDENT
      { let (i,qs) = $1 in 
        PVnt(i,parse_qid i qs,None) }

  | LPAREN pat RPAREN
      { $2 }
      
branch_list:
  | branch branch_list2
      { let (i1,p,e) = $1 in 
        let (i2,l) = $2 i1 in 
        (m i1 i2, (p,e)::l) }

  | BAR branch branch_list2
      { let (i1,p,e) = $2 in 
        let (i2,l) = $3 i1 in 
        (m $1 i2, (p,e)::l) }

branch_list2:
  | 
      { (fun i -> (i,[])) }
        
  | BAR branch branch_list2
      { let (i1,p,e) = $2 in 
        let (i2,l) = $3 i1 in 
        (fun _ -> (m $1 i2, (p,e)::l)) }

/* --------- SORTS ---------- */
sort: 
  | psort ARROW sort
      { SFunction($1,$3) }

  | psort
      { $1 }

/* product sorts */
psort:
  | psort STAR dsort
      { SProduct($1,$3) }

  | dsort
      { $1 }

/* data type sorts */
dsort:
  | LIDENT
      { SData([], qid_of_id $1) }

  | asort LIDENT
      { SData([$1], qid_of_id $2) }

  | LPAREN sort RPAREN LIDENT 
      { SData([$2], qid_of_id $4) }

  | LPAREN sort COMMA sort_list RPAREN LIDENT 
      { SData($2::$4, qid_of_id $6) }

  | QIDENT
      { let (i,qs) = $1 in 
        SData([], parse_qid i qs) }

  | asort QIDENT 
      { let (i,qs) = $2 in 
        SData([$1], parse_qid i qs) }

  | LPAREN sort RPAREN QIDENT
      { let (i,qs) = $4 in 
        SData([$2], parse_qid i qs) }

  | LPAREN sort COMMA sort_list RPAREN QIDENT 
      { let (i,qs) = $6 in 
        SData($2::$4, parse_qid i qs) }
      
  | bsort 
      { $1 }

bsort:
  | LPAREN sort RPAREN
      { $2 }

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

  | svar
      { $1 }

svar:
  | VIDENT
      { SRawVar $1 }

svar_list:
  | 
      { [] }

  | svar 
      { [$1] }

  | LPAREN svar_list2 RPAREN
      { $2 }

svar_list2:
  | svar
      { [$1] }

  | svar COMMA svar_list2
      { $1::$3 }

sort_list:
  | sort 
      { [$1] }

  | sort COMMA sort_list
      { $1 :: $3 }

cvar_list:
  | cvar
      { [$1] }

  | cvar COMMA cvar_list
      { $1 :: $3 }

cvar:
  | LBRACE VIDENT COLON sort_list RBRACE 
      { ($2,$4) }

/* sort test specifications */
decl_sort:
  | cvar_list EQARROW sort 
      { ($3, (fun i e -> e)) }

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
  | UIDENT
      { ($1,None) }

  | UIDENT OF sort
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
  | LIDENT
      { qid_of_id $1 }
  | UIDENT
      { qid_of_id $1 }
  | QIDENT
      { let (i,qs) = $1 in parse_qid i qs }

id:
  | LIDENT
      { $1 }
  | UIDENT 
      { $1 }

/* --------- PARAMETERS ---------- */
param_list:
  | param param_list
      { $1 :: $2 }

  |
      { [] }

param: 
  | id 
      { Param (fst $1,$1,fresh_sort Fre) }

  | LPAREN id COLON sort RPAREN
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
uid:
  | UIDENT
      { $1 }

