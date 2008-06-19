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

(* imports and abbreviations *)
open Bsyntax
open Bident
module L = Blenses 
let sprintf = Printf.sprintf
let (@) = Safelist.append

(* helpers for merging parsing info *)
let m = Info.merge_inc 
let me1 e1 i2 = m (info_of_exp e1) i2
let me2 i1 e2 = m i1 (info_of_exp e2)
let me e1 e2 = m (info_of_exp e1) (info_of_exp e2) 
let mp2 i1 p2 = m i1 (info_of_pat p2)
let mp p1 p2 = m (info_of_pat p1) (info_of_pat p2)

(* helpers for building ASTs *)
let mk_qid_var x = EVar(Qid.info_of_t x,x)
let mk_var x = mk_qid_var (Qid.t_of_id x)
let mk_native_prelude_var x = mk_qid_var (Qid.mk_native_prelude_t x)
let mk_core_var x = mk_qid_var (Qid.mk_core_t x)
let mk_list_var x = mk_qid_var (Qid.mk_list_t x)
let mk_over i op el = EOver(i,op,el)
let mk_app i e1 e2 = EApp(i,e1,e2)
let mk_bin_op i o e1 e2 = mk_app i (mk_app i o e1) e2
let mk_tern_op i o e1 e2 e3 = mk_app i (mk_bin_op i o e1 e2) e3
let mk_cat i e1 e2 = mk_over i ODot [e1;e2]
let mk_iter i min max e1 = mk_over i (OIter(min,max)) [e1]
let mk_acond i e1 e2 = mk_over i OBar [e1;e2]
let mk_cond i e1 e2 = mk_over i OBar [e1;e2]
let mk_swap i e1 e2 = mk_over i OTilde [e1;e2]
let mk_diff i e1 e2 = mk_bin_op i (mk_core_var "diff") e1 e2
let mk_inter i e1 e2 = mk_bin_op i (mk_core_var "inter") e1 e2
let mk_compose i e1 e2 = mk_bin_op i (mk_core_var "compose") e1 e2
let mk_set i e1 e2 = mk_bin_op i (mk_qid_var (Qid.mk_core_t "set")) e1 e2
let mk_match i x q =   
  mk_bin_op i 
    (mk_core_var "dmatch")
    (EString(i,x)) 
    (mk_qid_var q)
let mk_sim_match i e t q = 
  mk_tern_op i 
    (mk_core_var "smatch")
    (EString(i,string_of_float e))
    (EString(i,t))
    (mk_qid_var q)
let mk_rx i e = mk_app i (mk_core_var "str") e

(* error *)
let syntax_error i msg = 
  raise 
    (Error.Harmony_error
        (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
          (Info.string_of_t i)
          msg))

(* helper for parsing csets *)
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
          | '^' -> '^'
          | '-' -> '-'
          | 'b' -> '\008'
          | 'n' -> '\010'
          | 'r' -> '\013'
          | 't' -> '\009'
          | '\\' -> '\\'
          | _   -> err () 
      else (do_get ()) in 
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

(* helper for parsing qids *)
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

(* helper for building functions *)
let build_fun i param_alts body sort = 
  let f,_,f_sort = 
    Safelist.fold_right
      (fun pa (f,so,s) -> match pa with 
         | Misc.Left(p) -> 
             let f' = EFun(i,p,so,f) in 
             let s' = SFunction(Id.wild,sort_of_param p,s) in
             (f',None,s')
         | Misc.Right(a) -> 
             let f' = ETyFun(i,a,f) in 
             let s' = SForall(a,s) in 
             (f',None,s'))
      param_alts (body,Some sort,sort) in 
  (f,f_sort)

(* helper for building un-sorted functions *)
let build_bare_fun i param_alts body = 
  Safelist.fold_right
    (fun pa f -> 
       match pa with 
         | Misc.Left(p) -> 
             EFun(i,p,None,f)
         | Misc.Right(a) -> 
             ETyFun(i,a,f))
    param_alts body

let rec fixup_pat i p0 = match p0 with 
  | PVnt(_,x,Some _) -> syntax_error i "illegal pattern"
  | PVnt(i,x,None)   -> PVar(i,Qid.id_of_t x,None)
  | PPar(i,p1,p2)    -> PPar(i,fixup_pat i p1,fixup_pat i p2)
  | _ -> p0

%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> UNIT BOOL INT CHAR STRING REGEXP LENS CANONIZER FORALL WHERE
%token <Bident.Id.t> STR RXSTR UIDENT LIDENT QIDENT VIDENT CSET NSET
%token <Info.t * char> CHARACTER
%token <Info.t * int> INTEGER
%token <Info.t * bool> BOOLEAN
%token <Info.t * float> FLOAT
%token <Info.t> HASH LBRACE RBRACE LLIST LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE   
%token <Info.t> ARROW DARROW DEQARROW EQARROW
%token <Info.t> BEGIN END FUN LET IN TEST MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH SLASH
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS UNDERLINE HAT TILDE AMPERSAND QMARK 
%token <Info.t> LT GT LEQ GEQ  
%token <Info.t> CTYPE ATYPE BIJ GET PUT CREATE CANONIZE CHOOSE INTO
%token <Info.t> ERROR

%start modl uid qid
%type <Bsyntax.modl> modl
%type <Bident.Qid.t> uid
%type <Bident.Qid.t> qid
%%

/* --------- MODULES ---------- */
modl: 
  | MODULE UIDENT EQUAL opens decls EOF
      { Mod(m $1 $6,$2,$4,$5) }

opens:
  | OPEN qid opens  
      { $2::$3 }
  | { [] }

/* --------- DECLARATIONS ---------- */
decls:      
  | TYPE svar_list LIDENT EQUAL dtsort_list decls
      { let i = m $1 $4 in 
        DType(i,$2,Qid.t_of_id $3,$5)::$6 }

  | LET id param_list COLON sort EQUAL exp decls
      { let i = me2 $1 $7 in 
        let f,f_sort = build_fun i $3 $7 $5 in 
        let i2,_ = $2 in 
        let b = Bind(i,PVar(i2,$2,None),None,f) in 
        DLet(i,b)::$8 }

  | LET id param_list EQUAL exp decls
      { let i = me2 $1 $5 in 
        let f = build_bare_fun i $3 $5 in 
        let i2,_ = $2 in 
        let b =  Bind(i,PVar(i2,$2,None),None,f) in 
        DLet(i,b)::$6 }

  | LET ppat COLON sort EQUAL exp decls
      { let i = me2 $1 $6 in 
        let b = Bind(i,fixup_pat i $2,Some $4,$6) in 
        DLet(i,b)::$7 }

  | LET ppat EQUAL exp decls
      { let i = me2 $1 $4 in 
        let b =  Bind(i,fixup_pat i $2,None,$4) in 
        DLet(i,b)::$5 }

  | MODULE UIDENT EQUAL decls END decls 
      { let i = m $1 $5 in 
        DMod(i,$2,$4)::$6 }

  | TEST infixexp EQUAL test_res decls
      { let i4,tr = $4 in 
        let i = m $1 i4 in 
        DTest(i,$2,tr)::$5 }

  | TEST infixexp COLON test_type decls
      { let i = m $1 $3 in 
        DTest(i,$2,$4)::$5 }

  | TEST infixexp COLON ERROR decls 
      { let i = m $1 $4 in 
        DTest(i,$2,TestError)::$5 }
      
  | { [] }


/* --------- TEST RESULTS --------- */      
test_res:
  | QMARK
      { ($1,TestPrint) }
  | ERROR
      { ($1,TestError) }
  | appexp 
      { (info_of_exp $1, TestEqual $1) }

test_type:
  | QMARK
      { TestSortPrint None}

  | sort      
      { TestSortEqual $1 }

/* --------- EXPRESSIONS ---------- */      
exp:
  | LET id param_list COLON sort EQUAL exp IN exp 
      { let i = me2 $1 $9 in 
        let f,f_sort = build_fun i $3 $7 $5 in 
        let i2,_ = $2 in 
        let b = Bind(i,PVar(i2,$2,None),None,f) in 
        ELet(i,b,$9) }

  | LET id param_list EQUAL exp IN exp 
      { let i = me2 $1 $7 in 
        let f = build_bare_fun i $3 $5 in 
        let i2,_ = $2 in 
        let b = Bind(i,PVar(i2,$2,None),None,f) in 
        ELet(i,b,$7) }

  | LET ppat COLON sort EQUAL exp IN exp 
      { let i = me2 $1 $8 in 
        let b = Bind(i,fixup_pat i $2,Some $4,$6) in 
        ELet(i,b,$8) }

  | LET ppat EQUAL exp IN exp 
      { let i = me2 $1 $6 in 
        let b = Bind(i,fixup_pat i $2,None,$4) in 
        ELet(i,b,$6) }

  | FUN param_list ARROW exp
      { let i = me2 $1 $4 in 
        build_bare_fun i $2 $4 }

  | FUN param_list COLON asort ARROW exp
      { let i = me2 $1 $6 in 
        let f,_ = build_fun i $2 $6 $4 in 
        f }
      
  | cexp                               
      { $1 }


/* case expressions */
cexp:
  | MATCH composeexp WITH branch_list COLON sort
      { let i4,pl = $4 in 
        ECase(m $1 i4,$2,pl,$6) }

  | LPAREN MATCH composeexp WITH branch_list RPAREN COLON sort
      { let _,pl = $5 in 
        ECase(m $1 $6,$3,pl,$8) }

  | BEGIN MATCH composeexp WITH branch_list END COLON sort
      { let _,pl = $5 in 
        ECase(m $1 $6,$3,pl,$8) }

  | composeexp
      { $1 }

/* compose expressions */
composeexp:
  | composeexp SEMI barexp
      { mk_compose (me $1 $3) $1 $3 }
      
  | barexp
      { $1 }

/* bar expressions */
barexp:
  | barexp BAR equalexp
      { mk_over (me $1 $3) OBar [$1; $3] } 

  | barexp BAR BAR equalexp
      { mk_over (me $1 $4) OBarBar [$1; $4] } 

  | equalexp
      { $1 }

equalexp:
  | appexp EQUAL appexp 
      { mk_over (me $1 $3) OEqual [$1; $3] }
  | infixexp
      { $1 }

infixexp:
  | dotexp 
      { $1 }
  | tildeexp
      { $1 }
  | minusexp
      { $1 }
  | ampexp 
      { $1 }
  | ampampexp 
      { $1 }
  | darrowexp
      { $1 }
  | deqarrowexp
      { $1 }
  | ltexp 
      { $1 }
  | leqexp 
      { $1 }
  | gtexp 
      { $1 }
  | geqexp
      { $1 }
  | lcexp
      { $1 }
  | commaexp
      { $1 }
  | appexp
      { $1 }

dotexp:
  | dotexp DOT appexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | appexp DOT appexp
      { mk_over (me $1 $3) ODot [$1; $3] }
tildeexp:
  | tildeexp TILDE appexp
      { mk_over (me $1 $3) OTilde [$1; $3] }
  | appexp TILDE appexp
      { mk_over (me $1 $3) OTilde [$1; $3] }
minusexp:
  | appexp MINUS appexp
      { mk_over (me $1 $3) OMinus [$1; $3] }
ampexp:
  | ampexp AMPERSAND appexp 
      { mk_over (me $1 $3) OAmp [$1; $3] }
  | appexp AMPERSAND appexp 
      { mk_over (me $1 $3) OAmp [$1; $3] }
ampampexp:
  | appexp AMPERSAND AMPERSAND appexp 
      { mk_over (me $1 $4) OAmpAmp [$1; $4] }
darrowexp:
  | appexp DARROW appexp 
      { mk_over (me $1 $3) ODarrow [$1; $3] }
deqarrowexp:
  | appexp DEQARROW appexp
      { mk_over (me $1 $3) ODeqarrow [$1; $3] }
ltexp:
  | appexp LT appexp 
      { mk_over (me $1 $3) OLt [$1; $3] }
leqexp:
  | appexp LEQ appexp 
      { mk_over (me $1 $3) OLeq [$1; $3] }
gtexp:
  | appexp GT appexp 
      { mk_over (me $1 $3) OGt [$1; $3] }
geqexp:
  | appexp GEQ appexp 
      { mk_over (me $1 $3) OGeq [$1; $3] }
commaexp:
  | appexp COMMA appexp
      { EPair(me $1 $3, $1, $3) }
lcexp: 
  | appexp GET appexp
      { let i = me $1 $3 in 
        mk_bin_op i (mk_core_var "get") $1 $3 }
  | appexp PUT appexp INTO appexp
      { let i = me $1 $3 in
        mk_tern_op i (mk_core_var "put") $1 $3 $5 }
  | appexp CREATE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var "create") $1 $3 }
  | appexp CANONIZE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var "canonize") $1 $3 }
  | appexp CHOOSE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var "choose") $1 $3 }

/* application expressions */
appexp:
  | appexp rexp                         
      { mk_app (me $1 $2) $1 $2 }

  | rexp
      { $1 }
      
/* repeated expressions */
rexp:
  | tyexp rep                            
      { let i2,(min,max) = $2 in 
        let i = me1 $1 i2 in 
        mk_iter i min max $1 }

  | tyexp                                
      { $1 }

tyexp:
  | tyexp LBRACE sort RBRACE
      { let i = me1 $1 $4 in 
        ETyApp(i,$1,$3) }

  | aexp 
      { $1 }

/* atomic expressions */
aexp:
  | rexp CTYPE
      { let i = me1 $1 $2 in 
        mk_app i (mk_core_var "ctype") $1 }
  | rexp ATYPE
      { let i = me1 $1 $2 in 
        mk_app i (mk_core_var "atype") $1 }
  | rexp BIJ 
      { let i = me1 $1 $2 in 
        mk_app i (mk_core_var "bij") $1 }

  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

  | qid
      { mk_qid_var $1 }

  | LANGLE qid RANGLE
      { mk_match (m $1 $3) "" $2 }

  | LANGLE LIDENT COLON qid RANGLE
      { mk_match (m $1 $5) (Id.string_of_t $2) $4 }

  | LANGLE TILDE qid RANGLE 
      { mk_sim_match (m $1 $4) 1.0 "" $3 }

  | LANGLE TILDE LIDENT COLON qid RANGLE 
      { mk_sim_match (m $1 $6) 1.0 (Id.string_of_t $3) $5 }

  | LANGLE TILDE LBRACE FLOAT RBRACE qid RANGLE 
      { let _,f = $4 in 
        mk_sim_match (m $1 $7) f "" $6 }

  | LANGLE TILDE LBRACE FLOAT RBRACE LIDENT COLON qid RANGLE 
      { let _,f = $4 in 
        mk_sim_match (m $1 $9) f (Id.string_of_t $6) $8 }

  | HASH LBRACE sort LLIST list 
      { let i6,mk = $5 in 
        let i = m $1 i6 in 
        let l = mk i $3 in 
        l }

  | CHARACTER
      { let i,c = $1 in 
        EChar(i,c) }

  | INTEGER
      { let i,n = $1 in 
        EInteger(i,n) }

  | BOOLEAN
      { let i,b = $1 in 
        EBoolean(i,b) }

  | CSET                                
      { let i1,s1 = $1 in         
        ECSet(i1,true,parse_cset s1) }

  | NSET                                
      { let i1,s1 = $1 in 
        ECSet(i1,false,parse_cset s1) }

  | STR 
      { let i,s = $1 in 
        EString(i,s) }

  | RXSTR
      { let i,s = $1 in 
        mk_rx i (EString(i,s)) }

  | LPAREN RPAREN
      { EUnit(m $1 $2) }

/* --------- LISTS ------------ */
list:
  | RBRACK 
      { $1, (fun i s -> ETyApp(i,mk_list_var "Nil",s)) }

  | barexp RBRACK 
      { ($2, 
         (fun i s -> 
            mk_app i 
              (ETyApp(i,mk_list_var "Cons",s))
              (EPair(i,$1,ETyApp(i,mk_list_var "Nil",s))))) }

  | barexp SEMI list
    { let i3,mk = $3 in 
      (i3, 
       (fun i s -> 
          mk_app i
            (ETyApp(i,mk_list_var "Cons",s))
            (EPair(i,$1, mk i s)))) }

/* --------- PATTERNS ---------- */
branch: 
  | pat ARROW infixexp 
      { let i = m (info_of_pat $1) (info_of_exp $3) in 
        (i,$1,$3) }

pat:      
  | UIDENT ppat
      { let i1,_ = $1 in 
        let i = mp2 i1 $2 in 
         PVnt(i,Qid.t_of_id $1,Some $2) }

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
    { PWld($1) }

  | LIDENT
      { let i,_ = $1 in 
        PVar(i,$1,None) }

  | LPAREN RPAREN
      { PUnt(m $1 $2) }

  | INTEGER
    { let i,n = $1 in 
       PInt(i,n) }

  | BOOLEAN
      { let i,b = $1 in 
        PBol(i,b) }

  | STR
      { let i,s = $1 in 
        PStr(i,s) }

  | UIDENT
      { let i,_ = $1 in 
        PVnt(i,Qid.t_of_id $1,None) }
      
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
/* universal sorts */
sort:
  | FORALL VIDENT EQARROW sort 
      { SForall($2,$4) }

  | arrsort
      { $1 }

/* function sorts */
arrsort: 
  | psort ARROW arrsort 
      { SFunction(Id.wild,$1,$3) } 

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
  | bsort qvar
      { SData([$1],$2) }

  | LPAREN sort COMMA sort_list RPAREN qvar 
      { SData($2::$4, $6) }

  | bsort 
      { $1 }

bsort:
  | LPAREN sort RPAREN
      { $2 }

  | asort 
      { $1 }

/* atomic sorts */
asort:
  | qvar
      { SData([], $1) }

  | CHAR
      { SChar }

  | STRING 
      { SString }

  | REGEXP 
      { SRegexp }

  | LENS  
      { SLens }

  | INT
      { SInteger }

  | BOOL
      { SBool }

  | CANONIZER
      { SCanonizer }

  | UNIT
      { SUnit }

  | svar
      { SVar $1 }  

svar:
  | VIDENT
      { $1 }

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
      { Qid.t_of_id $1 }
  | UIDENT
      { Qid.t_of_id $1 }
  | QIDENT
      { let (i,qs) = $1 in parse_qid i qs }

qvar:
  | LIDENT
      { Qid.t_of_id $1 }

  | QIDENT
      { let (i,qs) = $1 in parse_qid i qs }

id:
  | LIDENT
      { $1 }
  | UIDENT 
      { $1 }

/* --------- PARAMETERS ---------- */
param_list:
  | param param_list2
      { $1 :: $2 }
param_list2:
  | param param_list2
      { $1 :: $2 }
  |
      { [] }

param: 
  | LPAREN id COLON sort RPAREN
      { let i = m $1 $5 in 
        Misc.Left (Param(i,$2,$4)) }

  | LPAREN VIDENT RPAREN
      { Misc.Right ($2) }

  | VIDENT 
      { Misc.Right ($1) }

/* --------- REPETITIONS ---------- */
rep: 
  | STAR                                
      { ($1, (0,-1)) }

  | PLUS                                
      { ($1, (1,-1)) }

  | QMARK                               
      { ($1, (0,1)) }

  | LBRACE INTEGER RBRACE
      { let i = m $1 $3 in let _,n = $2 in (i, (n,n)) }

  | LBRACE INTEGER COMMA RBRACE             
      { let i = m $1 $3 in let _,n = $2 in (i, (n,-1)) }

  | LBRACE INTEGER COMMA INTEGER RBRACE         
      { let i = m $1 $5 in let _,n2 = $2 in let _,n4 = $4 in (i, (n2, n4)) }

/* --------- MISC SYMBOLS ---------- */
uid:
  | UIDENT
      { Qid.t_of_id $1 }
