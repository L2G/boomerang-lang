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
let mk_bool i b = mk_exp i (EBoolean b)
let mk_int i n = mk_exp i (EInteger n)
let mk_qid_var x = mk_exp (Qid.info_of_t x) (EVar x)
let mk_var x = mk_qid_var (Qid.t_of_id x)
let mk_prelude_var x = mk_qid_var (Qid.mk_native_prelude_t x)
let mk_list_var x = mk_qid_var (Qid.mk_list_t x)
let mk_over i op el = mk_exp i (EOver(op,el))

let mk_pair i e1 e2 = mk_exp i (EPair(e1,e2)) 
let mk_str i s = mk_exp i (EString (RS.t_of_string s))
let mk_app i e1 e2 = mk_exp i (EApp(e1,e2))
let mk_bin_op i o e1 e2 = mk_app i (mk_app i o e1) e2
let mk_tern_op i o e1 e2 e3 = mk_app i (mk_bin_op i o e1 e2) e3
let mk_cat i e1 e2 = mk_over i ODot [e1;e2]
let mk_iter i min max e1 = mk_over i (OIter(min,max)) [e1]

let mk_union i e1 e2 = mk_over i OBar [e1;e2]
let mk_swap i e1 e2 = mk_over i OTilde [e1;e2]
let mk_diff i e1 e2 = mk_bin_op i (mk_prelude_var "diff") e1 e2
let mk_inter i e1 e2 = mk_bin_op i (mk_prelude_var "inter") e1 e2
let mk_compose i e1 e2 = mk_bin_op i (mk_prelude_var "compose") e1 e2
let mk_set i e1 e2 = mk_bin_op i (mk_qid_var (Qid.mk_prelude_t "set")) e1 e2
let mk_match i x q = mk_bin_op i (mk_qid_var (Qid.mk_prelude_t "dmatch")) (mk_str i x) (mk_qid_var q)
let mk_sim_match i e t q = 
  mk_tern_op i (mk_prelude_var "smatch")
    (mk_str i (string_of_float e))
    (mk_str i t)
    (mk_qid_var q)
let mk_rx i e = mk_app i (mk_prelude_var "str") e
let mk_tyapp i e s = mk_exp i (ETyApp(e,s))

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

let build_fun i param_alts body sort = 
  Safelist.fold_right
    (fun pa (f,s) -> match pa with 
       | Misc.Left(p) -> 
           let f' = mk_exp i (EFun(p,Some s,f)) in 
           let s' = SFunction(id_of_param p,sort_of_param p,s) in          
           (f',s')
       | Misc.Right(a) -> 
           let f' = mk_exp i (ETyFun(a,f)) in 
           let s' = SForall(a,s) in 
           (f',s'))
    param_alts (body,sort)

let build_bare_fun i param_alts body = 
  Safelist.fold_right
    (fun pa f -> 
       match pa with 
         | Misc.Left(p) -> 
             mk_exp i (EFun(p,None,f))
         | Misc.Right(a) -> 
             mk_exp i (ETyFun(a,f)))
    param_alts body

let mk_assert = function
  | None,None -> (fun i e -> e)
  | Some c,None -> 
      (fun i e -> mk_bin_op i (mk_prelude_var "assert_ctype") c e)
  | None,Some a -> 
      (fun i e -> mk_bin_op i (mk_prelude_var "assert_atype") a e)
  | Some c, Some a -> 
      (fun i e -> mk_tern_op i (mk_prelude_var "assert") c a e)

let rec fixup_pat i p0 = match p0.desc with 
  | PVnt(_,Some _) -> syntax_error i "illegal pattern"
  | PVnt(x,None) -> mk_pat i (PVar (Qid.id_of_t x))
  | PPar(p1,p2)  -> mk_pat i (PPar(fixup_pat i p1, fixup_pat i p2))
  | _ -> p0

let check_pat i p params = match p.desc,params with 
  | PVar _,_ -> ()
  | _,_::_ -> syntax_error i "illegal pattern" 
  | _ -> ()  

%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> UNIT BOOL INT STRING REGEXP LENS CANONIZER FORALL WHERE
%token <Bsyntax.Id.t> STR RXSTR UIDENT LIDENT QIDENT VIDENT CSET NSET
%token <Info.t * int> INTEGER
%token <Info.t * bool> BOOLEAN
%token <Info.t * float> FLOAT
%token <Info.t> HASH LBRACE RBRACE LLIST LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE   
%token <Info.t> ARROW DARROW EQARROW
%token <Info.t> BEGIN END FUN LET IN TEST MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH SLASH
%token <Info.t> STAR RLUS BANG BAR PLUS MINUS UNDERLINE HAT TILDE AMPERSAND QMARK 
%token <Info.t> GET PUT CREATE INTO
%token <Info.t> ERROR

%start modl uid qid
%type <Bsyntax.modl> modl
%type <Bsyntax.Id.t> uid
%type <Bsyntax.Qid.t> qid
%%

/* --------- MODULES ---------- */
modl: 
  | MODULE UIDENT EQUAL opens decls EOF
      { mk_mod (m $1 $6) (Mod($2,$4,$5)) }
opens:
  | OPEN UIDENT opens  
      { $2::$3 }
  | { [] }

/* --------- DECLARATIONS ---------- */
decls:      
  | TYPE svar_list LIDENT EQUAL dtsort_list decls
      { let i = m $1 $4 in 
        mk_decl i (DType($2,Qid.t_of_id $3,$5))::$6 }

  | LET id param_list COLON sort EQUAL exp decls
      { let i = me2 $1 $7 in 
        let f,f_sort = build_fun i $3 $7 $5 in 
        let b = mk_binding i (Bind($2,Some f_sort,f)) in 
        mk_decl i (DLet b)::$8 }

  | LET id param_list EQUAL exp decls
      { let i = me2 $1 $5 in 
        let f = build_bare_fun i $3 $5 in 
        let b = mk_binding i (Bind($2,None,f)) in 
        mk_decl i (DLet b)::$6 }

  | MODULE UIDENT EQUAL decls END decls 
      { let i = m $1 $5 in 
        mk_decl i (DMod($2,$4))::$6 }

  | TEST exp EQUAL test_res decls
      { let i4,tr = $4 in 
        let i = m $1 i4 in 
        mk_decl i (DTest($2,tr))::$5 }

  | TEST exp COLON test_type decls
      { let i = m $1 $3 in 
        mk_decl i (DTest($2,$4))::$5 }

  | TEST exp COLON ERROR decls 
      { let i = m $1 $4 in 
        mk_decl i (DTest($2,TestError))::$5 }
      
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
  | lens_type 
      { TestLensType $1 }

/* --------- EXPRESSIONS ---------- */      
exp:
  | LET id param_list COLON sort EQUAL exp IN exp 
      { let i = me2 $1 $9 in 
        let f,f_sort = build_fun i $3 $7 $5 in 
        let b = mk_binding i (Bind($2,Some f_sort,f)) in 
        mk_exp i (ELet(b,$9)) }

  | LET id param_list EQUAL exp IN exp 
      { let i = me2 $1 $7 in 
        let f = build_bare_fun i $3 $5 in 
        let b = mk_binding i (Bind($2,None,f)) in 
        mk_exp i (ELet(b,$7)) }

  | FUN param param_list ARROW exp
      { let i = me2 $1 $5 in 
        build_bare_fun i ($2::$3) $5 }

  | FUN param param_list COLON asort ARROW exp
      { let i = me2 $1 $7 in 
        let f,_ = build_fun i ($2::$3) $7 $5 in 
        f }
      
  | gpexp                               
      { $1 }

/* "get put" expressions -- snipped JNF*/
gpexp: 
  | cexp GET aexp
      { let i = me $1 $3 in 
        mk_bin_op i (mk_prelude_var "get") $1 $3 }
  | cexp PUT aexp INTO aexp        
      { let i = me $1 $3 in 
        mk_tern_op i (mk_prelude_var "put") $1 $3 $5 }
  | cexp CREATE aexp               
      { let i = me $1 $3 in 
        mk_bin_op i (mk_prelude_var "create") $1 $3 }
  | cexp
      { $1 } 

/* case expressions */
cexp:
  | MATCH composeexp WITH branch_list COLON sort
      { let i4,pl = $4 in 
        mk_exp (m $1 i4) (ECase($2,pl,$6)) }

  | LPAREN MATCH composeexp WITH branch_list RPAREN COLON sort
      { let i5,pl = $5 in 
        mk_exp (m $1 i5) (ECase($3,pl,$8)) }

  | BEGIN MATCH composeexp WITH branch_list END COLON sort
      { let i5,pl = $5 in 
        mk_exp (m $1 i5) (ECase($3,pl,$8)) }

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
      { mk_exp (me $1 $3) (EPair($1,$3)) }

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
        mk_tyapp i $1 $3 }

  | aexp 
      { $1 }

/* atomic expressions */
aexp:
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

  | INTEGER
      { let i,n = $1 in 
        mk_int i n }

  | BOOLEAN
      { let i,b = $1 in 
        mk_bool i b }

  | qid 
      { mk_qid_var $1 }

  | CSET                                
      { let i1,s1 = $1 in 
        mk_exp i1 (ECSet(true,parse_cset s1)) }

  | NSET                                
      { let i1,s1 = $1 in 
        mk_exp i1 (ECSet(false,parse_cset s1)) }

  | STR 
      { let i,s = $1 in 
        mk_str i s }

  | RXSTR
      { let i,s = $1 in 
        mk_rx i (mk_str i s) }

  | LPAREN RPAREN
      { mk_exp (m $1 $2) EUnit }
      
  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

  | LPAREN aexp EQUAL aexp RPAREN
      { let i = me $2 $4 in 
        mk_app i (mk_app i (mk_prelude_var "equals") $2) $4 } 

/* --------- LISTS ------------ */
list:
  | RBRACK 
      { $1, (fun i s -> mk_tyapp i (mk_list_var "Nil") s) }

  | pexp RBRACK 
      { ($2, 
         (fun i s -> 
            mk_app i 
              (mk_tyapp i (mk_list_var "Cons") s)
              (mk_pair i $1 (mk_tyapp i (mk_list_var "Nil") s)))) }

  | pexp SEMI list
    { let i3,mk = $3 in 
      (i3, 
       (fun i s -> 
          mk_app i
            (mk_tyapp i (mk_list_var "Cons") s)
            (mk_pair i $1 (mk i s)))) }

/* --------- PATTERNS ---------- */
branch: 
  | pat ARROW mexp 
      { let i = m (info_of_pat $1) (info_of_exp $3) in 
        (i,$1,$3) }

pat:      
  | UIDENT ppat
      { let i1,_ = $1 in 
        let i = mp2 i1 $2 in 
         mk_pat i (PVnt(Qid.t_of_id $1,Some $2)) }

  | QIDENT ppat
      { let (i,qs) = $1 in 
        mk_pat i (PVnt(parse_qid i qs,Some $2)) }

  | ppat
      { $1 }

ppat:
  | ppat COMMA apat
      { let i = mp $1 $3 in 
        mk_pat i (PPar($1,$3)) }

  | apat
      { $1 }

apat:
  | UNDERLINE 
    { mk_pat $1 PWld }

  | LIDENT
      { let i,_ = $1 in 
        mk_pat i (PVar $1) }

  | LPAREN RPAREN
    { mk_pat (m $1 $2) PUnt }

  | INTEGER
    { let i,n = $1 in 
      mk_pat i (PInt(n)) }

  | BOOLEAN
      { let i,b = $1 in 
        mk_pat i (PBol(b)) }

  | STR
      { let i,s = $1 in 
        mk_pat i (PStr(s)) }

  | UIDENT
      { let i,_ = $1 in 
        mk_pat i (PVnt(Qid.t_of_id $1,None)) }
      
  | QIDENT
      { let (i,qs) = $1 in 
        mk_pat i (PVnt(parse_qid i qs,None)) }

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
  | FORALL VIDENT DARROW sort 
      { SForall($2,$4) }

  | arrsort
      { $1 }

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
  | LIDENT
      { SData([], Qid.t_of_id $1) }

  | asort LIDENT
      { SData([$1], Qid.t_of_id $2) }

  | LPAREN sort RPAREN LIDENT 
      { SData([$2], Qid.t_of_id $4) }

  | LPAREN sort COMMA sort_list RPAREN LIDENT 
      { SData($2::$4, Qid.t_of_id $6) }

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

  | LPAREN sort WHERE exp RPAREN
      { SRefine(Id.wild,$2,$4) }

  | LPAREN id COLON sort WHERE exp RPAREN
      { SRefine($2,$4,$6) }

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
      { Qid.t_of_id $1 }
  | UIDENT
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
  | param param_list
      { $1 :: $2 }

  |
      { [] }

param: 
  | LPAREN id COLON sort RPAREN
      { Misc.Left (mk_param (fst $2) (Param($2,$4))) }

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
      { $1 }

