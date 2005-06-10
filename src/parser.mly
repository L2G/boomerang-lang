/************************************************************/
/* The Harmony Project                                      */
/* harmony@lists.seas.upenn.edu                             */
/*                                                          */
/* parser.mly - Focal parser generator                      */
/************************************************************/
/* $Id$ */

%{

open Error
open Syntax
open Info

(* imports *)
let ( @ ) = Safelist.append
let debug = Trace.debug "parser"

(* shorthands *)
let m = merge_inc
let me e1 e2 = m (info_of_exp e1) (info_of_exp e2)
let me1 e1 i2 = m (info_of_exp e1) i2
let me2 i1 e2 = m i1 (info_of_exp e2)
let mie x1 e2 = m (info_of_id x1) (info_of_exp e2)
let mis x1 s1 = m (info_of_id x1) (fst s1)
let ms s1 s2 = m (fst s1) (fst s2)
let mbs2 i1 bs2 = m i1 (info_of_bindings i1 bs2)
  
(* helpers for easy syntax tree construction *)

(* mk_compose2 exp *)
let mk_compose2_exp i e1 e2 = 
    EApp(i, EApp(i, EVar(i, compose2_qid i),e1),e2)
      
(* mk_get_exp *)
let mk_get_exp i l c = 
    EApp(i, EApp(i, EVar(i, get_qid i), l), c)

(* mk_put_exp *)
let mk_put_exp i l a co = 
  match co with 
      None -> EApp(i, EApp(i, EVar(i, create_qid i), l), a)
    | Some c -> EApp(i, EApp(i, EApp(i, EVar(i, put_qid i), l), a), c)
	
(* mk_sync_exp *)
let mk_sync_exp i lo la lb t v = 
    EApp(i,EApp(i,EApp(i,EApp(i,EApp(i,EVar(i,sync_qid i),lo),la),lb),t),v)
      
(* mk_empty_view *)
let mk_empty_view i = ECat(i,[])

%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT STRING
%token <Info.t> LET IN FUN AND MODULE BEGIN END OPEN TYPE
%token <Info.t> LENS VIEW TYPE NAME ARROW BACKTICK
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH SLASH TEST SYNC WITH AT ERROR MISSING
%token <Info.t> STAR BANG BAR PLUS QMARK

%start modl sort qid 
%type <Syntax.modl> modl
%type <Info.t * Syntax.sort> sort
%type <Syntax.qid> qid

%%

/* ---------------  modules --------------- */
modl:
  | MODULE IDENT EQUAL opens decls EOF        { MDef(m $1 $6,$2,$4,$5) }
      
opens:
  | OPEN qid opens                           { $2::$3 }
  |                                          { [] }

/* ---------------  declarations --------------- */
decls:
  | LET binding_list decls                   { DLet(mbs2 $1 $2,$2)::$3 }
  | MODULE IDENT EQUAL decls END decls       { DMod(m $1 $5,$2,$4)::$6 } 
  | TEST exp EQUAL test_res decls            { DTest(m $1 (fst $4), $2, snd $4)::$5 }
  | TEST composeexp SLASH BACKSLASH exp 
      EQUAL exp decls                        { let i = me2 $1 $7 in
					       let get_test = DTest(i, mk_get_exp i $2 $5, (Some $7)) in
					       let put_test = DTest(i, mk_put_exp i $2 $7 None, (Some $5)) in
						 get_test::put_test::$8 }
      
  | SYNC WITH aexp AT aexp aexp EQUAL 
      aexp decls                             { let i = me2 $1 $8 in
					       let sync_test = DTest(i, mk_sync_exp i $3 $3 $3 $5 $6, Some $8) in
						 sync_test::$9 } 
  | SYNC WITH aexp aexp aexp AT 
      aexp aexp EQUAL aexp decls             { let i = me2 $1 $10 in
					       let sync_test = DTest(i, mk_sync_exp i $3 $4 $5 $7 $8, Some $10) in
						 sync_test::$11 }
  |                                          { [] }
      
test_res:
  | exp                                      { (info_of_exp $1, Some $1) }
  | ERROR                                    { ($1, None) }

/* ---------------  bindings --------------- */
binding_list:
  | binding_list AND binding                 { $1@[$3] }
  | binding                                  { [$1] }

binding:    
  | IDENT param_list opt_sort EQUAL exp      { BDef(mie $1 $5,$1,$2,$3,$5) }
      
/* ---------------  sorts --------------- */
sort:
  | asort ARROW sort                         { (ms $1 $3, SArrow(snd $1, snd $3)) }
  | asort                                    { $1 }

asort:
  | LENS                                     { ($1, SLens) }
  | VIEW                                     { ($1, SView) }
  | NAME                                     { ($1, SName) } 
  | TYPE                                     { ($1, SType) }
  | LPAREN sort RPAREN                       { $2 }

opt_sort:
  | COLON asort                              { Some (snd $2) }
  |                                          { None }
      
param_list:
  | param param_list                         { $1::$2 }
  |                                          { [] }

param:
  | IDENT COLON asort                        { PDef(mis $1 $3, $1, snd $3) }
  | LPAREN IDENT COLON sort RPAREN           { PDef(m $1 $5, $2, snd $4) }
      
/* ---------------  expressions --------------- */
exp: 
  | LET binding_list IN exp                  { ELet(me2 $1 $4,$2,$4) }
  | FUN param param_list opt_sort ARROW exp  { EFun(me2 $1 $6,$2::$3,$4,$6) }
  | getputexp                                { $1 }

getputexp:
  | composeexp SLASH composeexp              { mk_get_exp (me $1 $3) $1 $3 }
  | composeexp BACKSLASH aexp aexp           { mk_put_exp (me $1 $4) $1 $3 (Some $4) }
  | composeexp BACKSLASH aexp MISSING        { mk_put_exp (me1 $1 $4) $1 $3 None }
  | composeexp                               { $1 }

composeexp:
  | composeexp SEMI barexp                   { mk_compose2_exp (me $1 $3) $1 $3 }
  | barexp                                   { $1 } 

barexp:
  | barexp BAR plusexp                       { EUnion(me $1 $3,[$1;$3]) }
  | plusexp                                  { $1 }

plusexp:
  | plusexp PLUS consexp                     { ECat(me $1 $3, [$1; $3]) }
  | consexp                                  { $1 }

consexp:
  | appexp COLON COLON consexp               { ECons(me $1 $4, $1, $4) }
  | appexp                                   { $1 }

appexp:
  | appexp aexp                              { EApp(me $1 $2,$1,$2) }
  | aexp                                     { $1 }

aexp:
  | STRING                                   { EName(info_of_id $1,$1) }
  | qid                                      { EVar(info_of_qid $1,$1) }
  | typeexp                                  { $1 }
  | LPAREN exp RPAREN                        { $2 }
  | BEGIN exp END                            { $2 }
  | LBRACE map_list RBRACE                   { EMap(m $1 $3, $2) }
      
map:
  | quoted_name ARROW exp                    { ($1,$3) }

/* maps must be non-empty to avoid overlap with types */
map_list:
  | map                                      { [$1] }
  | map COMMA map_list                       { $1::$3 }
      
/* ---------------  types/views --------------- */
typeexp:
  | LBRACE typeelt_list RBRACE               { ECat(m $1 $3,$2) }
  | LBRACK exp_list RBRACK                   { Safelist.fold_right
						 (fun ti acc -> 
						    ECons(me ti acc,ti,acc))
						 $2
						 (ENil($3))}

typeelt:
  | quoted_name                              { let i = info_of_exp $1 in 
						 EAtom(i, $1, mk_empty_view i) }
  | quoted_name EQUAL exp                    { EAtom(me $1 $3,$1,$3) }
  | quoted_name QMARK EQUAL exp              { let i = me $1 $4 in 
						 EUnion(i, [ECat(i,[]); EAtom(i,$1,$4)]) }
  | STAR excepts_opt EQUAL exp               { EStar (me2 $1 $4,$2,$4) }
  | BANG excepts_opt EQUAL exp               { EBang(me2 $1 $4,$2,$4) }
  | BANG QMARK excepts_opt EQUAL exp         { let i = me2 $1 $5 in 
						 EUnion(i, [ECat(i,[]); EBang(i,$3,$5)]) }							  
  | PLUS excepts_opt EQUAL exp               { let i = me2 $1 $4 in 
						 ECat(i,[EBang(i,$2,$4); EStar(i,$2,$4)]) }
      
/* --------------- type helpers --------------- */
quoted_name:
  | IDENT_or_STRING                          { EName(info_of_id $1,$1) }
  | BACKTICK aexp                            { $2 }


typeelt_list:
  | non_empty_typeelt_list                   { $1 }
  |                                          { [] }

non_empty_typeelt_list:
  | typeelt                                  { [$1] }
  | typeelt COMMA non_empty_typeelt_list     { $1::$3 }

exp_list:
  | non_empty_exp_list                       { $1 }
  |                                          { [] }

non_empty_exp_list:
  | exp                                      { [$1] }
  | exp COMMA non_empty_exp_list             { $1::$3 }

excepts_opt :
  | BACKSLASH LPAREN except_list RPAREN      { $3 }
  | BACKSLASH except_list                    { $2 }
  |                                          { [] }

two_except_list:
  | quoted_name COMMA except_list            { $1::$3 }

except_list:
  | quoted_name                              { [$1] }
  | two_except_list                          { $1 }
      
/* ---------------  identifiers --------------- */
qid:
  | IDENT                                    { qid_of_id ($1) }
  | qid DOT IDENT                            { dot $1 (qid_of_id $3) }

IDENT_or_STRING:
  | STRING                                  { $1 }
  | IDENT                                   { $1 }
					     
