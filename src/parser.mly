/************************************************************/
/* The Harmony Project                                      */
/* harmony@lists.seas.upenn.edu                             */
/*                                                          */
/* parser.mly - Focal parser generator                      */
/*                                                          */
/* $Id$ */
/*                                                          */
/************************************************************/

%{

open Error
open Syntax
open Info

let ( @ ) = Safelist.append
  
(* constants *)
let nat_pre i = Safelist.map (mk_id i) ["Native"; "Prelude"]

let compose2_qid i = mk_qid (nat_pre i) (mk_id i "compose2")
let nil_qid i = mk_qid (nat_pre i) (mk_id i "Nil")
let cons_qid i = mk_qid (nat_pre i) (mk_id i "Cons")
let nil_tag_qid i = mk_qid (nat_pre i) (mk_id i "nil_tag")
let nil_view i = 
  let nil_name = EVar(i, nil_tag_qid i) in 
  let empty_view = EView(i, []) in
    EView(i, [(i, nil_name, empty_view)]) 
%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT STRING
%token <Info.t> LET IN FUN AND MODULE BEGIN END OPEN TYPE
%token <Info.t> LENS VIEW TYPE NAME ARROW DOUBLEARROW BACKTICK
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKSLASH SLASH TEST SYNC WITH AT ERROR MISSING
%token <Info.t> STAR BANG BAR TILDE 

%start modl sort qid 
%type <Syntax.modl> modl
%type <Syntax.sort> sort
%type <Syntax.qid> qid

%%

/*** MODULES ***/
modl:
  | MODULE IDENT EQUAL opens decls EOF        { let i = merge_inc $1 $6 in 
						  MDef(i,$2,$4,$5) }
      
opens:
  |                                          { [] }
  | OPEN qid opens                           { $2::$3 }


/*** DECLARATIONS ***/
decls:
  |                                               { [] }
  | LET binding_list decls                        { (DLet($1,$2))::$3 }
  | TYPE typebinding_list decls                   { (DType($1,$2))::$3 } 
  | MODULE IDENT EQUAL decls END decls            { (DMod($1,$2,$4))::$6 }
  | TEST exp SLASH get_args EQUAL test_res decls                { let (i2,res) = $6 in (DTestGet(merge_inc $1 i2, $2, $4, res))::$7 }
  | TEST exp BACKSLASH put_args EQUAL test_res decls            { let (i2,res) = $6 in (DTestPut(merge_inc $1 i2, $2, $4, res))::$7 }
  | TEST exp SLASH BACKSLASH get_args EQUAL exp decls           { let i = merge_inc $1 (info_of_exp $7) in
								    (DTestGet(i, $2, $5, (Some $7)))::
								    (DTestPut(i, $2, ($7, None), Some $5))::$8 }
  | SYNC WITH aexp AT atypeexp aexp EQUAL aexp decls            { let i = merge_inc $1 (info_of_exp $8) in (DTestSync(i,$3,$3,$3,$5,$6,$8)) :: $9 }
  | SYNC WITH aexp aexp aexp AT atypeexp aexp EQUAL aexp decls  { let i = merge_inc $1 (info_of_exp $10) in (DTestSync(i,$3,$4,$5,$7,$8,$10)) :: $11 }
      
/* TEST Stuff */
get_args:
  | aexp                                 { $1 }

put_args:
  | aexp aexp                            { ($1, Some $2) }
  | aexp MISSING                         { ($1, None) }
  | LPAREN exp COMMA exp RPAREN          { ($2, Some $4) }
  | LPAREN exp COMMA MISSING RPAREN      { ($2, None) }

test_res:
  | exp                                     { (info_of_exp $1, Some $1) }
  | ERROR                                   { ($1, None) }

/**** BINDINGS ***/      
binding_list:
  | binding_list AND binding                 { $1@[$3] }
  | binding                                  {  [$1] }
      
binding:    
  | IDENT param_list opt_sort EQUAL exp      { let i = merge_inc 
						 (info_of_id $1) 
						 (info_of_exp $5) 
					       in 
						 BDef(i,$1,$2,$3,$5) 
					     }
      
typebinding:
  | IDENT IDENT_list EQUAL typeexp           { ($1, $2, $4) } 
      
typebinding_list:
  | typebinding_list AND typebinding         { $1@[$3] }
  | typebinding                              { [$1] }

/*** SORTS ***/
sort:
  | asort ARROW sort                         { let i = merge_inc 
						 (info_of_sort $1) 
						 (info_of_sort $3) 
					       in 
						 SArrow (i,$1,$3) 
					     }
  | asort DOUBLEARROW sort                   { let i = merge_inc 
						 (info_of_sort $1) 
						 (info_of_sort $3) 
					       in 
						 STOper (i,$1,$3) 
					     }
  | asort                                    { $1 }

asort:
  | LENS                                     { SLens($1) }
  | VIEW                                     { SView($1) }
  | NAME                                     { SName($1) } 
  | TYPE                                     { SType($1) }
  | LPAREN sort RPAREN                       { $2 }

opt_sort:
  |                                          { None }
  | COLON asort                              { Some $2 }
      
param_list:
  |                                          { [] }
  | param param_list                         { $1::$2 }

param:
  | IDENT COLON asort                        { let i = merge_inc 
						 (info_of_id $1) 
						 (info_of_sort $3) 
					       in 
						 PDef(i,$1,$3) 
					     }
  | LPAREN IDENT COLON sort RPAREN           { let i = merge_inc 
						 (info_of_id $2) 
						 (info_of_sort $4) 
					       in 
						 PDef($1,$2,$4) }

/**** QUOTED TERMS ****/
quoted_name:
  | IDENT_or_STRING                          { let i = info_of_id $1 in EName(i, $1) }
  | BACKTICK aexp                            { $2 }
      
quoted_view:
  | viewexp                                  { $1 }
  | IDENT_or_STRING                          { let i = info_of_id $1 in EName(i, $1) }
  | BACKTICK aexp                            { $2 }

/*** EXPRESSIONS ***/
exp: 
  | LET binding_list IN exp                  { ELet($1,$2,$4) }
  | FUN param param_list opt_sort ARROW exp  { EFun($1,$2::$3,$4,$6) }
  | composeexp                               { $1 }

composeexp:
  | composeexp SEMI appexp                   { let i = merge_inc 
						 (info_of_exp $1) 
						 (info_of_exp $3) 
					       in
					       let c2 = compose2_qid i in
						 EApp(i,EApp(i,EVar(i,c2),$1),$3)
					     }
  | appexp                                   { $1 } 

appexp:
  | appexp aexp                              { EApp(info_of_exp $1,$1,$2) }
  | aexp                                     { $1 }

aexp:
  | STRING                                   { let i = info_of_id $1 in EName(i, $1) } 
  | qid                                      { let i = info_of_qid $1 in EVar(i,$1) }
  | viewexp                                  { $1 }
  | LANGLE typeexp RANGLE                    { EType($1,$2) }
  | LPAREN exp RPAREN                        { $2 }
  | LBRACE map_list RBRACE                   { EMap($1, $2) }

map:
  | quoted_name ARROW exp                    { let i = merge_inc (info_of_exp $1) (info_of_exp $3) in 
						 (i, $1,$3) 
					     }

map_list:
  | map                                      { [$1] }
  | map COMMA map_list                       { $1::$3 }

/*** VIEWS ***/
viewexp:
  | aviewexp COLON COLON viewexp             { EConsView(merge_inc (info_of_exp $1) (info_of_exp $4) ,$1,$4) } 
  | aviewexp                                 { $1 }

aviewexp:
  | LBRACE viewelt_list RBRACE               { EView(merge_inc $1 $3, $2) }
  | LBRACK quoted_view_list RBRACK           { Safelist.fold_right 
						 (fun ci v -> 
						    let i = merge_inc (info_of_exp ci) (info_of_exp v) in 
						      EConsView(i, ci, v))
						 $2
						 (nil_view (merge_inc $1 $3)) }

viewelt:
  | quoted_name                              { let i = info_of_exp $1 in (i, $1, emptyView i) }				  	     
  | quoted_name EQUAL quoted_view            { (info_of_exp $1, $1, $3) }

viewelt_list:
  |                                          { [] }
  | non_empty_viewelt_list                   { $1 }

non_empty_viewelt_list:
  | viewelt                                  { [$1] }
  | viewelt COMMA non_empty_viewelt_list     { $1::$3 }

quoted_view_list:
  |                                          { [] }
  | non_empty_quoted_view_list               { $1 }

non_empty_quoted_view_list:
  | quoted_view                                  { [$1] }
  | quoted_view COMMA non_empty_quoted_view_list { $1::$3 }

/*** TYPES ***/
typeexp:
  | ptypeexp                                 { TT $1 }
  | TILDE ptypeexp                           { NT $2 }

ptypeexp:
  | ptypeexp BAR apptypeexp                  { TUnion($2,[$1;$3]) }
  | apptypeexp                               { $1 }

apptypeexp:
  | apptypeexp ctypeexp                      { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $2) in TApp(i, $1, $2) }
  | ctypeexp                                 { $1 }

ctypeexp:
  | atypeexp COLON COLON ctypeexp            { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $4) in
					       let cons = TVar(i, cons_qid i) in TApp(i, TApp(i, cons, $1), $4) }
  | atypeexp                                 { $1 }

atypeexp:
  | qid                                      { let i = info_of_qid $1 in 
						 match string_of_qid $1 with 
						     "Any" -> TAny(i)
						   | "Empty" -> TEmpty(i)
						   | _ -> TVar(i, $1) }
  | LPAREN ptypeexp RPAREN                   { $2 } 
  | LBRACE typeelt_list RBRACE               { let i = merge_inc $1 $3 in TCat(i,$2) }
  | LBRACK iptypeexp_list RBRACK             { let i = merge_inc $1 $3 in
					       let nil = TVar(i, nil_qid i) in
					       let cons = TVar(i, cons_qid i) in
						 Safelist.fold_right
						   (fun ti acc -> TApp(i, TApp(i, cons, ti), acc))
						   $2
						   nil
					     }

/* another copy of parsing rules above, but using iatypeexp at the leaf */
iptypeexp:
  | iptypeexp BAR iapptypeexp                  { TUnion($2,[$1;$3]) }
  | iapptypeexp                               { $1 }

iapptypeexp:
  | iapptypeexp ictypeexp                      { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $2) in TApp(i, $1, $2) }
  | ictypeexp                                 { $1 }

ictypeexp:
  | iatypeexp COLON COLON ictypeexp            { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $4) in
					       let cons = TVar(i, cons_qid i) in TApp(i, TApp(i, cons, $1), $4) }
  | iatypeexp                                 { $1 }

iatypeexp:
  | BACKTICK atypeexp                         { $2 }
  | LPAREN iptypeexp RPAREN                   { $2 } 
  | LBRACE typeelt_list RBRACE                { let i = merge_inc $1 $3 in TCat(i,$2) }
  | LBRACK iptypeexp_list RBRACK              { let i = merge_inc $1 $3 in
						let nil = TVar(i, nil_qid i) in
						let cons = TVar(i, cons_qid i) in
						  Safelist.fold_right
						    (fun ti acc -> TApp(i, TApp(i, cons, ti), acc))
						    $2
						    nil
					      }

typeelt:
  | quoted_name                              { let i = info_of_exp $1 in TName(i, $1, emptyViewType i) }
  | quoted_name EQUAL iptypeexp              { let i = merge_inc (info_of_exp $1) (info_of_ptypeexp $3) in
						 TName(i, $1, $3) }
  | quoted_name EQUAL IDENT_or_STRING        { let n_info = info_of_id $3 in
					       let i = merge_inc (info_of_exp $1) n_info in
						 TName(i, $1, TName(n_info, EName(n_info, $3), emptyViewType i)) }
  | STAR excepts_opt EQUAL iptypeexp         { TStar($1,$2,$4) }
  | STAR excepts_opt EQUAL IDENT_or_STRING   { let n_info = info_of_id $4 in 
					       let i = merge_inc $1 n_info in
						 TStar(i, $2, TName(n_info, EName(n_info, $4), emptyViewType i)) }
  | BANG excepts_opt EQUAL iptypeexp         { TBang($1,$2,$4) }
  | BANG excepts_opt EQUAL IDENT_or_STRING   { let n_info = info_of_id $4 in 
					       let i = merge_inc $1 n_info in
						 TBang(i, $2, TName(n_info, EName(n_info, $4), emptyViewType i)) }

typeelt_list:
  |                                          { [] }
  | non_empty_typeelt_list                   { $1 }

non_empty_typeelt_list:
  | typeelt                                  { [$1] }
  | typeelt COMMA non_empty_typeelt_list     { $1::$3 }

iptypeexp_list:
  |                                          { [] }
  | non_empty_iptypeexp_list                  { $1 }

non_empty_iptypeexp_list:
  | iptypeexp                                 { [$1] }
  | iptypeexp COMMA non_empty_iptypeexp_list   { $1::$3 }

excepts_opt :
  |                                          { [] }
  | BACKSLASH LPAREN except_list RPAREN      { $3 }
  | BACKSLASH except_list                        { $2 }

two_except_list:
  | quoted_name COMMA except_list                    { $1::$3 }

except_list:
  | quoted_name                              { [$1] }
  | two_except_list                          { $1 }
      
/*** identifiers ***/
qid:
  | IDENT                                   { qid_of_id ($1) }
  | qid DOT IDENT                           { dot $1 (qid_of_id $3) }

IDENT_list:
  |                                         { [] }
  | IDENT IDENT_list                        { $1::$2 }

IDENT_or_STRING:
  | STRING                                  { $1 }
  | IDENT                                   { $1 }

