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
let get_qid i = mk_qid (nat_pre i) (mk_id i "get")
let put_qid i = mk_qid (nat_pre i) (mk_id i "put")
let create_qid i = mk_qid (nat_pre i) (mk_id i "create")

let mk_compose2_exp i e1 e2 = EApp(i,EApp(i,EVar(i,compose2_qid i),e1),e2)

let mk_put_exp i l a co = match co with 
    None -> EApp(i, EApp(i, EVar(i, create_qid i), l), a)
  | Some c -> EApp(i, EApp(i, EApp(i, EVar(i, put_qid i), l), a), c)

let mk_get_exp i l c = EApp(i, EApp(i, EVar(i, get_qid i), l), c)

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
%type <Info.t * Syntax.sort> sort
%type <Syntax.qid> qid

%%

/* ---------------  modules --------------- */
modl:
  | MODULE IDENT EQUAL opens decls EOF        { let i = merge_inc $1 $6 in 
						  MDef(i,$2,$4,$5) }
      
opens:
  |                                          { [] }
  | OPEN qid opens                           { $2::$3 }


/* ---------------  declarations --------------- */
decls:
  |                                               { [] }
  | LET binding_list decls                        { let i = merge_inc $1 (info_of_bindings $1 $2) in 
						      (DLet(i,$2))::$3 }
  | TYPE typebinding_list decls                   { let i = merge_inc $1 (info_of_typebindings $1 $2) in 
						      (DType(i,$2))::$3 }
  | MODULE IDENT EQUAL decls END decls            { let i = merge_inc $1 $5 in (DMod(i,$2,$4))::$6 }
  | TEST exp EQUAL test_res decls                 { let (i2,res) = $4 in 
						    let i = merge_inc $1 i2 in						      
						      (DTest(i, $2, res))::$5 }
  | TEST appexp SLASH BACKSLASH exp EQUAL exp decls { let i = merge_inc $1 (info_of_exp $7) in
							DTest(i, mk_get_exp i $2 $5, (Some $7))
							::DTest(i,mk_put_exp i $2 $7 None, (Some $5))
							::$8 }

  | SYNC WITH aexp AT atypeexp aexp EQUAL aexp decls { let i = merge_inc $1 (info_of_exp $8) in 
							 (DTestSync(i,$3,$3,$3,$5,$6,$8)) :: $9 }
  | SYNC WITH aexp aexp aexp AT atypeexp aexp EQUAL aexp decls { let i = merge_inc $1 (info_of_exp $10) in 
								   (DTestSync(i,$3,$4,$5,$7,$8,$10)) :: $11 }

test_res:
  | exp                                     { (info_of_exp $1, Some $1) }
  | ERROR                                   { ($1, None) }

/* ---------------  bindings --------------- */
binding_list:
  | binding_list AND binding                 { $1@[$3] }
  | binding                                  {  [$1] }
      
binding:    
  | IDENT param_list opt_sort EQUAL exp      { let i = merge_inc (info_of_id $1) (info_of_exp $5) in 
						 BDef(i,$1,$2,$3,$5) }
      
typebinding:
  | IDENT IDENT_list EQUAL typeexp           { ($1, $2, $4) } 
      
typebinding_list:
  | typebinding_list AND typebinding         { $1@[$3] }
  | typebinding                              { [$1] }

/* ---------------  sorts --------------- */
sort:
  | asort ARROW sort                         { let (i1,s1) = $1 in 
					       let (i2,s2) = $3 in 
					       let i = merge_inc i1 i2 in 
						 (i,SArrow (s1,s2)) }
  | asort DOUBLEARROW sort                   { let (i1,s1) = $1 in 
					       let (i2,s2) = $3 in 
					       let i = merge_inc i1 i2 in 
						 (i,STOper (s1,s2)) }
  | asort                                    { $1 }

asort:
  | LENS                                     { ($1, SLens) }
  | VIEW                                     { ($1, SView) }
  | NAME                                     { ($1, SName) } 
  | TYPE                                     { ($1, SType) }
  | LPAREN sort RPAREN                       { $2 }

opt_sort:
  |                                          { None }
  | COLON asort                              { let (_,s) = $2 in Some s }
      
param_list:
  |                                          { [] }
  | param param_list                         { $1::$2 }

param:
  | IDENT COLON asort                        { let (i2,s) = $3 in 
					       let i = merge_inc (info_of_id $1) i2 in 
						 PDef(i,$1,s) }
  | LPAREN IDENT COLON sort RPAREN           { let i = merge_inc $1 $5 in
					       let (_,s) = $4 in 
						 PDef($1,$2,s) }

/* ---------------  quoted terms --------------- */
quoted_name:
  | IDENT_or_STRING                          { let i = info_of_id $1 in EName(i, $1) }
  | BACKTICK aexp                            { $2 }
      
quoted_view:
  | viewexp                                  { $1 }
  | IDENT_or_STRING                          { let i = info_of_id $1 in EName(i, $1) }
  | BACKTICK aexp                            { $2 }

/* ---------------  expressions --------------- */
exp: 
  | LET binding_list IN exp                  { let i = merge_inc $1 (info_of_exp $4) in ELet(i,$2,$4) }
  | FUN param param_list opt_sort ARROW exp  { let i = merge_inc $1 (info_of_exp $6) in EFun(i,$2::$3,$4,$6) }
  | composeexp                               { $1 }

composeexp:
  | composeexp SEMI getputexp                { let i = merge_inc (info_of_exp $1) (info_of_exp $3) in
						 mk_compose2_exp i $1 $3 }
  | getputexp                                { $1 } 

getputexp:
  | appexp SLASH appexp                                   { let i = merge_inc (info_of_exp $1) (info_of_exp $3) in
							      mk_get_exp i $1 $3 }
  | appexp BACKSLASH aexp aexp                            { let i = merge_inc (info_of_exp $1) (info_of_exp $4) in 
							      mk_put_exp i $1 $3 (Some $4) }
  | appexp BACKSLASH LPAREN exp COMMA exp RPAREN          { let i = merge_inc (info_of_exp $1) $7 in
							      mk_put_exp i $1 $4 (Some $6) }
  | appexp BACKSLASH LPAREN exp COMMA MISSING RPAREN      { let i = merge_inc (info_of_exp $1) $7 in 
							      mk_put_exp i $1 $4 None }
  | appexp BACKSLASH aexp MISSING                         { let i = merge_inc (info_of_exp $1) $4 in 
							      mk_put_exp i $1 $3 None}
  | appexp                                                { $1 }
      
appexp:
  | appexp aexp                              { let i = merge_inc (info_of_exp $1) (info_of_exp $2) in 
						 EApp(i,$1,$2) }
  | aexp                                     { $1 }

aexp:
  | STRING                                   { let i = info_of_id $1 in EName(i, $1) } 
  | qid                                      { let i = info_of_qid $1 in EVar(i,$1) }
  | viewexp                                  { $1 }
  | LANGLE typeexp RANGLE                    { let i = info_of_typeexp $2 in EType(i,$2) }
  | LPAREN exp RPAREN                        { $2 }
  | LBRACE map_list RBRACE                   { let i = merge_inc $1 $3 in EMap(i, $2) }

map:
  | quoted_name ARROW exp                    { let i = merge_inc (info_of_exp $1) (info_of_exp $3) in 
						 (i, $1,$3) }

map_list:
  | map                                      { [$1] }
  | map COMMA map_list                       { $1::$3 }

/* ---------------  views --------------- */
viewexp:
  | aviewexp COLON COLON viewexp             { let i = merge_inc (info_of_exp $1) (info_of_exp $4) in 
						 EConsView(i,$1,$4) } 
  | aviewexp                                 { $1 }

aviewexp:
  | LBRACE viewelt_list RBRACE               { let i = merge_inc $1 $3 in 
						 EView(i, $2) }
  | LBRACK quoted_view_list RBRACK           { let i = merge_inc $1 $3 in 
						 Safelist.fold_right 
						   (fun ci v -> 
						      let i = merge_inc (info_of_exp ci) (info_of_exp v) in 
							EConsView(i, ci, v))
						   $2
						   (nil_view i) }

viewelt:
  | quoted_name                              { let i = info_of_exp $1 in (i, $1, emptyView i) }				  	     
  | quoted_name EQUAL quoted_view            { let i = info_of_exp $1 in (i, $1, $3) }

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

/* ---------------  types --------------- */
typeexp:
  | ptypeexp                                 { TT $1 }
  | TILDE ptypeexp                           { NT $2 }

ptypeexp:
  | ptypeexp BAR apptypeexp                  { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $3) in 
						 TUnion(i,[$1;$3]) }
  | apptypeexp                               { $1 }

apptypeexp:
  | apptypeexp ctypeexp                      { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $2) in 
						 TApp(i, $1, $2) }
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

/* a copy of parsing rules above, but using iatypeexp at the leaf */
iptypeexp:
  | iptypeexp BAR iapptypeexp                { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $3) in 
					     	   TUnion(i,[$1;$3]) }
  | iapptypeexp                              { $1 }

iapptypeexp:
  | iapptypeexp ictypeexp                    { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $2) in 
					     	   TApp(i, $1, $2) }
  | ictypeexp                                { $1 }

ictypeexp:
  | iatypeexp COLON COLON ictypeexp          { let i = merge_inc (info_of_ptypeexp $1) (info_of_ptypeexp $4) in
					     	 let cons = TVar(i, cons_qid i) in TApp(i, TApp(i, cons, $1), $4) }
  | iatypeexp                                { $1 }

iatypeexp:
  | BACKTICK atypeexp                        { $2 }
  | LPAREN iptypeexp RPAREN                  { $2 } 
  | LBRACE typeelt_list RBRACE               { let i = merge_inc $1 $3 in TCat(i,$2) }
  | LBRACK iptypeexp_list RBRACK             { let i = merge_inc $1 $3 in
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
  | STAR excepts_opt EQUAL iptypeexp         { let i = merge_inc $1 (info_of_ptypeexp $4) in 
						 TStar($1,$2,$4) }
  | STAR excepts_opt EQUAL IDENT_or_STRING   { let n_info = info_of_id $4 in 
					       let i = merge_inc $1 n_info in
						 TStar(i, $2, TName(n_info, EName(n_info, $4), emptyViewType i)) }
  | BANG excepts_opt EQUAL iptypeexp         { let i = merge_inc $1 (info_of_ptypeexp $4) in 
						 TBang($1,$2,$4) }
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
  | non_empty_iptypeexp_list                 { $1 }

non_empty_iptypeexp_list:
  | iptypeexp                                { [$1] }
  | iptypeexp COMMA non_empty_iptypeexp_list { $1::$3 }

excepts_opt :
  |                                          { [] }
  | BACKSLASH LPAREN except_list RPAREN      { $3 }
  | BACKSLASH except_list                    { $2 }

two_except_list:
  | quoted_name COMMA except_list            { $1::$3 }

except_list:
  | quoted_name                              { [$1] }
  | two_except_list                          { $1 }
      
/* ---------------  identifiers --------------- */
qid:
  | IDENT                                   { qid_of_id ($1) }
  | qid DOT IDENT                           { dot $1 (qid_of_id $3) }

IDENT_list:
  |                                         { [] }
  | IDENT IDENT_list                        { $1::$2 }

IDENT_or_STRING:
  | STRING                                  { $1 }
  | IDENT                                   { $1 }
