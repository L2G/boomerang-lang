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

let error t info = let (l,c1),(_,c2) = info in
  let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Error.Parse_error (s,info))
    else raise (Error.Parse_error (s^ ": " ^ t,info))

(* constants *)
let compose2_qid i = ([(i,"Pervasives"); (i,"Native")], (i, "compose2"))
let list_qid i = ([(i,"Pervasives")], (i, "List"))

(* rewrite exp_cores w/o ENames to typeexps *)
let rec e2te e = match e with
  | EVar(i,q)     -> TVar(i,q)
  | EApp(i,e1,e2) -> TApp(i, e2te e1, e2te e2)
  | _             -> 
      let i = info_of_exp e in 
	raise Parsing.Parse_error
%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT STRING
%token <Info.t> LET IN FUN AND MODULE END OPEN TYPE
%token <Info.t> LENS VIEW TYPE NAME ARROW
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE
%token <Info.t> SEMI COMMA DOT EQUAL COLON
%token <Info.t> EMPTY STAR BANG BAR TILDE 

%start modl sort qid ext_view
%type <Syntax.modl> modl
%type <Syntax.sort> sort
%type <Syntax.qid> qid
%type <V.t> ext_view

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
  |                                          { [] }
  | LET binding_list decls                   { let i = merge_inc 
						 $1 
						 (info_of_bindings $1 $2) 
					       in 
						 (DLet(i,$2))::$3 
					     }
  | TYPE typebinding_list decls              { let i = merge_inc 
						 $1 
						 (info_of_typebindings $1 $2) 
					       in 
                                                 (DType($1,$2))::$3 
					     } 
  | MODULE IDENT EQUAL decls END decls       { let i = merge_inc $1 $5 in 
						 (DMod($1,$2,$4))::$6 
					     }

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
  | name                                     { let (i,_) = $1 in 
						 EName(i,$1) 
					     } 
  | qid                                      { let (_,(i,_)) = $1 in 
						 EVar(i,$1) 
					     }
  | viewexp                                  { $1 }
  | LANGLE typeexp RANGLE                    { EType($1,$2) }
  | LPAREN exp RPAREN                        { $2 }
  | LBRACE map_list RBRACE                   { EMap($1, $2) }

map:
  | exp_core ARROW exp                       { let i = 
						 merge_inc 
						   (info_of_exp $1) 
						   (info_of_exp $3) 
					       in 
						 (i, $1,$3) 
					     }
      
map_list:
  | map                                      { [$1] }
  | map COMMA map_list                       { $1::$3 }

/* Core subset of expressions: 
 *    just identifiers, names, applications; 
 *    used in views, maps, and types.
 */
exp_core:
  | exp_core aexp_core                       { EApp(info_of_exp $1,$1,$2) }
  | aexp_core                                { $1 }

aexp_core:
  | name                                     { let (i,_) = $1 in EName(i,$1) } 
  | aexp_core_but_name                       { $1 }

aexp_core_but_name:  
  | qid                                      { let (_,(i,_)) = $1 in EVar(i,$1) }
  | LPAREN exp_core RPAREN                   { $2 }
                                                 
/*** VIEWS ***/
viewexp:
  | LBRACE viewelt_list RBRACE               { EView($1,$2, false) }
  | LBRACK viewelt_list RBRACK               { EView($1,$2, true) }
  
viewelt_list:
  |                                          { [] }
  | non_empty_viewelt_list                   { $1 }

non_empty_viewelt_list:
  | viewelt                                  { [$1] }
  | viewelt COMMA non_empty_viewelt_list     { $1::$3 }

viewelt:
  | exp_core                                 { let i = info_of_exp $1 in 
						 (i, $1, emptyView i) 
					     }
  | exp_core EQUAL innerview                 { let i = info_of_exp $1 in 
						 (i, $1, $3) 
					     }

innerview:
  | viewexp                                  { $1 }
  | aexp_core_but_name                       { $1 }
  | name                                     { let (i,x) = $1 in 
						 EView(i,[(i,EName(i,$1), emptyView i)], false) 
					     }      

/*** TYPES ***/
typeexp:
  | ptypeexp                                 { TT $1 }
  | TILDE exp_core                           { NT (e2te $2) }

ptypeexp:
  | ptypeexp BAR ctypeexp                    { TUnion($2,[$1;$3]) }
  | ctypeexp                                 { $1 }

ctypeexp:
  | atypeexp COMMA ctypeexp                  { TCat($2, [$1;$3]) }
  | atypeexp                                 { $1 }

atypeexp:
  | EMPTY                                    { TEmpty($1) } 
  | exp_core LBRACE innertype RBRACE         { let i = info_of_exp $1 in 
						 TName(i,$1,$3) 
					     }  
  | STAR excepts_opt LBRACE innertype RBRACE { TStar($1,$2,$4) }
  | BANG excepts_opt LBRACE innertype RBRACE { TBang($1,$2,$4) }
  | LPAREN ptypeexp RPAREN                   { $2 } 
  | LBRACE typeelt_list RBRACE               { let i = merge_inc $1 $3 in 
						 TCat(i,$2) 
					     }
  | LBRACK typeelt_list RBRACK               { let i = merge_inc $1 $3 in
					       let list = TVar(i, list_qid i) in
					       let t = TCat(i,$2) in
						 TApp(i,list, t)
					     }  
      
innertype: 
  | exp_core                                 { e2te $1 } 
  | atypeexp                                 { $1 } 

typeelt_list:
  |                                          { [] }
  | non_empty_typeelt_list                   { $1 }

non_empty_typeelt_list:
  | typeelt                                  { [$1] }
  | typeelt COMMA non_empty_typeelt_list     { $1::$3 }

typeelt:
  | aexp_core_but_name                       { e2te $1 }
  | aexp_core EQUAL innertype                { let i = info_of_exp $1 in 
						 TName(i, $1, $3)
					     }
  | name                                     { let i = info_of_id $1 in 
						 TName(i,EName(i,$1),emptyViewType i) 
					     }
      
excepts_opt :
  |                                         { [] }
  | LPAREN except_list RPAREN               { $2 }

except_list:
  |                                         { [] }
  | exp_core COMMA except_list              { $1::$3 }

/*** identifiers ***/
qid:
  | IDENT                                   { qid_of_id $1 }
  | qid DOT IDENT                           { dot $1 (qid_of_id $3) }

IDENT_list:
  |                                         { [] }
  | IDENT IDENT_list                        { $1::$2 }

name:
  | STRING                                  { $1 }

IDENT_or_STRING: 
  | IDENT                                   { $1 }
  | STRING                                  { $1 }

/*** EXTERNAL view parser */
ext_view: 
  | LBRACE ext_viewelt_list RBRACE          { Safelist.fold_right 
						(fun v vacc -> V.concat vacc v) 
						$2 
						V.empty 
					    }
  | LBRACK ext_viewelt_list RBRACK          { Safelist.fold_right 
						(fun v vacc -> V.cons v vacc) 
						$2 
						V.empty 
					    }
  
ext_viewelt_list:
  |                                        { [] }
  | ext_non_empty_viewelt_list             { $1 }

ext_non_empty_viewelt_list:
  | ext_viewelt                                  { [$1] }
  | ext_viewelt COMMA ext_non_empty_viewelt_list { $1::$3 }

ext_viewelt:
  | IDENT_or_STRING                          { let n = string_of_id $1 in
						 V.set V.empty n (Some V.empty)
					     }
  | IDENT_or_STRING EQUAL ext_innerview      { let n = string_of_id $1 in
						 V.set V.empty n (Some $3)
					     }
ext_innerview:
  | ext_view                                 { $1 }
  | IDENT_or_STRING                          { let n = string_of_id $1 in 
						 V.set V.empty n (Some V.empty)	  
					     }
