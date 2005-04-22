/************************************************************/
/* The Harmony Project                                      */
/* harmony@lists.seas.upenn.edu                             */
/*                                                          */
/* parser.mly - Focal parser generator                      */
/*                                                          */
/* $Id: parser.mly,v 1.1 2005/04/11 18:24:56 jnfoster Exp $ */
/*                                                          */
/************************************************************/

%{

open Error
open Syntax

let error t info = let (l,c1),(_,c2) = info in
  let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Error.Parse_error (s,info))
    else raise (Error.Parse_error (s^ ": " ^ t,info))
      
%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT STRING
%token <Info.t> LET IN FUN AND MODULE END OPEN TYPE
%token <Info.t> LENS VIEW TYPE NAME ARROW
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE
%token <Info.t> SEMI COMMA DOT EQUAL COLON BACKTICK
%token <Info.t> EMPTY STAR BANG QMARK BAR MINUS AMP 

%start modl sort qid
%type <Syntax.modl> modl
%type <Syntax.sort> sort
%type <Syntax.qid> qid

%%

/*** declarations ***/
modl:
  | MODULE IDENT EQUAL decls EOF               { MDef($1,$2,[],$4) }
  | MODULE IDENT EQUAL open_decls IN decls EOF { MDef($1,$2,$4,$6) }

open_decls:
  |                                            { [] }
  | OPEN qid open_decls                        { $2::$3 }

decls:
  |                                           { [] }
  | LET binding_list decls                    { (DLet($1,$2))::$3 }
  | TYPE typebinding_list decls               { (DType($1,$2))::$3 }
  | MODULE IDENT EQUAL decls END decls        { (DMod($1,$2,$4))::$6 }

binding_list:
  | binding_list AND binding                  { $1@[$3] }
  | binding                                   { [$1] }

binding:    
  | IDENT param_list opt_sort EQUAL exp       { BDef($4,$1,$2,$3,$5) }

typebinding:
  | IDENT IDENT_list EQUAL typeexp            { ($1, $2, $4) }

typebinding_list:
  | typebinding_list AND typebinding          { $1@[$3] }
  | typebinding                               { [$1] }

/*** sorts ***/

sort:
  | asort ARROW sort                          { SArrow ($2,$1,$3) }
  | asort                                     { $1 }

asort:
  | LENS                                      { SLens($1) }
  | VIEW                                      { SView($1) }
  | NAME                                      { SName($1) } 
  | LPAREN sort RPAREN                        { $2 }

opt_sort:
  |                                           { None }
  | COLON asort                               { Some $2 }

param_list:
  |                                           { [] }
  | param param_list                          { $1::$2 }

param:
  | IDENT COLON asort                         { PDef($2,$1,$3) }
  | LPAREN IDENT COLON sort RPAREN            { PDef($1,$2,$4) }

/*** expressions ***/
exp: 
  | LET binding_list IN exp                      { ELet($1,$2,$4) }
  | FUN param param_list opt_sort ARROW exp      { EFun($1,$2::$3,$4,$6) }
  | composeexp                                   { $1 }

composeexp:
  | composeexp SEMI appexp                    { let cid = ([],($2, "compose2")) in EApp($2,EApp($2,EVar($2,cid),$1),$3) }
  | appexp                                    { $1 } 

appexp:
  | appexp aexp                               { EApp(info_of_exp $1,$1,$2) }
  | aexp                                      { $1 }

aexp:
  | qid                                       { let (_,(i,_)) = $1 in EVar(i,$1) }
  | viewexp                                   { $1 }
  | LANGLE typeexp RANGLE                     { EType($1,$2) }
  | LPAREN exp RPAREN                         { $2 }
  | STRING                                    { let (i,_) = $1 in EName(i,$1) }                            
  | LBRACE map_list LBRACE                    { EMap($1, $2) }

map:
  | IDENT_or_STRING ARROW aexp                { ($1,$3) }

map_list:
  |                                           { [] }
  | map COMMA non_empty_map_list              { $1::$3 }

non_empty_map_list:
  | map                                       { [$1] }
  | map COMMA non_empty_map_list              { $1::$3 }
                                                 
/*** views ***/
viewexp:
  | LBRACE viewelt_list RBRACE                { EView($1,$2) }
  | LBRACK viewelt_list RBRACK                { list_view (EView($1,$2)) }
  
viewelt_list:
  |                                           { [] }
  | non_empty_viewelt_list                    { $1 }

non_empty_viewelt_list:
  | viewelt                                   { [$1] }
  | viewelt COMMA non_empty_viewelt_list      { $1::$3 }

viewelt:
  | IDENT_or_STRING                           { let (i,_) = $1 in (i, EName(i,$1), emptyView i) }
  | IDENT_or_STRING EQUAL innerview           { ($2, EName(get_info_id $1, $1), $3) }
  | BACKTICK exp BACKTICK EQUAL innerview     { ($1, $2, $5) }

innerview:
  | IDENT_or_STRING                           { let (i,x) = $1 in EView(i,[(i,EName(i,$1), emptyView i)]) }
  | viewexp                                   { $1 }
  | BACKTICK exp BACKTICK                     { $2 }
  
/*** types ***/
typeexp:
  | typeexp BAR ctypeexp                      { TUnion($2,[$1;$3]) }
  | typeexp AMP ctypeexp                      { TInter($2,$1,$3) }
  | typeexp MINUS ctypeexp                    { TDiff($2,$1,$3) }
  | ctypeexp                                  { $1 }

ctypeexp:
  | atypeexp COMMA ctypeexp                   { TCat($2, [$1;$3]) }
  | atypeexp                                  { $1 }

atypeexp:
  | EMPTY                                     { TEmpty($1) } 
  | LBRACE typeelt_list RBRACE                { TCat($1,$2) }
  | LBRACK typeelt_list RBRACK                { list_type (TCat($1,$2)) }
  | IDENT_or_STRING ARROW innertype           { let (i,_) = $1 in TName($2,EName(i,$1),$3) } 
  | BACKTICK exp BACKTICK ARROW innertype     { TName($1,$2,$5) } 
  | STAR except_list_opt ARROW innertype      { TStar($1,$2,$4) }
  | BANG except_list_opt ARROW innertype      { TBang($1,$2,$4) }
  | LPAREN typeexp RPAREN                     { $2 } 
  | BACKTICK exp BACKTICK                     { let i = info_of_exp $2 in TExp(i, $2) }

typeelt_list:
  |                                           { [] }
  | non_empty_typeelt_list                    { $1 }

non_empty_typeelt_list:
  | typeelt                                   { [$1] }
  | typeelt COMMA non_empty_typeelt_list      { $1::$3 }

typeelt:
  | valuetype                                 { $1 }
  | BACKTICK exp ARROW innertype              { TName($1, $2, $4) }
  | IDENT_or_STRING ARROW innertype           { let i = get_info_id $1 in TName($2, EName(i,$1), $3) }

innertype: 
  | valuetype                                 { $1 }
  | atypeexp                                  { $1 } 

valuetype:
  | IDENT_or_STRING                           { let i = get_info_id $1 in TName(i,EName(i,$1),emptyViewType i) }

/* stub productions */

except_list_opt :
  |                                           { [] }
  | MINUS except_list                         { $2 }

except_list:
  |                                           { [] }               
  | IDENT_or_STRING except_list               { $1::$2 }

/*** identifiers ***/
qid:
  | IDENT                                     { ([],$1) }
  | qid DOT IDENT                             { let l,i = $1 in (l@[i],$3) }

IDENT_list:
  |                                           { [] }
  | IDENT IDENT_list                          { $1::$2 }

IDENT_or_STRING:
  | IDENT                                     { let (i,x) = $1 in (i,x) }
  | STRING                                    { let (i,x) = $1 in (i,x) }
  
