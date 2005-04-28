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

let (@) = Safelist.append

let error t info = let (l,c1),(_,c2) = info in
  let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Error.Parse_error (s,info))
    else raise (Error.Parse_error (s^ ": " ^ t,info))

(* constants *)
let compose2_qid i = ([(i,"Pervasives"); (i,"Native")], (i, "compose2"))
let list_qid i = ([(i,"Pervasives")], (i, "List"))
      
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
  | MODULE IDENT EQUAL decls EOF               
      { let i = merge_inc $1 $5 in 
	  MDef(i,$2,[],$4) 
      }
  | MODULE IDENT EQUAL open_decls IN decls EOF 
      { let i = merge_inc $1 $7 in 
	  MDef(i,$2,$4,$6) 
      }
      
open_decls:
  |   { [] }
  | OPEN qid open_decls                        
      { $2::$3 }

decls:
  |   { [] }
  | LET binding_list decls                    
      { let i = merge_inc $1 (info_of_bindings $1 $2) in 
	  (DLet(i,$2))::$3 
      }
  | TYPE typebinding_list decls               
      { let i = merge_inc $1 (info_of_typebindings $1 $2) in 
	  (DType($1,$2))::$3 
      }
  | MODULE IDENT EQUAL decls END decls        
      { let i = merge_inc $1 $5 in 
	  (DMod($1,$2,$4))::$6 
      }
      
binding_list:
  | binding_list AND binding                  
      { $1@[$3] }
  | binding                                   
      {  [$1] }
      
binding:    
  | IDENT param_list opt_sort EQUAL exp       
      { let i = merge_inc (info_of_id $1) (info_of_exp $5) in 
	  BDef(i,$1,$2,$3,$5) 
      }
      
typebinding:
  | IDENT IDENT_list EQUAL typeexp            
      { ($1, $2, $4) }

typebinding_list:
  | typebinding_list AND typebinding 
      { $1@[$3] }
  | typebinding 
      { [$1] }

/*** sorts ***/

sort:
  | asort ARROW sort                          
      { let i = merge_inc (info_of_sort $1) (info_of_sort $3) in 
	  SArrow (i,$1,$3) 
      }
  | asort                                     
      { $1 }

asort:
  | LENS                                      
      { SLens($1) }
  | VIEW                                      
      { SView($1) }
  | NAME                                      
      { SName($1) } 
  | TYPE
      { SType($1) }
  | LPAREN sort RPAREN                        
      { $2 }

opt_sort:
  |   { None }
  | COLON asort                               
      { Some $2 }

param_list:
  |   { [] }
  | param param_list                          
      { $1::$2 }

param:
  | IDENT COLON asort                         
      { let i = merge_inc (info_of_id $1) (info_of_sort $3) in 
	  PDef(i,$1,$3) 
      }
  | LPAREN IDENT COLON sort RPAREN 
      { let i = merge_inc (info_of_id $2) (info_of_sort $4) in 
	  PDef($1,$2,$4) }

/*** expressions ***/
exp: 
  | LET binding_list IN exp                      
      { ELet($1,$2,$4) }
  | FUN param param_list opt_sort ARROW exp      
      { EFun($1,$2::$3,$4,$6) }
  | composeexp                                   
      { $1 }

composeexp:
  | composeexp SEMI appexp                    
      { let i = merge_inc (info_of_exp $1) (info_of_exp $3) in
	let c2_qid = compose2_qid i in
	  EApp(i,EApp(i,EVar(i,c2_qid),$1),$3)
      }
  | appexp                                    
      { $1 } 

appexp:
  | appexp aexp                               
      { EApp(info_of_exp $1,$1,$2) }
  | aexp                                      
      { $1 }

aexp:
  | name                                      
      { let (i,_) = $1 in 
	  EName(i,$1) 
      } 
  | qid                                       
      { let (_,(i,_)) = $1 in 
	  EVar(i,$1) 
      }
  | viewexp                                   
      { $1 }
  | LANGLE typeexp RANGLE                     
      { EType($1,$2) }
  | LPAREN exp RPAREN                         
      { $2 }
  | LBRACE map_list RBRACE                    
      { EMap($1, $2) }

map:
  | exp_core ARROW exp                        
      { ($1,$3) }

map_list:
  | map                                       
      { [$1] }
  | map COMMA map_list                        
      { $1::$3 }

/* subset of expressions: just identifiers, names, applications */
exp_core:
  | exp_core aexp_core                        
      { EApp(info_of_exp $1,$1,$2) }
  | aexp_core                                 
      { $1 }

aexp_core:
  | name                                      
      { let (i,_) = $1 in 
	  EName(i,$1) 
      } 
  | aexp_core_but_name                        
      { $1 }

aexp_core_but_name:  
  | qid                                       
      { let (_,(i,_)) = $1 in 
	  EVar(i,$1) 
      }
  | LPAREN exp_core RPAREN                    
      { $2 }
                                                 
/*** views ***/
viewexp:
  | LBRACE viewelt_list RBRACE                
      { EView($1,$2, false) }
  | LBRACK viewelt_list RBRACK                
      { (EView($1,$2, true)) }
  
viewelt_list:
  |   { [] }
  | non_empty_viewelt_list                    
      { $1 }

non_empty_viewelt_list:
  | viewelt                                   
      { [$1] }
  | viewelt COMMA non_empty_viewelt_list      
      { $1::$3 }

viewelt:
  | exp_core                                  
      { let i = info_of_exp $1 in 
	  (i, $1, emptyView i) 
      }
  | exp_core EQUAL innerview                  
      { let i = info_of_exp $1 in 
	  (i, $1, $3) 
      }

innerview:
  | viewexp                                   
      { $1 }
  | aexp_core_but_name                        
      { $1 }
  | name                                      
      { let (i,x) = $1 in 
	  EView(i,[(i,EName(i,$1), emptyView i)], false) 
      }
  
/*** types ***/
typeexp:
  | typeexp BAR ctypeexp                      
      { TUnion($2,[$1;$3]) }
/*   | typeexp AMP ctypeexp   */
/*       { TInter($2,$1,$3) } */
/*   | typeexp MINUS ctypeexp */
/*       { TDiff($2,$1,$3) }  */
  | ctypeexp                                  
      { $1 }

ctypeexp:
  | atypeexp COMMA ctypeexp                   
      { TCat($2, [$1;$3]) }
  | atypeexp                                  
      { $1 }

atypeexp:
  | EMPTY                                     
      { TEmpty($1) } 
  | aexp_core LBRACE innertype RBRACE         
      { let i = info_of_exp $1 in 
	  TName(i,$1,$3) 
      } 
  | STAR excepts_opt LBRACE innertype RBRACE  
      { TStar($1,$2,$4) }
  | BANG excepts_opt LBRACE innertype RBRACE  
      { TBang($1,$2,$4) }
  | LPAREN typeexp RPAREN                     
      { $2 } 
  | LBRACE typeelt_list RBRACE                
      { let i = merge_inc $1 $3 in 
	  TCat(i,$2) }
  | LBRACK typeelt_list RBRACK                
      { let i = merge_inc $1 $3 in
	let l_qid = list_qid i in
	  TExp(i, EApp(i, EVar(i, l_qid), EType(i, (TCat(i,$2)))))
      }  
      
typeelt_list:
  |   { [] }
  | non_empty_typeelt_list                    
      { $1 }

non_empty_typeelt_list:
  | typeelt                                   
      { [$1] }
  | typeelt COMMA non_empty_typeelt_list      
      { $1::$3 }

typeelt:
  | aexp_core_but_name                        
      { let i = info_of_exp $1 in 
	  TExp(i,$1) 
      }
  | exp_core EQUAL innertype 
      { let i = info_of_exp $1 in 
	  TName(i, $1, $3)
      }
  | name    
      { let i = info_of_id $1 in 
	  TName(i,EName(i,$1),emptyViewType i) 
      }
      
innertype: 
  | exp_core
      { let i = info_of_exp $1 in 
	  TExp(i,$1) 
      }
  | atypeexp  
      { $1 } 

excepts_opt :
  |   { [] }
  | LPAREN except_list RPAREN                 
      { $2 }

except_list:
  |   { [] }
  | exp_core COMMA except_list                
      { $1::$3 }


/*** identifiers ***/
qid:
  | IDENT                                     
      { qid_of_id $1 }
  | qid DOT IDENT                             
      { dot $1 (qid_of_id $3) }

IDENT_list:
  |   { [] }
  | IDENT IDENT_list                          
      { $1::$2 }

name:
  | STRING                                    
      { $1 }
