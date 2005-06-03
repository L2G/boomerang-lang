/************************************************************/
/* The Harmony Project                                      */
/* harmony@lists.seas.upenn.edu                             */
/*                                                          */
/* metay.mly - Focal meta parser generator                  */
/*                                                          */
/************************************************************/
/* $Id$ */

%{
open Error
open Info

let string_of_id (_,s) = s
let info_of_id (i,_) = i

%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK COMMA COLON EQUAL

%start view
%type <V.t> view

%%

view:
  | aview COLON COLON view         { V.cons $1 $4 }
  | aview                          { $1 }

aview: 
  | LBRACE viewelt_list RBRACE     { Safelist.fold_right 
				       (fun v vacc -> V.concat vacc v) 
				       $2 
				       V.empty 
				   }
  | LBRACK innerview_list RBRACK   { Safelist.fold_right 
				       (fun v vacc -> V.cons v vacc) 
				       $2 
				       V.empty_list
				   }
  
viewelt_list:
  |                                { [] }
  | non_empty_viewelt_list     { $1 }

non_empty_viewelt_list:
  | viewelt                              { [$1] }
  | viewelt COMMA non_empty_viewelt_list { $1::$3 }

 viewelt:
  | IDENT                          { let n = string_of_id $1 in
				       V.set V.empty n (Some V.empty)
				   }
  | IDENT EQUAL innerview          { let n = string_of_id $1 in
				       V.set V.empty n (Some $3)
				   }
innerview:
  | view                           { $1 }
  | IDENT                          { let n = string_of_id $1 in 
				       V.set V.empty n (Some V.empty)	  
				   }

innerview_list:
  |                                      { [] }
  | non_empty_innerview_list             { $1 }

non_empty_innerview_list:
  | innerview                                { [$1] }
  | innerview COMMA non_empty_innerview_list { $1::$3 }
