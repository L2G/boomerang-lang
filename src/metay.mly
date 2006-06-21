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

%}

%token <Info.t> EOF 
%token <Syntax.id> IDENT
%token <Info.t> LBRACE RBRACE LBRACK RBRACK LBRACKPIPE RBRACKPIPE COMMA COLON EQUAL

%start tree
%type <Tree.t> tree

%%

tree:
  | atree COLON COLON tree         { Tree.cons $1 $4 }
  | atree                          { $1 }

atree: 
  | LBRACE treeelt_list RBRACE     { Safelist.fold_right 
				       (fun v vacc -> Tree.concat vacc v) 
				       $2 
				       Tree.empty 
				   }
  | LBRACK innertree_list RBRACK   { Safelist.fold_right 
				       (fun v vacc -> Tree.cons v vacc) 
				       $2 
				       Tree.empty_list
				   }
  
treeelt_list:
  |                                { [] }
  | non_empty_treeelt_list     { $1 }

non_empty_treeelt_list:
  | treeelt                              { [$1] }
  | treeelt COMMA non_empty_treeelt_list { $1::$3 }

 treeelt:
  | IDENT                          { let n = Syntax.string_of_id $1 in
				       Tree.set Tree.empty n (Some Tree.empty)
				   }
  | IDENT EQUAL innertree          { let n = Syntax.string_of_id $1 in
				       Tree.set Tree.empty n (Some $3)
				   }
innertree:
  | tree                           { $1 }
  | IDENT                          { let n = Syntax.string_of_id $1 in 
				       Tree.set Tree.empty n (Some Tree.empty)	  
				   }
      
innertree_list:
  |                                      { [] }
  | non_empty_innertree_list             { $1 }

non_empty_innertree_list:
  | innertree                                { [$1] }
  | innertree COMMA non_empty_innertree_list { $1::$3 }

