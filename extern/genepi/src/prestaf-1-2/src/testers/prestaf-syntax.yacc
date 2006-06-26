/* $Id: prestaf-syntax.yacc,v 1.2 2006/03/17 08:47:51 point Exp $ */
%{
#include "testers/prestaf-parser.h"
#include "testers/prestaf-interp.h"

static void prestaf_error(const char *error);
extern int prestaf_lex(void);
extern char *prestaf_text;
#define YYSTYPE ccl_parse_tree

#define NN(_type,_vtype,_child,_next) \
(NODES=ccl_parse_tree_create(_type,#_type,_vtype,prestaf_current_line,	\
                 prestaf_filename,\
                 _child,_next,NODES))

#define NE(_type,_child,_next) NN(_type,CCL_PARSE_TREE_EMPTY,_child,_next)
#define NID()  NN(PRESTAF_IDENT,CCL_PARSE_TREE_IDENT,NULL,NULL)
#define NINT() NN(PRESTAF_INTEGER,CCL_PARSE_TREE_INT,NULL,NULL)
#define NSTR() NN(PRESTAF_STRING,CCL_PARSE_TREE_STRING,NULL,NULL)

#define malloc ccl_malloc
#define realloc ccl_realloc
#define calloc ccl_calloc
#define free ccl_free

ccl_parse_tree NODES = NULL;
ccl_parse_tree TREE = NULL;
# define CLEAR_NODES() \
do { if( NODES != NULL ) ccl_parse_tree_delete_container(NODES); \
NODES = NULL; } while(0)

# define WARNING(_msg) ccl_log(CCL_WARNING,"%s:%d: %s\n",_msg\
                           prestaf_filename, \
                           prestaf_current_line);

extern int   prestaf_current_line;
extern char *prestaf_filename;

%}

%token  PRESTAF_TOK_EQUIV
%token  PRESTAF_TOK_IMPLY
%token  PRESTAF_TOK_AND
%token  PRESTAF_TOK_OR
%token  PRESTAF_TOK_EXISTS
%token  PRESTAF_TOK_FORALL
%token  PRESTAF_TOK_INTEGER
%token  PRESTAF_TOK_IDENT
%token  PRESTAF_TOK_LEQ
%token  PRESTAF_TOK_GEQ
%token  PRESTAF_TOK_NEQ
%token  PRESTAF_TOK_EQ
%token  PRESTAF_TOK_IN
%token  PRESTAF_TOK_DEF

%start start

%%

start : prestaf_statement_list { $$ = NE(PRESTAF_STATEMENT_LIST,$1,NULL); }


prestaf_statement_list : prestaf_statement ';' prestaf_statement_list
                        { $$ = $1; $1->next = $3; }
                       | prestaf_statement
                        { $$ = $1; }
                       ;

prestaf_statement : presburger_formula 
                  | prestaf_definition
                  ;

prestaf_definition : ident PRESTAF_TOK_DEF presburger_formula
                   { $$ = NE(PRESTAF_DEFINITION,$1,NULL); $1->next = $3; }
                   ;

presburger_formula : presburger_formula PRESTAF_TOK_OR formula_and
                   { $$ = NE(PRESTAF_OR,$1,NULL); $1->next = $3; }
                   | formula_and
                   { $$ = $1; }
                   ;

formula_and : formula_and PRESTAF_TOK_AND formula_eq
            { $$ = NE(PRESTAF_AND,$1,NULL); $1->next = $3; }
            | formula_eq
            { $$ = $1; }
            ;

formula_eq : formula PRESTAF_TOK_IMPLY formula
           { $$ = NE(PRESTAF_IMPLY,$1,NULL); $1->next = $3; }
           | formula PRESTAF_TOK_EQUIV formula
           { $$ = NE(PRESTAF_EQUIV,$1,NULL); $1->next = $3; }
           | formula
           { $$ = $1; }
           ;


formula : '!' formula 
        { $$ = NE(PRESTAF_NOT,$2,NULL); }
        | quantified_formula
        { $$ = $1; }
        | linear_equation 
        { $$ = $1; }
        | in_formula
        { $$ = $1; }
        | '(' presburger_formula ')'
        { $$ = $2; }
        | ident
        { $$ = $1; }
        ;

quantified_formula : PRESTAF_TOK_FORALL list_of_variables formula
                   { $$ = NE(PRESTAF_FORALL,$3,NULL); $3->next = $2; }
                   | PRESTAF_TOK_EXISTS list_of_variables formula
                   { $$ = NE(PRESTAF_EXISTS,$3,NULL); $3->next = $2; }
                   ;

in_formula : linear_term PRESTAF_TOK_IN '[' linear_term ',' linear_term ']' 
           { $$ = NE(PRESTAF_IN,$1,NULL); $1->next = $4; $4->next = $6; }
           ;

list_of_variables : ident 
                  { $$ = $1; }
                  | ident ',' list_of_variables
                  { $$ = $1; $1->next = $3;}
                  ;

linear_equation :  linear_term PRESTAF_TOK_EQ linear_term 
                 { $$ = NE(PRESTAF_EQ,$1,NULL); $1->next = $3; }
                 | linear_term '=' linear_term 
                 { $$ = NE(PRESTAF_EQ,$1,NULL); $1->next = $3; }
                 | linear_term PRESTAF_TOK_NEQ linear_term
                 { $$ = NE(PRESTAF_NEQ,$1,NULL); $1->next = $3; }
                 | linear_term '<' linear_term
                 { $$ = NE(PRESTAF_LT,$1,NULL); $1->next = $3; }
                 | linear_term PRESTAF_TOK_LEQ linear_term
                 { $$ = NE(PRESTAF_LEQ,$1,NULL); $1->next = $3; }
                 | linear_term '>' linear_term
                 { $$ = NE(PRESTAF_GT,$1,NULL); $1->next = $3; }
                 | linear_term PRESTAF_TOK_GEQ linear_term
                 { $$ = NE(PRESTAF_GEQ,$1,NULL); $1->next = $3; }
	         ;

linear_term : linear_term '+' factor 
              { $$ = NE(PRESTAF_PLUS,$1,NULL); $1->next = $3; }
            | linear_term '-' factor
              { $$ = NE(PRESTAF_MINUS,$1,NULL); $1->next = $3; }
            | factor
              { $$ = $1; }
            | '-' factor
              { $$ = NE(PRESTAF_NEG,$2,NULL); }
             ;

factor : integer '*' ident
         { $$ = NE(PRESTAF_FACTOR,$1,NULL); $1->next = $3; }
       | ident '*' integer
         { $$ = NE(PRESTAF_FACTOR,$3,NULL); $3->next = $1; }
       | ident
         { $$ = $1; }
       | integer
         { $$ = $1; }
;
 
ident : PRESTAF_TOK_IDENT
      { 
	$$ = NID(); 
	$$->value.id_value = ccl_string_make_unique(prestaf_text); 
	}
      ;
 
integer : PRESTAF_TOK_INTEGER
       { $$ = NINT(); $$->value.int_value = atoi(prestaf_text); }
       ;
%%


static void
prestaf_error(const char *message)
{
  CLEAR_NODES();
  ccl_log(CCL_LOG_ERROR,":%d:on token '%s'\n",prestaf_current_line,
	  prestaf_text);
  ccl_throw(prestaf_parse_exception,"");
}

			/* --------------- */


