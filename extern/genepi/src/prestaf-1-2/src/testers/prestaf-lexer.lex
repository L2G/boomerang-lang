/* $Id: prestaf-lexer.lex,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
%{
#include <stdlib.h>
#include <stdio.h>
#include <ccl-log.h>
#include "prestaf-syntax.tab.h"
#include "testers/prestaf-parser.h"

#define malloc ccl_malloc
#define realloc ccl_realloc
#define calloc ccl_calloc
#define free ccl_free

int prestaf_current_line = 0;
const char *prestaf_filename = NULL;

%}

%x comment

%option  always-interactive
%option  nounput

%%

"/*"			BEGIN(comment);
<comment>.		;
<comment>"*/"		BEGIN(INITIAL);
[ \t]			;
"//".*\n                { prestaf_current_line++; }
<*>\n			{ prestaf_current_line++; }


"in"		{ return(PRESTAF_TOK_IN); }
"::="		{ return(PRESTAF_TOK_DEF); }
"=="		{ return(PRESTAF_TOK_EQ); }
"<=>"		{ return(PRESTAF_TOK_EQUIV); }
"<="		{ return(PRESTAF_TOK_LEQ); }
">="		{ return(PRESTAF_TOK_GEQ); }
"!="		{ return(PRESTAF_TOK_NEQ); }
"=>"		{ return(PRESTAF_TOK_IMPLY); }
"->"		{ return(PRESTAF_TOK_IMPLY); }
"&&"		{ return(PRESTAF_TOK_AND); }
"||"		{ return(PRESTAF_TOK_OR); }
"E."		{ return(PRESTAF_TOK_EXISTS); }
"A."		{ return(PRESTAF_TOK_FORALL); }
"exists"	{ return(PRESTAF_TOK_EXISTS); }
"forall"	{ return(PRESTAF_TOK_FORALL); }

[0-9]+			{ return (PRESTAF_TOK_INTEGER); }; 
[0-9_]*[A-Za-z_\^][A-Za-z0-9_\^]*  { return( PRESTAF_TOK_IDENT ); }; 
.                       return(yytext[0]);

%%

int
prestaf_wrap(void)
{
  return 1;
}

			/* --------------- */

extern int prestaf_parse(void);
extern ccl_parse_tree NODES;

			/* --------------- */

CCL_DEFINE_EXCEPTION(prestaf_parse_exception,exception);

			/* --------------- */

ccl_parse_tree
prestaf_load_file(const char *filename)
{
  FILE *stream = fopen(filename,"r");
  void  *state = NULL;
  ccl_parse_tree result = NULL;


  NODES = NULL;
  prestaf_filename = filename;

  if( stream == NULL )
    {
      ccl_log(CCL_LOG_ERROR,"can't open file '%s'\n",filename);
      ccl_throw(exception,"");
    }

  if( (state = yy_create_buffer(stream,YY_BUF_SIZE)) == NULL )
    {
      fclose(stream);
      ccl_throw(exception,"memory error");
    }

  yy_switch_to_buffer(state);

  ccl_try(prestaf_parse_exception)
    { 
      prestaf_parse(); 
    }
  ccl_catch 
    { 
      fclose(stream);
      ccl_rethrow();
    }
  ccl_end_try;

  yy_delete_buffer(state);
  fclose(stream);

  result = NODES;
  NODES = NULL;

  return result;
}
