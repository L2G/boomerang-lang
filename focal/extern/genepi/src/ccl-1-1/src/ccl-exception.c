/* $Id: ccl-exception.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <stdio.h>
#include "ccl-log.h"
#include "ccl-assert.h"
#include "ccl-exception.h"


			/* --------------- */

static ccl_exception_type current_exception = NULL;
const char        *ccl_exception_current_message = NULL;
static const char        *current_file = NULL;
static int                current_line = 0;
#define current_message ccl_exception_current_message
static ccl_exception     *top_context = NULL;

			/* --------------- */

struct ccl_exception_type_st CCL_EXCEPTION_NAME(___toplevel_throwable) = { 
  "___toplevel_throwable",
  NULL
};

			/* --------------- */

CCL_DEFINE_EXCEPTION(error,___toplevel_throwable);
CCL_DEFINE_EXCEPTION(exception,___toplevel_throwable);
CCL_DEFINE_EXCEPTION(internal_error,error);
CCL_DEFINE_EXCEPTION(runtime_exception,exception);
CCL_DEFINE_EXCEPTION(division_by_zero,exception);

			/* --------------- */

void
ccl_exception_push(ccl_exception *e, const ccl_exception_type type)
{
  ccl_pre( e != NULL && type != NULL );

  e->type           = type;
  e->next           = top_context;
  top_context       = e;
  current_exception = NULL;
}

			/* --------------- */

void
ccl_exception_pop(void)
{
  ccl_pre( top_context != NULL );
  
  top_context = top_context->next;
}

			/* --------------- */

static int
s_inherits_from(const ccl_exception_type child, const ccl_exception_type type)
{
  if( child == NULL ) 
    return 0;
  
  return child == type || s_inherits_from(child->super,type); 
}

			/* --------------- */

int
ccl_exception_is_raised__(const ccl_exception_type type)
{
  ccl_pre( type != NULL );

  return s_inherits_from(current_exception,type);
}

			/* --------------- */

void
ccl_throw__(const ccl_exception_type ex,  const char *msg,
	   const char *file, int line)
{
  current_exception = ex;
  current_message   = msg;
  current_file      = file;
  current_line      = line;

  while( top_context != NULL )
    {
      if( s_inherits_from(ex,top_context->type) )
	  longjmp(top_context->context,1);
      top_context = top_context->next;
    }

  fprintf(stderr,"%s:%d: uncaught raised exception (%s) : '%s'\n",file,line,
	  ex->name,msg);
  abort();
}

			/* --------------- */

void
ccl_rethrow__(const char *file, int line)
{
  ccl_pre( current_exception != NULL );

  ccl_throw__(current_exception,current_message,file,line);
}

			/* --------------- */

void
ccl_exception_print(void)
{
  ccl_pre( current_exception != NULL );

  ccl_error("%s:%d: %s raised : %s\n",
	   current_file,current_line,
	   current_exception->name,current_message?current_message:"");
}
