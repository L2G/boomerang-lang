#include "ccl-assert.h"

CCL_DEFINE_EXCEPTION(assertion,error);

                        /* --------------- */

void
ccl_check_condition(int cond, const char *file, int line, const char *msg)
{
  if( cond ) 
    return;
  ccl_throw__(&CCL_EXCEPTION_NAME(assertion),msg,file,line);
}
