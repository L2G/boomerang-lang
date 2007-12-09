/* $Id: ccl-debug.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <stdarg.h>
#include <stdio.h>
#include "ccl-log.h"
#include "ccl-debug.h"

uint32_t ccl_debug_mask = 0;

#ifndef NDEBUG
void
ccl_debug_printf(uint32_t mask, const char *s, ...) 
{
  va_list ap;

  if (!(ccl_debug_mask & mask))
    return;
  
  va_start(ap, s);
  ccl_log_va(CCL_LOG_DEBUG,s,ap);
  va_end(ap);
}
#endif


