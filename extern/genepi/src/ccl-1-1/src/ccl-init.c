/* $Id: ccl-init.c,v 1.2 2006/01/04 07:59:11 point Exp $ */
#include "ccl-debug.h"
#include "ccl-log.h"
#include "ccl-pool.h"
#include "ccl-string.h"
#include "ccl-init.h"

static int init_counter = 0;

void
ccl_init(uint32_t debug_level)
{
  if( init_counter++ == 0 )
    {
      ccl_debug_mask = debug_level;
      ccl_log_init();
      ccl_pool_init();
      ccl_string_init();
    }
}

			/* --------------- */

void
ccl_terminate(void)
{
  if( --init_counter == 0 )
    {
      ccl_string_terminate();
      ccl_pool_terminate();
      ccl_log_terminate();
      ccl_debug_mask = 0;
    }
}
