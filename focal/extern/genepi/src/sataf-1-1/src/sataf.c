/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.3 $
 * @date    : $Date: 2006/01/27 13:13:45 $
 */
#include <ccl-init.h>
#include <ccl-pool.h>
#include "shared-automaton.h"
#include "sataf.h"

			/* --------------- */

static int init_counter = 0;

void
sataf_init(void)
{
  if( init_counter++ == 0 )
    {
      sataf_automaton_init();
      exit_automaton_init();
      shared_automaton_init();
      sataf_msa_init();
    }
}

			/* --------------- */

void
sataf_terminate(void)
{
  if( --init_counter == 0 )
    {
      sataf_msa_terminate();
      shared_automaton_terminate();
      exit_automaton_terminate();
      sataf_automaton_terminate();
    }
}

			/* --------------- */

void
sataf_log_statistics(ccl_log_type log)
{
  exit_automaton_ut_statistics(log);
  shared_automaton_table_statistics(log);
  sataf_msa_ut_statistics(log);
  sataf_msa_cache_statistics(log);
  sataf_msa_sharing_cache_statistics(log);
  ccl_pools_display_info(log);
}
