#include <ccl-memory.h>
#include "sataf-msa-p.h"

typedef struct msa_cache_record_st { 
  sataf_msa arg1;
  sataf_msa arg2;
  sataf_msa arg3;
  const void *aux;
  sataf_msa R;
} msa_cache_record;

			/* --------------- */

static uint32_t cache_filled_entries = 0;
static uint32_t number_of_collisions = 0;
static uint32_t number_of_insertions = 0;
static uint32_t number_of_successful_searches = 0;
static uint32_t number_of_searches = 0;

			/* --------------- */

#ifndef MSA_CACHE_SIZE
# define MSA_CACHE_SIZE (1024*1024+41)
#endif /* MSA_CACHE_SIZE */

# define H(a1,a2,a3,aux) \
  ((117*((uint32_t)a1)+1043*((uint32_t)a2)+2049*((uint32_t)a3)+ \
    4041*((uint32_t)aux)) % MSA_CACHE_SIZE)

static msa_cache_record *MSA_CACHE = NULL;


			/* --------------- */

void
sataf_msa_cache_init(void)
{
  cache_filled_entries = 0;
  number_of_collisions = 0;
  number_of_insertions = 0;
  number_of_successful_searches = 0;
  number_of_searches = 0;
  MSA_CACHE = ccl_new_array(msa_cache_record,MSA_CACHE_SIZE);
}

			/* --------------- */

static void
s_clear_record(msa_cache_record *rec)
{
  ccl_zdelete(sataf_msa_del_reference,rec->arg1);
  ccl_zdelete(sataf_msa_del_reference,rec->arg2);
  ccl_zdelete(sataf_msa_del_reference,rec->arg3);
  ccl_zdelete(sataf_msa_del_reference,rec->R);
  ccl_memzero(rec,sizeof(msa_cache_record));
}

			/* --------------- */

void
sataf_msa_cache_terminate(void)
{
  int i;
  msa_cache_record *rec = MSA_CACHE;

  for(i = 0; i < MSA_CACHE_SIZE; i++,rec++)
    s_clear_record(rec);
  ccl_delete(MSA_CACHE);
}

			/* --------------- */

# define DEACTIVATE_CACHE 0
sataf_msa
sataf_msa_cache_get(sataf_msa a1, sataf_msa a2, sataf_msa a3, const void *aux)
{
  if( DEACTIVATE_CACHE )
    return NULL;
  else
    {
  sataf_msa R = NULL;
  uint32_t index = H(a1,a2,a3,aux);
  msa_cache_record *rec = MSA_CACHE+index;

  number_of_searches++;
  
  if( rec->arg1 == a1 && rec->arg2 == a2 && rec->arg3 == a3 && 
      rec->aux == aux )
    {
      R = sataf_msa_add_reference(rec->R);
      number_of_successful_searches++;
    }

  return R;
}
}

			/* --------------- */

void
sataf_msa_cache_put(sataf_msa a1, sataf_msa a2, sataf_msa a3, const void *aux,
		    sataf_msa R)
{
  if( DEACTIVATE_CACHE ) return;
  else
    {
  uint32_t index = H(a1,a2,a3,aux);
  msa_cache_record *rec = MSA_CACHE+index;

  number_of_insertions++;
  
  if( rec->arg1 == NULL && rec->arg2 == NULL && rec->arg3 == NULL && 
      rec->aux == NULL )
    cache_filled_entries++;
  else
    {
      number_of_collisions++;
      s_clear_record(rec);
    }

  if( a1 != NULL )
    {
      rec->arg1 = sataf_msa_add_reference(a1);
      if( a2 != NULL )
	{
	  rec->arg2 = sataf_msa_add_reference(a2);
	  if( a3 != NULL )
	    {
	      rec->arg3 = sataf_msa_add_reference(a3);
	    }
	}
    }

  rec->aux = aux;

  rec->R = sataf_msa_add_reference(R);
}
}
			/* --------------- */

void
sataf_msa_cache_statistics(ccl_log_type log)
{
  ccl_log(log,"Usage of cache 'MSA operations'\n");  
  ccl_log(log," size       : %d\n",MSA_CACHE_SIZE);
  ccl_log(log," entries    : %d (%.2f%%)\n",cache_filled_entries,
	  100.0*(float)cache_filled_entries/(float)MSA_CACHE_SIZE);
  ccl_log(log," requests   : %d\n",number_of_searches);
  ccl_log(log," success    : %d (%f %%)\n",
	  number_of_successful_searches,
	  100.0*(float)number_of_successful_searches/
	  (float)number_of_searches);
  ccl_log(log," collisions : %d\n",number_of_collisions);
  ccl_log(log,"\n");
}
