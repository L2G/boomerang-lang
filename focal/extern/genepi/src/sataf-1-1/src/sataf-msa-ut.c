/* $Id: sataf-msa-ut.c,v 1.4 2006/01/25 16:13:30 point Exp $ */
#include <ccl-pool.h>
#include "sataf-msa-p.h"
#include "sataf-msa.h"

# define MSA_POOL_PAGE_SIZE 1000
# define MSA_UT_INIT_TABLE_SIZE 1403641
# define MSA_UT_FILL_DEGREE 5

typedef struct msa_manager_st {
  uint32_t table_size;
  sataf_msa *table;
  uint32_t nb_elements;
  ccl_pool pool; 
  sataf_msa current;
} msa_manager;

			/* --------------- */

#define LAZY_DEL_REF
#define HVALUE MSA_HVALUE

#define MUST_INCREASE_TABLE() \
( MSA_MANAGER->nb_elements > MSA_MANAGER->table_size*MSA_UT_FILL_DEGREE)

#define MUST_DECREASE_TABLE() \
( MSA_MANAGER->nb_elements < MSA_MANAGER->table_size*MSA_UT_FILL_DEGREE  \
 && MSA_MANAGER->table_size > MSA_UT_INIT_TABLE_SIZE)

			/* --------------- */

static msa_manager *MSA_MANAGER = NULL;

			/* --------------- */

static void
s_msa_manager_resize_table(uint32_t new_size);

			/* --------------- */

void
sataf_msa_unique_table_init(void)
{
  MSA_MANAGER = ccl_new(msa_manager);

  MSA_MANAGER->table_size  = MSA_UT_INIT_TABLE_SIZE;
  MSA_MANAGER->table = ccl_new_array(sataf_msa,MSA_MANAGER->table_size);
  MSA_MANAGER->nb_elements = 0;
  MSA_MANAGER->pool = ccl_pool_create("MSA Pool",sizeof(struct sataf_msa_st),
				      MSA_POOL_PAGE_SIZE);
  MSA_MANAGER->current = NULL;
}

			/* --------------- */

void
sataf_msa_unique_table_terminate(void)
{
#ifdef LAZY_DEL_REF
  do
    {
      int i = MSA_MANAGER->table_size;
      
      while( i-- )
	{
	  sataf_msa msa, next;
	  
	  for(msa = MSA_MANAGER->table[i]; msa; msa = next)
	    {
	      next = msa->next;
	      if( msa->A->refcount )
		shared_automaton_del_reference(msa->A);
	    }
	}
    }
  while( MSA_MANAGER->nb_elements != 0 );
#else
  ccl_assert( MSA_MANAGER->nb_elements == 0 );
#endif

  ccl_delete(MSA_MANAGER->table);
  ccl_pool_delete(MSA_MANAGER->pool);
  ccl_delete(MSA_MANAGER);
}

			/* --------------- */

void
sataf_msa_del_reference_(sataf_msa msa)
{  
  if( msa == MSA_MANAGER->current )
    MSA_MANAGER->current = NULL;

#ifndef LAZY_DEL_REF
  {
    sataf_msa  *pmsa;
    uint32_t index = HVALUE(msa->A,msa->initial) % 
      MSA_MANAGER->table_size;
  
    for(pmsa = MSA_MANAGER->table+index; *pmsa && *pmsa != msa; 
	pmsa = &((*pmsa)->next))
      /* do nothing */;
  
    ccl_assert( *pmsa != NULL );
    *pmsa = msa->next;
    shared_automaton_del_reference(msa->A);
    
    ccl_pool_release(MSA_MANAGER->pool,msa);
  }
#endif

  MSA_MANAGER->nb_elements--;
  
  if( MUST_DECREASE_TABLE() )
    s_msa_manager_resize_table((MSA_MANAGER->table_size-13)>>1);
}

			/* --------------- */

sataf_msa
sataf_msa_find_or_add(shared_automaton sa, uint32_t  initial)
{
  sataf_msa msa;
  uint32_t index;
  sataf_msa *pmsa;

  if( MSA_MANAGER->current != NULL && 
      MSA_MANAGER->current->A == sa && 
      MSA_MANAGER->current->initial == initial )
    {
      ccl_assert( MSA_MANAGER->current->refcount > 0 );
      return sataf_msa_add_reference(MSA_MANAGER->current);
    }

  index = HVALUE(sa,initial) % MSA_MANAGER->table_size;
  pmsa = MSA_MANAGER->table+index; 

  while( *pmsa )
    {
      msa = *pmsa;

#ifdef LAZY_DEL_REF
      if( msa->refcount == 0 )
	{
	  shared_automaton_del_reference(msa->A);
	  *pmsa = msa->next;
	  ccl_pool_release(MSA_MANAGER->pool,msa);
	}
      else 
#endif
	if( msa->A == sa && msa->initial == initial )
	{
	  return sataf_msa_add_reference(msa);
	}
      else
	{
	  pmsa = &(msa->next);
	}
    }

  msa = (sataf_msa)ccl_pool_alloc(MSA_MANAGER->pool);
  msa->next     = NULL;
  msa->refcount = 1;
  msa->A        = shared_automaton_add_reference(sa);
  msa->initial  = initial;
  
  *pmsa = msa;

  MSA_MANAGER->nb_elements++;

  if( MUST_INCREASE_TABLE() )
    s_msa_manager_resize_table((MSA_MANAGER->table_size<<1)+13);

  MSA_MANAGER->current = msa;

  return msa;
}

			/* --------------- */

void
sataf_msa_ut_statistics(ccl_log_type log)
{
  int i;
  uint32_t nb_queue = 0;
  uint32_t nb_refcount = 0;
  uint32_t min_refcount = 0xFFFFFFFF;
  uint32_t max_refcount = 0;

  for(i = 0; i < MSA_UT_INIT_TABLE_SIZE; i++)
    {
      sataf_msa msa = MSA_MANAGER->table[i];

      if( msa == NULL ) 
	continue;

      nb_queue++;
      for(; msa != NULL; msa = msa->next)
	{
	  nb_refcount += msa->refcount;
	  if( msa->refcount < min_refcount )
	    min_refcount = msa->refcount;
	  if( msa->refcount > max_refcount )
	    max_refcount = msa->refcount;
	}
    }

  ccl_log(log,"MSA uniqueness table statistics :\n");
  ccl_log(log,"---------------------------------\n");
  ccl_log(log,"init size = %d\n",MSA_UT_INIT_TABLE_SIZE);
  ccl_log(log,"fill degree = %d\n",MSA_UT_FILL_DEGREE);
  ccl_log(log,"table size = %d\n",MSA_MANAGER->table_size);
  ccl_log(log,"nb entries = %d\n",MSA_MANAGER->nb_elements);
  ccl_log(log,"mean collision list len = %4.2f\n",
	  (float)MSA_MANAGER->nb_elements/(float)nb_queue);
  ccl_log(log,"mean sharing count = %4.2f\n",
	  (float)nb_refcount/(float)MSA_MANAGER->nb_elements);
  ccl_log(log,"refcount range = [%d, %d]\n",min_refcount,max_refcount);
  ccl_log(log,"\n");
}

			/* --------------- */

static void
s_msa_manager_resize_table(uint32_t new_size)
{
  uint32_t         i;
  sataf_msa *new_table = ccl_new_array(sataf_msa,new_size);
  sataf_msa        msa;
  sataf_msa       next;
  uint32_t     index;

  if( new_table == NULL )
    return;

  for(i = 0; i < MSA_MANAGER->table_size; i++)
    {
      for(msa = MSA_MANAGER->table[i]; msa; msa = next)
	{
	  next  = msa->next;
#ifdef LAZY_DEL_REF
	  if( msa->refcount == 0 )
	    {
	      shared_automaton_del_reference(msa->A);
	      ccl_pool_release(MSA_MANAGER->pool,msa);
	    }
	  else
#endif
	    {
	      index = HVALUE(msa->A,msa->initial) % new_size;
	      msa->next = new_table[index];
	      new_table[index] = msa;
	    }
	}
    }
  MSA_MANAGER->table_size = new_size;
  ccl_delete(MSA_MANAGER->table);
  MSA_MANAGER->table = new_table;
}

			/* --------------- */

