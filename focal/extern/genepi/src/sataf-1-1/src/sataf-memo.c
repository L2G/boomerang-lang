/* $Id: sataf-memo.c,v 1.3 2005/12/12 10:55:05 point Exp $ */
#include <ccl-assert.h>
#include <ccl-iterator.h>
#include <ccl-hash.h>
#include <ccl-pool.h>
#include "sataf-memo.h"

# define MEMO_POOL_PAGE_SIZE 1000

struct sataf_memorizer_st {
  ccl_hash ht;
  size_t memo_size;
  sataf_memo_init_proc init;
  void *init_data;
  sataf_memo_clean_proc clean;
  ccl_pool memo_pool;
};

			/* --------------- */

static unsigned int 
s_hash_automaton(const void *key);

static int 
s_compare_automata(const void *a1, const void *a2);

static sataf_memo *
s_new_memo(sataf_memorizer M);

static void
s_delete_memo(sataf_memorizer M, sataf_memo *m);

			/* --------------- */

sataf_memorizer
sataf_memorizer_create(size_t            memo_size,
		       sataf_memo_init_proc   init,
		       void             *init_data,
		       sataf_memo_clean_proc clean)
{
  sataf_memorizer result;

  ccl_pre( memo_size >= sizeof(sataf_memo) );

  result = ccl_new(struct sataf_memorizer_st);
  result->ht = ccl_hash_create(s_hash_automaton,s_compare_automata,NULL,NULL);
  result->memo_size = memo_size;
  result->init = init;
  result->init_data = init_data;
  result->clean = clean;
  result->memo_pool = ccl_pool_create("Memo Pool",memo_size,
				     MEMO_POOL_PAGE_SIZE);

  return result;
}

			/* --------------- */

void
sataf_memorizer_delete(sataf_memorizer M)
{
  ccl_pointer_iterator *i = ccl_hash_get_elements(M->ht);

  while( ccl_iterator_has_more_elements(i) )
    {
      sataf_memo *m = (sataf_memo *)ccl_iterator_next_element(i);
      s_delete_memo(M,m);
    }
  ccl_iterator_delete(i);
  ccl_hash_delete(M->ht);
  ccl_pool_delete(M->memo_pool);
  ccl_delete(M);
}

			/* --------------- */

sataf_memo *
sataf_memorizer_remind(sataf_memorizer M, sataf_automaton A)
{
  sataf_memo *result;

  if( ccl_hash_find(M->ht,A) )
    result = (sataf_memo *)ccl_hash_get(M->ht);
  else
    {
      result = s_new_memo(M);
      result->A = sataf_automaton_add_reference(A);
      if( M->init != NULL )
	M->init(result,M->init_data);

      ccl_hash_insert(M->ht,result);
    }

  return result;
}

			/* --------------- */

sataf_memo *
sataf_memorizer_succ(sataf_memorizer M, sataf_automaton A, int letter)
{
  sataf_automaton tmp = sataf_automaton_succ(A,letter);
  sataf_memo       *R = sataf_memorizer_remind(M,tmp);

  sataf_automaton_del_reference(tmp);

  return R;
}

			/* --------------- */

static unsigned int 
s_hash_automaton(const void *key)
{
  ccl_pre( key != NULL );

  return (unsigned int)sataf_automaton_hashcode((sataf_automaton)key);
}

			/* --------------- */

static int 
s_compare_automata(const void *a1, const void *a2)
{
  ccl_pre( a1 != NULL ); 
  ccl_pre( a2 != NULL );

  if( sataf_automaton_equals((sataf_automaton)a1,(sataf_automaton)a2) )
    return 0;
  else if( a1 < a2 )
    return -1;
  else
    return 1;
}

			/* --------------- */

static sataf_memo *
s_new_memo(sataf_memorizer M)
{
  sataf_memo *r = (sataf_memo *)ccl_pool_alloc(M->memo_pool);

  ccl_memzero(r,sizeof(sataf_memo));

  return r;
}

			/* --------------- */

static void
s_delete_memo(sataf_memorizer M, sataf_memo *m)
{
  if( M->clean != NULL )
    M->clean(m);
  sataf_automaton_del_reference(m->A);

  ccl_pool_release(M->memo_pool,m);
}
