/* $Id: sataf-automaton-sharing.c,v 1.8 2006/01/27 13:13:56 point Exp $ */
#include <ccl-pool.h>
#include <ccl-hash.h>
#include "sataf-memo.h"
#include "sataf-msa-p.h"
#include "shared-automaton.h"
#include "sataf.h"
#include "homomorphism-checking.h"


static const int PRIME_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361,
   2729, 5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641,
   2807303, 5614657, 11229331
};

# define PRIME_TABLE_SIZE (sizeof(PRIME_TABLE)/PRIME_TABLE[0])

# define CACHE_SIZE 11229331

			/* --------------- */

sataf_automaton SATAF_ONE = (sataf_automaton)&SATAF_ONE;

sataf_automaton SATAF_ZERO = (sataf_automaton)&SATAF_ZERO;

			/* --------------- */

# define SBSCC_STATE_POOL_PAGE_SIZE 4000
# define STATE_SPACE_TABLE_SIZE_INDEX  14
# define STATE_SPACE_FILL_DEGREE 5

# define STATE_COMMON_FIELDS \
  state       *next; \
  sataf_automaton A; \
  uint32_t     id; \
  int  exit_index; \
  uint32_t   hval

typedef struct state_st state;
struct state_st {
  STATE_COMMON_FIELDS;
};

typedef struct state_before_scc_st {
  STATE_COMMON_FIELDS;
  uint32_t       min;
  uint32_t    letter;
  state      *dfs_next;
  state      *scc_next;
  state          *succ[1];
} state_before_scc;

typedef struct state_after_scc_st {
  STATE_COMMON_FIELDS;
  state     *next_aux;
  shared_automaton sa;
} state_after_scc;

typedef struct state_space_st {
  state **state_table;
  uint32_t state_table_size;
  uint32_t table_size_index;
  uint32_t nb_states;  
  ccl_pool state_pool;
} state_space;

			/* --------------- */

# define IS_SASCC(_m) CCL_PTRHASBIT(((state *)(_m))->A)
# define SBSCC(_m) ((state_before_scc *)(_m))
# define SASCC(_m) ((state_after_scc *)(_m))

			/* --------------- */

static int SIZE_FOR_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361, 2729, 
  5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641, 2807303,
  5614657, 11229331
};

static const size_t SIZE_FOR_TABLE_SIZE = 
  sizeof(SIZE_FOR_TABLE)/sizeof(SIZE_FOR_TABLE[0]);

			/* --------------- */

static state_space *
s_state_space_create(uint32_t sigma_size);

static state *
s_find_or_add_state(state_space *sp, sataf_automaton A);

static state *
s_state_succ(state_space *sp, sataf_automaton A, uint32_t letter);

static void
s_state_space_delete(state_space *sp);

			/* --------------- */

static void
s_compile_scc(uint32_t                sigma_size, 
	      state                             *S, 
	      state                   **pscc_stack,	      
	      sataf_shared_automaton_transformer T,
	      uint32_t           *tmp_array_size,
	      uint32_t               **tmp_array,
	      uint32_t        *tmp_sa_array_size,
	      shared_automaton      **tmp_sa_array,
	      state_space                      *sp);

static void
s_compile_scc_with_cache(uint32_t                sigma_size, 
			 state                             *S, 
			 state                   **pscc_stack,	      
			 sataf_shared_automaton_transformer T,
			 uint32_t           *tmp_array_size,
			 uint32_t               **tmp_array,
			 uint32_t        *tmp_sa_array_size,
			 shared_automaton      **tmp_sa_array,
			 state_space                      *sp);


			/* --------------- */

static void
s_cache_init(void);
static void
s_cache_terminate(void);
static shared_automaton
s_cache_get(sataf_automaton a, uint32_t *pinit);
static void
s_cache_put(sataf_automaton a, shared_automaton R, uint32_t initial);

			/* --------------- */


void
sataf_msa_sharing_init(void)
{
  s_cache_init();
}

			/* --------------- */

void
sataf_msa_sharing_terminate(void)
{
  s_cache_terminate();
}

			/* --------------- */

sataf_msa
sataf_msa_compute_with_transformer2(sataf_automaton                    A, 
				   sataf_shared_automaton_transformer T)
{
  sataf_msa               R;
  uint32_t     sigma_size;
  int                     r;
  int                letter;
  state_space           *sp;
  state          *dfs_stack = NULL;
  state          *scc_stack = NULL;
  state              *start;
  state               *succ;

  uint32_t           tmp_array_size;
  uint32_t               *tmp_array;
  uint32_t        tmp_sa_array_size;
  shared_automaton      *tmp_sa_array;
  ccl_list  scc;
  

  ccl_pre( A != NULL );

  if( strcmp(sataf_automaton_get_type(A),SATAF_MSA_ID) == 0 )
    return sataf_msa_find_or_add(((SSA *)A)->A,((SSA *)A)->initial);

  r = 0;
  sigma_size = sataf_automaton_get_alphabet_size(A);

  sp = s_state_space_create(sigma_size);
  scc = ccl_list_create();

  start = s_find_or_add_state(sp,A);

  SBSCC(start)->id       = r;
  SBSCC(start)->min      = r++;
  SBSCC(start)->letter   = 0;
  SBSCC(start)->dfs_next = dfs_stack;
  SBSCC(start)->scc_next = scc_stack;
  dfs_stack              = start;
  scc_stack              = start;

  tmp_array_size = 1000;
  tmp_array = ccl_new_array(uint32_t,tmp_array_size);
  tmp_sa_array_size = 1000;
  tmp_sa_array = ccl_new_array(shared_automaton,tmp_sa_array_size);
  
  while( dfs_stack != NULL )
    {
      ccl_assert( ! IS_SASCC(dfs_stack) );

      if( SBSCC(dfs_stack)->letter < sigma_size )
	{
	  letter = SBSCC(dfs_stack)->letter++;
	  succ = s_state_succ(sp,dfs_stack->A,letter);
	 
	  SBSCC(dfs_stack)->succ[letter] = succ;

	  if( IS_SASCC(succ) )
	    {	      
	      s_cache_put(CCL_BITPTR2PTR(sataf_automaton,succ->A),
			  SASCC(succ)->sa,SASCC(succ)->id);      
	      continue;
	    }

	  if( SBSCC(succ)->id == 0xFFFFFFFF )
	    {
	      /* not yet visited */
	      SBSCC(succ)->id       = r;
	      SBSCC(succ)->min      = r++;
	      SBSCC(succ)->letter   = 0;
	      SBSCC(succ)->dfs_next = dfs_stack;
	      SBSCC(succ)->scc_next = scc_stack;
	      dfs_stack             = succ;
	      scc_stack             = succ;
	    }
	  else if( SBSCC(succ)->letter != 0xFFFFFFFF ) 
	    {
	      /* still in scc stack */
	      if( SBSCC(succ)->id < SBSCC(dfs_stack)->min )
		SBSCC(dfs_stack)->min = SBSCC(succ)->id;
	    }
	}
      else /* all successors are visited */
	{
	  state *next = SBSCC(dfs_stack)->dfs_next;

	  if( next != NULL )
	    {
	      ccl_assert( ! IS_SASCC(next) );
	      if( SBSCC(dfs_stack)->min < SBSCC(next)->min )
		SBSCC(next)->min = SBSCC(dfs_stack)->min;
	    }

	  if( SBSCC(dfs_stack)->min == SBSCC(dfs_stack)->id )
	    { 
	      /* the current state is the root of a SCC */
	      s_compile_scc(sigma_size,dfs_stack,&scc_stack,T,
			    &tmp_array_size,&tmp_array,
			    &tmp_sa_array_size,&tmp_sa_array,sp);
	      ccl_list_add(scc,SASCC(dfs_stack)->sa);
	    }
	  dfs_stack = next;	  
	}
    }

  ccl_assert( IS_SASCC(start) );
  
  ccl_delete(tmp_array);
  ccl_delete(tmp_sa_array);
  R = sataf_msa_find_or_add(SASCC(start)->sa,SASCC(start)->id);
  s_state_space_delete(sp);

  {
    ccl_pair p;
    for(p = FIRST(scc); p; p = CDR(p))
      shared_automaton_del_reference((shared_automaton)CAR(p));
    ccl_list_delete(scc);
  }

  return R;
}

			/* --------------- */

#define NEW_ALGO

#ifndef NEW_ALGO
sataf_msa
sataf_msa_compute_with_transformer(sataf_automaton                    A, 
				    sataf_shared_automaton_transformer T)
{
  sataf_msa               R;
  uint32_t     sigma_size;
  int                     r;
  int                letter;
  state_space           *sp;
  state          *dfs_stack = NULL;
  state          *scc_stack = NULL;
  state              *start;
  state               *succ;

  uint32_t           tmp_array_size;
  uint32_t               *tmp_array;
  uint32_t        tmp_sa_array_size;
  shared_automaton      *tmp_sa_array;
  ccl_list  scc;
  sataf_msa msa;

  static int Z = 0;

  ccl_pre( A != NULL );

  if( strcmp(sataf_automaton_get_type(A),SATAF_MSA_ID) == 0 )
    return sataf_msa_find_or_add(((SSA *)A)->A,((SSA *)A)->initial);

  r = 0;
  sigma_size = sataf_automaton_get_alphabet_size(A);

  sp = s_state_space_create(sigma_size);
  scc = ccl_list_create();

  start = s_find_or_add_state(sp,A);

  SBSCC(start)->id       = r;
  SBSCC(start)->min      = r++;
  SBSCC(start)->letter   = 0;
  SBSCC(start)->dfs_next = dfs_stack;
  SBSCC(start)->scc_next = scc_stack;
  dfs_stack              = start;
  scc_stack              = start;

  tmp_array_size = 1000;
  tmp_array = ccl_new_array(uint32_t,tmp_array_size);
  tmp_sa_array_size = 1000;
  tmp_sa_array = ccl_new_array(shared_automaton,tmp_sa_array_size);
  
  while( dfs_stack != NULL )
    {
      ccl_assert( ! IS_SASCC(dfs_stack) );

      if( SBSCC(dfs_stack)->letter < sigma_size )
	{
	  letter = SBSCC(dfs_stack)->letter++;
	  succ = s_state_succ(sp,dfs_stack->A,letter);
	 	  
	  SBSCC(dfs_stack)->succ[letter] = succ;
	  
	  if( IS_SASCC(succ) )
	    {
	      s_cache_put(CCL_BITPTR2PTR(sataf_automaton,succ->A),
			  SASCC(succ)->sa,SASCC(succ)->id);      
	      continue;
	    }

	  if( SBSCC(succ)->id == 0xFFFFFFFF )
	    {
	      /* not yet visited */
	      SBSCC(succ)->id       = r;
	      SBSCC(succ)->min      = r++;
	      SBSCC(succ)->letter   = 0;
	      SBSCC(succ)->dfs_next = dfs_stack;
	      SBSCC(succ)->scc_next = scc_stack;
	      dfs_stack             = succ;
	      scc_stack             = succ;
	    }
	  else if( SBSCC(succ)->letter != 0xFFFFFFFF ) 
	    {
	      /* still in scc stack */
	      if( SBSCC(succ)->id < SBSCC(dfs_stack)->min )
		SBSCC(dfs_stack)->min = SBSCC(succ)->id;
	    }
	}
      else /* all successors are visited */
	{
	  state *next = SBSCC(dfs_stack)->dfs_next;

	  if( next != NULL )
	    {
	      ccl_assert( ! IS_SASCC(next) );
	      if( SBSCC(dfs_stack)->min < SBSCC(next)->min )
		SBSCC(next)->min = SBSCC(dfs_stack)->min;
	    }

	  if( SBSCC(dfs_stack)->min == SBSCC(dfs_stack)->id )
	    { 
	      /* the current state is the root of a SCC */
	      s_compile_scc_with_cache(sigma_size,dfs_stack,&scc_stack,T,
				       &tmp_array_size,&tmp_array,
				       &tmp_sa_array_size,&tmp_sa_array,
				       sp);
	      ccl_list_add(scc,SASCC(dfs_stack)->sa);
	    }
	  dfs_stack = next;	  
	}
    }

  ccl_assert( IS_SASCC(start) );
  
  ccl_delete(tmp_array);
  ccl_delete(tmp_sa_array);
  R = sataf_msa_find_or_add(SASCC(start)->sa,SASCC(start)->id);
  s_state_space_delete(sp);

  {
    ccl_pair p;
    for(p = FIRST(scc); p; p = CDR(p))
      shared_automaton_del_reference((shared_automaton)CAR(p));
    ccl_list_delete(scc);
  }

  if( 0 ) {
    sataf_msa R2 = sataf_msa_compute_with_transformer2(A,T);
    ccl_assert( R2 == R );
    sataf_msa_del_reference(R2);
  }

  return R;
}

#else
			/* --------------- */

static void
s_delete_sa(void *p)
{
  shared_automaton_del_reference((shared_automaton)p);
}

			/* --------------- */

static void
s_delete_scc_list(ccl_list l)
{
  while(! ccl_list_is_empty(l) )
    {
      shared_automaton sa = (shared_automaton)ccl_list_take_first(l);
      shared_automaton_del_reference(sa);
    }
  ccl_list_delete(l);
}

			/* --------------- */

sataf_msa
sataf_msa_compute_with_transformer(sataf_automaton                    A, 
				   sataf_shared_automaton_transformer T)
{
  sataf_msa               R;
  uint32_t     sigma_size;
  int                     r;
  int                letter;
  state_space           *sp;
  state          *dfs_stack = NULL;
  state          *scc_stack = NULL;
  state              *start;
  state               *succ;

  uint32_t           tmp_array_size;
  uint32_t               *tmp_array;
  uint32_t        tmp_sa_array_size;
  shared_automaton      *tmp_sa_array;
  ccl_hash scc;
  

  ccl_pre( A != NULL );

  if( strcmp(sataf_automaton_get_type(A),SATAF_MSA_ID) == 0 )
    return sataf_msa_find_or_add(((SSA *)A)->A,((SSA *)A)->initial);

  r = 0;
  sigma_size = sataf_automaton_get_alphabet_size(A);

  sp = s_state_space_create(sigma_size);
  scc = ccl_hash_create(NULL,NULL,NULL,(ccl_delete_proc)s_delete_sa);

  start = s_find_or_add_state(sp,A);

  SBSCC(start)->id       = r;
  SBSCC(start)->min      = r++;
  SBSCC(start)->letter   = 0;
  SBSCC(start)->dfs_next = dfs_stack;
  SBSCC(start)->scc_next = scc_stack;
  dfs_stack              = start;
  scc_stack              = start;

  tmp_array_size = 1000;
  tmp_array = ccl_new_array(uint32_t,tmp_array_size);
  tmp_sa_array_size = 1000;
  tmp_sa_array = ccl_new_array(shared_automaton,tmp_sa_array_size);
  
  while( dfs_stack != NULL )
    {
      uint32_t sa_init;
      shared_automaton sa;

      ccl_assert( ! IS_SASCC(dfs_stack) );

      if( (sa = s_cache_get(SBSCC(dfs_stack)->A,&sa_init)) != NULL )
	{
	  sataf_automaton A = dfs_stack->A;
	  state *next = SBSCC(dfs_stack)->dfs_next;

	  ccl_assert( SBSCC(dfs_stack)->letter == 0 );

	  if( next != NULL )
	    {
	      ccl_assert( ! IS_SASCC(next) );
	      if( SBSCC(dfs_stack)->min < SBSCC(next)->min )
		SBSCC(next)->min = SBSCC(dfs_stack)->min;
	    }

	  ccl_assert( SBSCC(dfs_stack)->min == SBSCC(dfs_stack)->id );
	  
	  scc_stack = SBSCC(scc_stack)->scc_next;
	  dfs_stack->A = CCL_BITPTR(sataf_automaton,A);
	  SASCC(dfs_stack)->sa = sa;
	  SASCC(dfs_stack)->id = sa_init;
	  dfs_stack = next;	  

	  if( ccl_hash_find(scc,sa) )
	    shared_automaton_del_reference(sa);
	  else
	    ccl_hash_insert(scc,sa);
	}
      else if( SBSCC(dfs_stack)->letter < sigma_size )
	{
	  letter = SBSCC(dfs_stack)->letter++;
	  succ = s_state_succ(sp,dfs_stack->A,letter);
	 
	  SBSCC(dfs_stack)->succ[letter] = succ;

	  if( IS_SASCC(succ) )
	    {
	      s_cache_put(CCL_BITPTR2PTR(sataf_automaton,succ->A),
			  SASCC(succ)->sa,SASCC(succ)->id);      
	      continue;
	    }

	  if( SBSCC(succ)->id == 0xFFFFFFFF )
	    {
	      /* not yet visited */
	      SBSCC(succ)->id       = r;
	      SBSCC(succ)->min      = r++;
	      SBSCC(succ)->letter   = 0;
	      SBSCC(succ)->dfs_next = dfs_stack;
	      SBSCC(succ)->scc_next = scc_stack;
	      dfs_stack             = succ;
	      scc_stack             = succ;
	    }
	  else if( SBSCC(succ)->letter != 0xFFFFFFFF ) 
	    {
	      /* still in scc stack */
	      if( SBSCC(succ)->id < SBSCC(dfs_stack)->min )
		SBSCC(dfs_stack)->min = SBSCC(succ)->id;
	    }
	}
      else /* all successors are visited */
	{
	  state *next = SBSCC(dfs_stack)->dfs_next;

	  if( next != NULL )
	    {
	      ccl_assert( ! IS_SASCC(next) );
	      if( SBSCC(dfs_stack)->min < SBSCC(next)->min )
		SBSCC(next)->min = SBSCC(dfs_stack)->min;
	    }

	  if( SBSCC(dfs_stack)->min == SBSCC(dfs_stack)->id )
	    { 
	      sataf_automaton A = dfs_stack->A;
	      /* the current state is the root of a SCC */
	      s_compile_scc(sigma_size,dfs_stack,&scc_stack,T,
			    &tmp_array_size,&tmp_array,
			    &tmp_sa_array_size,&tmp_sa_array,sp);

	      if( ccl_hash_find(scc,SASCC(dfs_stack)->sa) )
		shared_automaton_del_reference(SASCC(dfs_stack)->sa);
	      else
		ccl_hash_insert(scc,SASCC(dfs_stack)->sa);
	      s_cache_put(A,SASCC(dfs_stack)->sa,SASCC(dfs_stack)->id);
	    }
	  dfs_stack = next;	  
	}
    }

  ccl_assert( IS_SASCC(start) );
  
  ccl_delete(tmp_array);
  ccl_delete(tmp_sa_array);
  R = sataf_msa_find_or_add(SASCC(start)->sa,SASCC(start)->id);
  s_state_space_delete(sp);

  /* s_delete_scc_list(scc); */
  ccl_hash_delete(scc);


  return R;
}
#endif
			/* --------------- */

static uint32_t
s_hash_state(const void *p)
{
  state_after_scc *s = SASCC(p);

  return 13*s->sa->depth+1011*(uint32_t)s->sa+43011*s->id;
}

			/* --------------- */

static int
s_compare_states(const void *p1, const void *p2)
{
  state_after_scc *s1 = SASCC(p1);
  state_after_scc *s2 = SASCC(p2);

  ccl_pre( IS_SASCC(p1) );
  ccl_pre( IS_SASCC(p2) );
  ccl_pre( s1->sa != NULL );
  ccl_pre( s2->sa != NULL );

  if( s2->sa->depth == s1->sa->depth )
    {
      if( s2->sa == s1->sa )
	{
	  ccl_assert( s2->id != s1->id );
	  return s2->id-s1->id;
	}
      else
	return (uint32_t)s2->sa-(uint32_t)s1->sa;
    }
  return s2->sa->depth-s1->sa->depth;
}

			/* --------------- */

static exit_automaton
s_build_exit_automaton(uint32_t sigma_size, 
		       state              *S, 
		       state      *scc_stack,
		       ccl_list        exits,
		       uint32_t      *psrc,
		       uint32_t   *pletter)
{
  size_t l;
  state *pS;
  exit_automaton R;
  uint32_t nb_states = 0;

  /* measure the size of the exit automaton */
  for(pS = scc_stack; pS != SBSCC(S)->scc_next; pS = SBSCC(pS)->scc_next)
    {
      SBSCC(pS)->id = nb_states++;
      for(l = 0; l < sigma_size; l++)
	{
	  int found = 0;
	  state_after_scc *succ;
	  ccl_pair            p;

	  if( ! IS_SASCC(SBSCC(pS)->succ[l]) )
	    continue;

	  succ = SASCC(SBSCC(pS)->succ[l]);

	  for(p = FIRST(exits); p != NULL; p = CDR(p))
	    {
	      state_after_scc *sm = SASCC(CAR(p));
	      
	      if( sm->sa == succ->sa && sm->id == succ->id )
		{
		  SBSCC(pS)->succ[l] = (state *)sm;
		  break;
		}
	    }
	  if( p == NULL )
	    ccl_list_add(exits,SBSCC(pS)->succ[l]);	    	
	}
    }

  if( ccl_list_get_size(exits) > 1 )
    ccl_list_sort(exits,s_compare_states);


  {
    ccl_pair p; 
    int i = 0;
    for(i = 0, p = FIRST(exits); p; p = CDR(p))
      ((state *)CAR(p))->exit_index = i++;
  }

  R = exit_automaton_create(nb_states,ccl_list_get_size(exits),sigma_size);
  *psrc = 0xFFFFFFFF;

  /* fill exit automaton 'succ' function */
  for(pS = scc_stack; pS != SBSCC(S)->scc_next; pS = SBSCC(pS)->scc_next)
    {
      uint32_t src = SBSCC(pS)->id;

      ccl_assert( ! IS_SASCC(pS) );

      if( sataf_automaton_is_final(pS->A) )
	R->is_final[src] = 1;

      for(l = 0; l < sigma_size; l++)
	{
	  uint32_t    tgt;
	  int is_exit_state = IS_SASCC(SBSCC(pS)->succ[l]);

	  if( is_exit_state ) 
	    {
	      ccl_assert( ccl_list_has(exits,SBSCC(pS)->succ[l]) );
	      tgt = SBSCC(pS)->succ[l]->exit_index;

	      if( tgt == 0 && *psrc == 0xFFFFFFFF )
		{
		  *psrc = src;
		  *pletter = l;
		}
	    }
	  else
	    {
	      tgt = SASCC(SBSCC(pS)->succ[l])->id;
	    }

	  exit_automaton_set_successor(R,src,l,tgt,is_exit_state);
	}
    } 

  return R;
}

			/* --------------- */

static void
s_compile_scc(uint32_t                sigma_size, 
	      state                             *S, 
	      state                   **pscc_stack,
	      sataf_shared_automaton_transformer T,
	      uint32_t           *tmp_array_size,
	      uint32_t               **tmp_array,
	      uint32_t        *tmp_sa_array_size,
	      shared_automaton      **tmp_sa_array,
	      state_space                      *sp)
{  
  uint32_t   nb_exits;
  ccl_list        exits;
  exit_automaton  xauto;
  state            *aux;
  state      *scc_stack = *pscc_stack;
  shared_automaton   sa;
  uint32_t homo_found;
  uint32_t *bind_init;
  uint32_t         *h = NULL;
  uint32_t        src;
  uint32_t     letter = 0;
  uint32_t          k;
  ccl_pair            p;

  exits = ccl_list_create();
  xauto = s_build_exit_automaton(sigma_size,S,scc_stack,exits,&src,&letter);
  nb_exits = ccl_list_get_size(exits);

  if( *tmp_array_size < 2*xauto->nb_local_states+nb_exits )
    {	 
      uint32_t *aux;
      *tmp_array_size = 2*xauto->nb_local_states+nb_exits;
      aux = ccl_new_array(uint32_t,*tmp_array_size);
      ccl_delete(*tmp_array);
      *tmp_array = aux;
    }

  if( *tmp_sa_array_size < nb_exits )
    {      
      shared_automaton *aux;
      *tmp_sa_array_size = nb_exits;
      aux = ccl_new_array(shared_automaton,*tmp_sa_array_size);
      ccl_delete(*tmp_sa_array);
      *tmp_sa_array = aux;
    }

  h = (*tmp_array)+xauto->nb_local_states;
  bind_init = h+xauto->nb_local_states;

  for(k = 0, p = FIRST(exits); p; p = CDR(p), k++)
    {
      state_after_scc *m = SASCC(CAR(p));

      ccl_assert( IS_SASCC(m) );

      (*tmp_sa_array)[k]   = m->sa;
      bind_init[k] = m->id;
    }

  homo_found = 0;

  if( T != NULL )
    {
      T(xauto,*tmp_sa_array,bind_init);
    }

  sa = NULL;

  if( (xauto->nb_exit_states == 0 || 
      (xauto->nb_exit_states == 1 && bind_init[0] == 0 &&
       shared_automaton_is_zero_or_one((*tmp_sa_array)[0]))) )
    {
      uint32_t p;
      int isConstant = 1;
      int isOne;

      if( xauto->nb_exit_states == 0 )
	isOne = xauto->is_final[0];
      else
	isOne = shared_automaton_is_one((*tmp_sa_array)[0]);
                
      for(p = 0; p < xauto->nb_local_states && isConstant; p++)
	isConstant = (isOne == xauto->is_final[p]);
      
      if( isConstant ) 
	{
	  sa = isOne
	    ?shared_automaton_create_one(sigma_size)
	    :shared_automaton_create_zero(sigma_size);

	  for(p = 0; p < xauto->nb_local_states; p++)
	    h[p] = 0;
	  exit_automaton_del_reference(xauto);
	}
    }

  if( sa == NULL )
    {
      if( src != 0xFFFFFFFF )
	{
	  if( xauto->nb_local_states == 1 )
	    homo_found = check_homomorphism1(xauto,*tmp_sa_array,bind_init,
					     letter,*tmp_array,h);
	  else
	    homo_found = check_homomorphism(xauto,*tmp_sa_array,bind_init,
					    src,letter,*tmp_array,h);

	}
      
      if( homo_found )
	{      
	  ccl_assert( xauto->nb_exit_states > 0 );
	  
	  exit_automaton_del_reference(xauto);
	  sa = shared_automaton_add_reference((*tmp_sa_array)[0]);
	}
      else
	{
	  exit_automaton        min_xauto = exit_automaton_minimize(xauto,h);
	  exit_automaton unique_min_xauto = 
	    exit_automaton_find_or_add(min_xauto);
	  
	  exit_automaton_del_reference(min_xauto);
	  exit_automaton_del_reference(xauto);
	  
	  sa  = shared_automaton_find_or_add(unique_min_xauto,*tmp_sa_array,
					     bind_init);
	  exit_automaton_del_reference(unique_min_xauto);
	}     
    }

  ccl_list_delete(exits);
  do {
    aux       = scc_stack;
    k         = SBSCC(aux)->id;
    scc_stack = SBSCC(aux)->scc_next;

    ccl_assert( ! IS_SASCC(aux) );

    aux->A         = CCL_BITPTR(sataf_automaton,aux->A);
    SASCC(aux)->sa = sa;
    SASCC(aux)->id = h[k];
  } while( S != aux );

  *pscc_stack = scc_stack;
}

			/* --------------- */

static void
s_compile_scc_with_cache(uint32_t                sigma_size, 
			 state                             *S, 
			 state                   **pscc_stack,
			 sataf_shared_automaton_transformer T,
			 uint32_t           *tmp_array_size,
			 uint32_t               **tmp_array,
			 uint32_t        *tmp_sa_array_size,
			 shared_automaton      **tmp_sa_array,
			 state_space                      *sp)
{    
  uint32_t initial;
  shared_automaton R;
  sataf_automaton A = S->A;

  if( (R = s_cache_get(A,&initial)) == NULL )
    {    
      s_compile_scc(sigma_size,S,pscc_stack,T,tmp_array_size,tmp_array,
		    tmp_sa_array_size,tmp_sa_array,sp);
      s_cache_put(A,SASCC(S)->sa,SASCC(S)->id);      

      if(0) {
	sataf_msa msa = sataf_msa_compute_with_transformer2(A,T);
	ccl_assert( msa->A == SASCC(S)->sa && msa->initial == SASCC(S)->id);
	sataf_msa_del_reference(msa);	
      }
    }
  else
    {      
      uint32_t l;
      state *aux;
      state *scc_stack = *pscc_stack;
      ccl_list stack = ccl_list_create();

      if(0) {	
	sataf_msa msa = sataf_msa_compute_with_transformer2(A,T);
	ccl_assert( msa->A == R );
	sataf_msa_del_reference(msa);
      }
     
      ccl_list_add(stack,S);
      S->A = CCL_BITPTR(sataf_automaton,S->A);
      S->id = initial;

      while( ! ccl_list_is_empty(stack) )
	{
	  state *s = (state *)ccl_list_take_first(stack);

	  for(l = 0; l < sigma_size; l++)
	    {
	      uint32_t sa_succ;
	      state *scc_succ = SBSCC(s)->succ[l];
	      
	      if( IS_SASCC(scc_succ) )
		continue;

	      scc_succ->A = CCL_BITPTR(sataf_automaton,scc_succ->A);
	      sa_succ = exit_automaton_successor(R->automaton,s->id,l);
	      scc_succ->id = exit_automaton_decode_succ_state(sa_succ);
	      ccl_assert( exit_automaton_is_local_state(sa_succ) );

	      ccl_list_add(stack,scc_succ);
	    }
	}
      ccl_list_delete(stack);

      do {
	aux = scc_stack;
	scc_stack = SBSCC(aux)->scc_next;	
	SASCC(aux)->sa = R;
      } while( S != aux );
      
      *pscc_stack = scc_stack;
    }
}

			/* --------------- */

static state_space *
s_state_space_create(uint32_t sigma_size)
{
  state_space *result = ccl_new(state_space);
  size_t sz = sizeof(state_before_scc)+sizeof(state *)*(sigma_size-1);


  result->table_size_index = STATE_SPACE_TABLE_SIZE_INDEX;
  result->state_table_size = SIZE_FOR_TABLE[result->table_size_index];
  result->state_table = ccl_new_array(state *,result->state_table_size);
  result->nb_states = 0;
  result->state_pool = ccl_pool_create("SBSCC-State-Pool",sz,
				       SBSCC_STATE_POOL_PAGE_SIZE/sz);

  return result;
}

			/* --------------- */

static void
s_increase_state_table_size(state_space *sp)
{
  uint32_t        i;
  uint32_t    index;
  state            *s;
  state         *next;
  uint32_t new_size;
  state   **new_table;

  if( sp->table_size_index+1 >= SIZE_FOR_TABLE_SIZE )
    return;
 
  new_size = SIZE_FOR_TABLE[sp->table_size_index+1];

  ccl_warning("increase table from %d to %d.\n"
	      "state space size = %d\n",
	      SIZE_FOR_TABLE[sp->table_size_index],
	      SIZE_FOR_TABLE[sp->table_size_index+1],
	      sp->nb_states);
	      
  new_table = ccl_new_array(state *,new_size);

  for(i = 0; i < sp->state_table_size; i++)
    {
      for(s = sp->state_table[i]; s; s = next)
	{
	  next  = s->next;	  
	  index = s->hval % new_size;
	  
	  s->next = new_table[index];
	  new_table[index] = s;
	}
    }

  ccl_delete(sp->state_table);
  sp->state_table = new_table;
  sp->state_table_size = new_size;
  sp->table_size_index++;
}

			/* --------------- */

static state *
s_find_or_add_state(state_space *sp, sataf_automaton A)
{ 
  state      **pstate;
  uint32_t     hval = sataf_automaton_hashcode(A);
  uint32_t    index = hval % sp->state_table_size;
  state_before_scc *s;
  
  for(pstate = sp->state_table+index; *pstate; pstate = &((*pstate)->next))
    {
      if( (*pstate)->hval != hval )
	continue;

      if( sataf_automaton_equals(CCL_BITPTR2PTR(sataf_automaton,(*pstate)->A),
				 A) )
	return *pstate;
    }
  sp->nb_states++;

  s = (state_before_scc *)ccl_pool_alloc(sp->state_pool);
  *pstate = (state *)s;

  s->next      = NULL;
  s->A         = sataf_automaton_add_reference(A);
  s->hval      = hval;
  s->id        = 0xFFFFFFFF;
  s->min       = 0xFFFFFFFF;
  s->letter    = 0xFFFFFFFF;
  s->dfs_next  = NULL;
  s->scc_next  = NULL;
  s->exit_index = -1;

  if( sp->nb_states > sp->state_table_size*STATE_SPACE_FILL_DEGREE )
    s_increase_state_table_size(sp);

  return (state *)s;
}

			/* --------------- */

static state *
s_state_succ(state_space *sp, sataf_automaton A, uint32_t letter)
{
  sataf_automaton tmp = sataf_automaton_succ(A,letter);
  state *S = s_find_or_add_state(sp,tmp);

  sataf_automaton_del_reference(tmp);

  return S;
}

			/* --------------- */

static void
s_state_space_clean(state_space *sp)
{
  size_t i = sp->state_table_size;
  state *s;
  state **pt = sp->state_table;

  while( i-- )
    {
      for(s = *(pt++); s; s = s->next)
	sataf_automaton_del_reference(CCL_BITPTR2PTR(sataf_automaton,s->A));
    }
}


			/* --------------- */

static void
s_state_space_delete(state_space *sp)
{
  s_state_space_clean(sp);
  ccl_pool_delete(sp->state_pool);
  ccl_delete(sp->state_table);
  ccl_delete(sp);
}

			/* --------------- */

typedef struct sa_cache_record_st {
  sataf_automaton a;
  shared_automaton sa;
  uint32_t initial;
} sa_cache_record;

typedef struct {
  int requests;
  int collisions;
  int success;
  int number_of_entries;
  int size;
  sa_cache_record records[CACHE_SIZE];
} sa_cache;

			/* --------------- */


static sa_cache *SA_CACHE = NULL;

			/* --------------- */

static void
s_cache_init(void)
{  
  SA_CACHE = ccl_new(sa_cache);
  ccl_memzero(SA_CACHE,sizeof(SA_CACHE));
  SA_CACHE->size = CACHE_SIZE;
}

			/* --------------- */

static void
s_cache_terminate(void)
{
  int i = CACHE_SIZE;

  while( i-- )
    {
      ccl_zdelete(sataf_automaton_del_reference,SA_CACHE->records[i].a);
      ccl_zdelete(shared_automaton_del_reference,SA_CACHE->records[i].sa);
    }
}

			/* --------------- */

static shared_automaton
s_cache_get(sataf_automaton a, uint32_t *pinitial)
{
  uint32_t index = sataf_automaton_hashcode(a) % CACHE_SIZE;
  sa_cache_record *rec = SA_CACHE->records+index;

  SA_CACHE->requests++;
  if( rec->a != NULL && sataf_automaton_equals(rec->a,a) )
    {
      SA_CACHE->success++;
      *pinitial = rec->initial;
      return shared_automaton_add_reference(rec->sa);
    }

  return NULL;
}

			/* --------------- */

static void
s_cache_put(sataf_automaton a, shared_automaton R, uint32_t init)
{
  uint32_t index = sataf_automaton_hashcode(a) % CACHE_SIZE;
  sa_cache_record *rec = SA_CACHE->records+index;

  if( rec->a == NULL )
    SA_CACHE->number_of_entries++;
  else 
    {
      ccl_assert( ccl_imply(sataf_automaton_equals(rec->a,a), rec->sa == R) );

      if( sataf_automaton_equals(rec->a,a) )
	return;

      SA_CACHE->collisions++;

      sataf_automaton_del_reference(rec->a);
      shared_automaton_del_reference(rec->sa);
    }

  rec->a = sataf_automaton_add_reference(a);
  rec->sa = shared_automaton_add_reference(R);
  rec->initial = init;
}

			/* --------------- */

void
sataf_msa_sharing_cache_statistics(ccl_log_type log)
{
  ccl_log(log,"Usage of cache 'A->SHA' :\n");
  ccl_log(log,"  size       : %d\n",SA_CACHE->size);
  ccl_log(log,"  entries    : %d\n",SA_CACHE->number_of_entries);
  ccl_log(log,"  requests   : %d\n",SA_CACHE->requests);
  ccl_log(log,"  success    : %d (%.2f %%)\n",SA_CACHE->success,
	  100.0*(float)SA_CACHE->success/(float)SA_CACHE->requests);
  ccl_log(log,"  collisions : %d\n",SA_CACHE->collisions);
}
