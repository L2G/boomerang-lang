/* $Id: sataf-msa.c,v 1.3 2006/01/25 16:12:51 point Exp $ */
#include <ccl-assert.h>
#include <ccl-hash.h>
#include <ccl-pool.h>
#include "shared-automaton.h"
#include "sataf-msa-p.h"
#include "sataf-msa.h"

static SSA *
s_new_ssa(shared_automaton sa, uint32_t initial);

static void
s_ssa_destroy(sataf_automaton self);

static uint32_t
s_ssa_get_alphabet_size(sataf_automaton self);

# define s_ssa_to_string NULL

static sataf_automaton
s_ssa_succ(sataf_automaton self, uint32_t letter);

static int
s_ssa_is_final(sataf_automaton self);

static int
s_ssa_equals(sataf_automaton self, sataf_automaton other);

static uint32_t
s_ssa_hashcode(sataf_automaton self);

# define s_ssa_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st SSA_METHODS = {
  "SA-MSA",
  sizeof(SSA),
  s_ssa_destroy,
  s_ssa_get_alphabet_size,
  s_ssa_to_string,
  s_ssa_succ,
  s_ssa_is_final,
  s_ssa_equals,
  s_ssa_hashcode,
  s_ssa_to_dot
};

const char *SATAF_MSA_ID = "SA-MSA";

			/* --------------- */

static sataf_msa ZERO_FOR_BOOLEANS = NULL;
static sataf_msa ONE_FOR_BOOLEANS = NULL;

			/* --------------- */

# define DUP(msa) sataf_msa_add_reference(msa)
# define DEL(msa) sataf_msa_del_reference(msa)

			/* --------------- */


void
sataf_msa_init(void)
{
  shared_automaton a;

  sataf_msa_unique_table_init();
  sataf_msa_sharing_init();
  sataf_msa_cache_init();

  a = shared_automaton_create_zero(2);
  ZERO_FOR_BOOLEANS = sataf_msa_find_or_add(a,0);
  shared_automaton_del_reference(a);
  
  a = shared_automaton_create_one(2);
  ONE_FOR_BOOLEANS = sataf_msa_find_or_add(a,0);
  shared_automaton_del_reference(a);
}

			/* --------------- */

void
sataf_msa_terminate(void)
{
  sataf_msa_del_reference(ZERO_FOR_BOOLEANS);
  sataf_msa_del_reference(ONE_FOR_BOOLEANS);
  sataf_msa_cache_terminate();
  sataf_msa_sharing_terminate();
  sataf_msa_unique_table_terminate();
}

			/* --------------- */

sataf_msa
sataf_msa_zero(uint32_t alphabet_size)
{
  sataf_msa R;

  if( alphabet_size == 2 )
    R = sataf_msa_add_reference(ZERO_FOR_BOOLEANS);
  else
    {
      sataf_automaton a = sataf_automaton_create_zero(alphabet_size);
      R = sataf_msa_compute(a);
      sataf_automaton_del_reference(a);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_one(uint32_t alphabet_size)
{
  sataf_msa R;

  if( alphabet_size == 2 )
    R = sataf_msa_add_reference(ONE_FOR_BOOLEANS);
  else
    {
      sataf_automaton a = sataf_automaton_create_one(alphabet_size);
      R = sataf_msa_compute(a);
      sataf_automaton_del_reference(a);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_epsilon(uint32_t alphabet_size)
{
  sataf_automaton a = sataf_automaton_create_epsilon(alphabet_size);
  sataf_msa       R = sataf_msa_compute(a);
  sataf_automaton_del_reference(a);

  return R;  
}

			/* --------------- */

sataf_msa
sataf_msa_letter(uint32_t alphabet_size, uint32_t l)
{
  sataf_automaton a = sataf_automaton_create_letter(alphabet_size,l);
  sataf_msa       R = sataf_msa_compute(a);
  sataf_automaton_del_reference(a);

  return R;  
}

			/* --------------- */

sataf_msa
sataf_msa_alphabet(uint32_t alphabet_size)
{
  sataf_automaton a = sataf_automaton_create_alphabet(alphabet_size);
  sataf_msa       R = sataf_msa_compute(a);
  sataf_automaton_del_reference(a);

  return R;  
}

			/* --------------- */

sataf_automaton 
sataf_msa_to_automaton(sataf_msa msa)
{
  return (sataf_automaton)s_new_ssa(msa->A,msa->initial);
}

			/* --------------- */

sataf_msa
sataf_msa_succ(sataf_msa msa, uint32_t letter)
{
  sataf_msa  R;
  uint32_t index;
  uint32_t  succ;

  ccl_pre( msa != NULL );

  succ  = exit_automaton_successor(msa->A->automaton,msa->initial,letter);
  index = exit_automaton_decode_succ_state(succ);

  if( exit_automaton_is_local_state(succ) )
    {
      if( index == msa->initial ) 
	R = DUP(msa);
      else
	R = sataf_msa_find_or_add(msa->A,index);
    }
  else
    {
      R = DUP(msa->A->bind[index]);
    }

  return R;
}

			/* --------------- */

int
sataf_msa_is_final(sataf_msa msa)
{
  ccl_pre( msa != NULL );

  return msa->A->automaton->is_final[msa->initial];
}


			/* --------------- */

uint32_t
sataf_msa_get_alphabet_size(sataf_msa msa)
{
  return msa->A->automaton->alphabet_size;
}

			/* --------------- */

void
sataf_msa_display_as_dot(sataf_msa msa, ccl_log_type log, const char **alphab)
{
  shared_automaton_display_as_dot(msa->A,log,msa->initial,alphab);
}

			/* --------------- */

void
sataf_msa_display_as_olddot(sataf_msa msa,
			    ccl_log_type log,
			    const char **alphabet)
{
  const char *fmt;

  ccl_log(log,"digraph system { \n");
  if( sataf_msa_is_zero(msa) )
    ccl_log(log,"0 [label=zero]\n");
  else if( sataf_msa_is_one(msa) )
    ccl_log(log,"0 [label=one]\n");
  else 
    {
      ccl_list todo;
      ccl_hash done = ccl_hash_create(NULL,NULL,NULL,NULL);
      int countSA = 1;
      int countStates = 0;      
      shared_automaton A;
      int Ai;

      ccl_hash_find(done,msa->A);
      ccl_hash_insert(done,(void *)0);
      todo = ccl_list_create();
      ccl_list_put_first(todo,msa->A);

      ccl_log(log,"I -> C0I%d\n",msa->initial);
      
      while( ! ccl_list_is_empty(todo) )
	{
	  uint32_t i;

	  A  = (shared_automaton)ccl_list_take_first(todo);
	  ccl_assert( ccl_hash_find(done,A) );
	  ccl_hash_find(done,A);
	  Ai = (int)ccl_hash_get(done);
	
	  for(i = 0; i < A->automaton->nb_local_states; i++)
	    {
	      uint32_t a;

	      if( A->automaton->is_final[i] ) fmt = "C%dI%d [label=A%d]\n";
	      else                            fmt = "C%dI%d [label=%d]\n";

	      ccl_log(log,fmt,Ai,i,countStates);
	      countStates++;
	  
	      for(a = 0; a < A->automaton->alphabet_size; a++) 
		{
		  uint32_t    j = exit_automaton_successor(A->automaton,i,a);
		  uint32_t succ = exit_automaton_decode_succ_state(j);

		  ccl_log(log,"C%dI%d->",Ai,i);
		  if( exit_automaton_is_local_state(j) )
		    ccl_log(log,"C%dI%d",Ai,succ);
		  else 
		    {
		      sataf_msa M = A->bind[succ];

		      if( sataf_msa_is_one(M) )       ccl_log(log,"one");
		      else if( sataf_msa_is_zero(M) ) ccl_log(log,"zero");
		      else 
			{
			  int indexTgt;
			  
			  if( ! ccl_hash_find(done,M->A) ) 
			    {
			      indexTgt = countSA++;
			      ccl_hash_insert(done,(void *)indexTgt);
			      ccl_list_put_first(todo,M->A);
			    }
			  else 
			    {			      
			      indexTgt = (int)ccl_hash_get(done);
			    }
			  ccl_log(log,"C%dI%d",indexTgt,M->initial);
		      }
		    }

		  if( alphabet != NULL )
		    ccl_log(log," [label=%s]\n",alphabet[a]);
		  else 
		    ccl_log(log," [label=%d]\n",a);
		}
	    }
	}
      ccl_list_delete(todo);
      ccl_hash_delete(done);
    }
  ccl_log(log,"}\n");
}

			/* --------------- */

int
sataf_msa_is_zero(sataf_msa msa)
{
  ccl_pre( msa != NULL );

  return shared_automaton_is_zero(msa->A);
}

			/* --------------- */

int
sataf_msa_is_one(sataf_msa msa)
{
  ccl_pre( msa != NULL );

  return shared_automaton_is_one(msa->A);
}

			/* --------------- */

int
sataf_msa_is_zero_or_one(sataf_msa msa)
{
  ccl_pre( msa != NULL );

  return shared_automaton_is_zero_or_one(msa->A);
}

			/* --------------- */

int
sataf_msa_is_included_in(sataf_msa a1, sataf_msa a2)
{
  sataf_msa intersection = sataf_msa_and(a1,a2);
  int result = (intersection == a1);
  sataf_msa_del_reference(intersection);

  return result;
}

			/* --------------- */

void
sataf_msa_log_info(ccl_log_type log, sataf_msa msa)
{
  sataf_msa_info info;

  sataf_msa_get_info(msa,&info);
  ccl_log(log,"#SA=%d #EA=%d #S=%d #D=%d #Refs=%d\n",
	  info.nb_shared_automata,
	  info.nb_exit_automata,
	  info.nb_states,
	  info.depth,
	  info.refcount);
}

			/* --------------- */

void
sataf_msa_get_info(sataf_msa msa, sataf_msa_info *info)
{
  ccl_pair    p;
  ccl_list   ea = ccl_list_create();
  ccl_list todo = ccl_list_create();
  ccl_list done = ccl_list_create();

  ccl_list_add(done,msa->A);
  ccl_list_add(todo,msa->A);

  while( ! ccl_list_is_empty(todo) )
    {
      uint32_t        b;
      shared_automaton sa = ccl_list_take_first(todo);
     
      if( ! ccl_list_has(ea,sa->automaton) )
	ccl_list_add(ea,sa->automaton);
      for(b = 0; b < sa->automaton->nb_exit_states; b++)
	{
	  if( ! ccl_list_has(done,sa->bind[b]->A) )
	    {
	      ccl_list_add(done,sa->bind[b]->A);
	      ccl_list_add(todo,sa->bind[b]->A);
	    }
	}
    }

  info->nb_shared_automata = ccl_list_get_size(done);  
  info->nb_exit_automata = ccl_list_get_size(ea);  
  info->nb_states = 0;
  for(p = FIRST(done); p; p = CDR(p))
    {
      shared_automaton sa = (shared_automaton)(CAR(p));
      info->nb_states += sa->automaton->nb_local_states;
    }
  info->depth = msa->A->depth;
  info->refcount = msa->refcount;

  ccl_list_delete(todo);
  ccl_list_delete(done);
  ccl_list_delete(ea);
}

			/* --------------- */

static SSA *
s_new_ssa(shared_automaton sa, uint32_t initial)
{
  SSA *R = (SSA *)sataf_automaton_create(sizeof(SSA),&SSA_METHODS);

  R->A       = shared_automaton_add_reference(sa);
  R->initial = initial;

  return R;
}


			/* --------------- */

static void
s_ssa_destroy(sataf_automaton self)
{
  SSA *ssa = (SSA *)self;

  shared_automaton_del_reference(ssa->A);
}

			/* --------------- */

static uint32_t
s_ssa_get_alphabet_size(sataf_automaton self)
{
  SSA *ssa = (SSA *)self;

  return ssa->A->automaton->alphabet_size;
}

			/* --------------- */

static sataf_automaton
s_ssa_succ(sataf_automaton self, uint32_t letter)
{
  SSA     *ssa = (SSA *)self;
  uint32_t index, succ;
  sataf_automaton R;

  ccl_pre( self != NULL );

  succ  = exit_automaton_successor(ssa->A->automaton,ssa->initial,letter);
  index = exit_automaton_decode_succ_state(succ);

  if( exit_automaton_is_local_state(succ) )
    {
      if( index == ssa->initial ) 
	R = sataf_automaton_add_reference(self);
      else
	R =(sataf_automaton)s_new_ssa(ssa->A,index);
    }
  else
    {
      R = (sataf_automaton)s_new_ssa(ssa->A->bind[index]->A,
				     ssa->A->bind[index]->initial);
    }

  return R;
}

			/* --------------- */

static int
s_ssa_is_final(sataf_automaton self)
{
  SSA *ssa = (SSA *)self;

  return ssa->A->automaton->is_final[ssa->initial];
}

			/* --------------- */

static int
s_ssa_equals(sataf_automaton self, sataf_automaton other)
{
  SSA       *ssa = (SSA *)self;
  SSA *ssa_other = (SSA *)other;

  return ssa->A == ssa_other->A && ssa->initial == ssa_other->initial;
}

			/* --------------- */

static uint32_t
s_ssa_hashcode(sataf_automaton self)
{
  SSA *ssa = (SSA *)self;

  return 5*(uint32_t)ssa->A+13*ssa->initial;
}

