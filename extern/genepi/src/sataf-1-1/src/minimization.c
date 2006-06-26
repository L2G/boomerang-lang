/* $Id: minimization.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <ccl-assert.h>
#include <ccl-memory.h>
#include <ccl-list.h>
#include <ccl-bittable.h>
#include "exit-automaton.h"

			/* --------------- */

static void
s_init(exit_automaton ea, ccl_list *ppartition, ccl_bittable **pSclasses);

static int
s_partition_refinement(exit_automaton ea, ccl_bittable *Sclasses,
		       ccl_bittable c, ccl_list partition);

			/* --------------- */

static int
s_nb_bits(ccl_bittable bt)
{
  int max = ccl_bittable_size(bt);
  int i;
  int r = 0;

  for(i = 0; i < max; i++) 
    if( ccl_bittable_has(bt,i) ) 
      r++;

  return r;
}

			/* --------------- */

static int
s_cmp(const void *p1, const void *p2)
{
  int s1 = s_nb_bits((const ccl_bittable)p1);
  int s2 = s_nb_bits((const ccl_bittable)p2);

  if( s1 != s2 ) 
    return s1-s2;
  else
    {
      int f1 = ccl_bittable_get_first((const ccl_bittable)p1);
      int f2 = ccl_bittable_get_first((const ccl_bittable)p2);

      ccl_assert( f1 != f2 );
      return f1-f2;
    }
}


static exit_automaton
s_minimize(exit_automaton ea, 
	   uint32_t     *h)
{
  int                  i;
  ccl_pair             p;
  exit_automaton       R;
  int            refined = 1;
  ccl_list     partition;
  ccl_bittable *Sclasses;

  /* build initial partition */
  s_init(ea,&partition,&Sclasses);

  /* refine partition */
  while( refined )
    {
      refined = 0;
      
      for(p = FIRST(partition); p && !refined; p = CDR(p))
	{
	  ccl_bittable c = (ccl_bittable)CAR(p);
	  if( s_partition_refinement(ea,Sclasses,c,partition) )
	    refined = 1;
	}
    }
  ccl_list_sort(partition,s_cmp);
  R = exit_automaton_create(ccl_list_get_size(partition),
			    ea->nb_exit_states,
			    ea->alphabet_size);

  for(i = 0, p = FIRST(partition); p; p = CDR(p), i++)
    {
      ccl_bittable  c = (ccl_bittable)CAR(p);
      int          si = ccl_bittable_get_first(c);

      R->is_final[i] = ea->is_final[si];
	
      for(; si >= 0; si = ccl_bittable_get_next(c,si))
	h[si] = i;
    }


  for(i = 0, p = FIRST(partition); p; p = CDR(p), i++)
    {
      uint32_t a;
      ccl_bittable c = (ccl_bittable)CAR(p);
      size_t si = ccl_bittable_get_first(c);
      
      ccl_assert( ccl_bittable_get_first(c) >= 0 );

      for(a = 0; a < ea->alphabet_size; a++)
	{
	  uint32_t succ = exit_automaton_successor(ea,si,a);
	  uint32_t index = exit_automaton_decode_succ_state(succ);

	  if( exit_automaton_is_exit_state(succ) )
	    exit_automaton_set_successor(R,h[si],a,index,1);
	  else
	    exit_automaton_set_successor(R,h[si],a,h[index],0);
	}
      ccl_bittable_delete(c);
    }
  
  ccl_list_delete(partition);
  ccl_delete(Sclasses);

  return R;
}

			/* --------------- */

exit_automaton
exit_automaton_ref_minimize(exit_automaton ea, 
			    uint32_t     *h)
{
  size_t i;
  uint32_t *h1 = ccl_new_array(uint32_t,ea->nb_local_states);
  uint32_t *h2 = ccl_new_array(uint32_t,ea->nb_local_states);
  exit_automaton min1 = s_minimize(ea,h1);
  exit_automaton min2 = s_minimize(ea,h2);

  exit_automaton_del_reference(min1);

  for(i = 0; i < ea->nb_local_states; i++)
    h[i] = h2[h1[i]];
  ccl_delete(h1);
  ccl_delete(h2);

  return min2;
}


			/* --------------- */


static int
s_states_are_equivalents(exit_automaton ea, 
			 uint32_t s1, uint32_t s2, 
			 ccl_bittable *Sclasses)
{
  uint32_t a;

  if( ea->is_final[s1] != ea->is_final[s2] )
    return 0;

  for(a = 0; a < ea->alphabet_size; a++)
    {
      uint32_t itgt1 = exit_automaton_successor(ea,s1,a);
      uint32_t  tgt1 = exit_automaton_decode_succ_state(itgt1);
      uint32_t itgt2 = exit_automaton_successor(ea,s2,a);
      uint32_t  tgt2 = exit_automaton_decode_succ_state(itgt2);

      if( exit_automaton_is_local_state(itgt1) != 
	  exit_automaton_is_local_state(itgt2) )
	return 0;
      if( Sclasses[tgt1] != Sclasses[tgt2] )
	return 0;
    }
    
  return 1;
}

			/* --------------- */

static int
s_partition_refinement(exit_automaton ea, 
		       ccl_bittable *Sclasses,
		       ccl_bittable c, ccl_list partition)
{
  int           i;
  int       first;
  ccl_bittable nc = NULL;

  first = ccl_bittable_get_first(c);
  for(i = ccl_bittable_get_next(c,first); i >= 0;
      i = ccl_bittable_get_next(c,i))
    {
      if( s_states_are_equivalents(ea,first,i,Sclasses) )
	continue;
      if( nc == NULL )
	nc = ccl_bittable_create(ccl_bittable_size(c));
      ccl_bittable_unset(c,i);
      ccl_bittable_set(nc,i);
    }


  if( nc != NULL )
    {
      for(i = ccl_bittable_get_first(nc); i >= 0;
	  i = ccl_bittable_get_next(nc,i))
	Sclasses[i] = nc;
      ccl_list_add(partition,nc);
    }

  return (nc != NULL);
}

			/* --------------- */

static void
s_init(exit_automaton ea, ccl_list *ppartition, ccl_bittable **pSclasses)
{
  size_t i;
  size_t size = ea->nb_local_states;
  ccl_bittable firstc = ccl_bittable_create(size);
  
  *ppartition = ccl_list_create();
  ccl_list_add(*ppartition,firstc);
  *pSclasses = ccl_new_array(ccl_bittable,size);

  for(i = 0; i < ea->nb_local_states; i++)
    {
      (*pSclasses)[i] = firstc;
      ccl_bittable_set(firstc,i);
    }
}

/*
  uint32_t state;

  if( ph != NULL )
    {
      uint32_t *h;

      if( (h=*ph) == NULL )	
	h = *ph = ccl_new_array(uint32_t,ea->nb_local_states);

      if( h == NULL )
	return NULL;

      for(state = 0; state < ea->nb_local_states; state++)
	h[state] = state;
    }

  return exit_automaton_add_reference(ea);
*/
