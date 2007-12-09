/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.3 $
 * @date    : $Date: 2006/01/23 13:32:17 $
 */

#include "sataf-msa-p.h"
#include "homomorphism-checking.h"

# define H_UNDEF 0xFFFFFFFF

uint32_t 
check_homomorphism(exit_automaton ea,
		   shared_automaton *bind_sa, uint32_t *bind_init,
		   uint32_t src, uint32_t letter,
		   uint32_t *stack, uint32_t *h)
{  
  uint32_t q, i, initial, top, a, N, found_h;
  uint32_t p, u, du, v, dv, asize = ea->alphabet_size;
  shared_automaton SA = bind_sa[0];
  exit_automaton A = SA->automaton;

  N = A->nb_local_states;

  initial = exit_automaton_encode_succ_as_local_state(bind_init[0]);
  found_h = 0;

  for(q = 0; q < N && ! found_h; q++)
    {
      if( exit_automaton_successor(A,q,letter) != initial || 
	  A->is_final[q] != ea->is_final[src] )
	continue;

      memset(h,H_UNDEF,sizeof(uint32_t)*ea->nb_local_states);

      top          = 0;
      found_h      = 1;
      h[src]       = q;
      stack[top++] = src;

      while( top > 0 && found_h ) 
	{
	  p = stack[--top];

	  for(a = 0; a < asize && found_h; a++) 
	    {
	      u = exit_automaton_successor(ea,p,a);
	      du = exit_automaton_decode_succ_state(u);
	      v = exit_automaton_successor(A,h[p],a);
	      dv = exit_automaton_decode_succ_state(v);

	      if( exit_automaton_is_local_state(u) )
		{	
		  found_h = exit_automaton_is_local_state(v);  
		  if( found_h )
		    {
		      if( h[du] == H_UNDEF && 
			  ea->is_final[du] == A->is_final[dv] )
			{
			  h[du] = dv;
			  stack[top++] = du;
			} 
		      else 
			{
			  found_h = (h[du] == dv);
			}
		    }
		}
	      else if( exit_automaton_is_local_state(v) )
		{
		  if( bind_init[du] != dv || 
		      bind_sa[du] != SA )
		    found_h = 0;
		}
	      else 
		{
		  if( bind_sa[du] != SA->bind[dv]->A || 
		      bind_init[du] != SA->bind[dv]->initial )
		    found_h = 0;
		}
	    }
	}
    }

  return found_h;
}

			/* --------------- */

uint32_t 
check_homomorphism1(exit_automaton ea,
		    shared_automaton *bind_sa, uint32_t *bind_init,
		    uint32_t letter,
		    uint32_t *stack, uint32_t *h)
{  
  uint32_t q, i, initial, top, a, N, found_h;
  uint32_t u, du, v, dv, asize = ea->alphabet_size;
  shared_automaton SA = bind_sa[0];
  exit_automaton A = SA->automaton;

  ccl_pre( ea->nb_local_states == 1  );

  N = A->nb_local_states;
  
  initial = exit_automaton_encode_succ_as_local_state(bind_init[0]);
  found_h = 0;
  
  for(q = 0; q < N && ! found_h; q++)
    {
      if( exit_automaton_successor(A,q,letter) != initial || 
	  A->is_final[q] != ea->is_final[0] )
	continue;
      
      found_h = 1;
      h[0] = q;
      
      for(a = 0; a < asize && found_h; a++) 
	{
	  u = exit_automaton_successor(ea,0,a);
	  du = exit_automaton_decode_succ_state(u);
	  v = exit_automaton_successor(A,q,a);
	  dv = exit_automaton_decode_succ_state(v);
	  
	  if( exit_automaton_is_local_state(u) )
	    {	
	      found_h = exit_automaton_is_local_state(v) && (dv == q);
	    }
	  else if( exit_automaton_is_local_state(v) )
	    {
	      if( bind_init[du] != dv || bind_sa[du] != SA )
		found_h = 0;
	    }
	  else 
	    {
	      if( bind_sa[du] != SA->bind[dv]->A || 
		  bind_init[du] != SA->bind[dv]->initial )
		found_h = 0;
	    }
	}
    }
  
  return found_h;
}
