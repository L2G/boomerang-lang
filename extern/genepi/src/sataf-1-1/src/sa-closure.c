/* $Id: sa-closure.c,v 1.2 2006/01/04 08:03:24 point Exp $ */
#include "shared-automaton.h"
#include "sataf-shared-automaton.h"


static void
s_close(sataf_ea ea, sataf_sa *bind_sa, uint32_t *bind_init, int type)
{  
  uint32_t *set = ccl_new_array(uint32_t,ea->nb_local_states);
  char *done = ccl_new_array(char,ea->nb_local_states);
  uint32_t set_size = 0;
  uint32_t in;
  uint32_t q, p, s;

  p = ea->nb_local_states;
  while( p-- )
    {
      if( done[p] || ea->is_final[p] == type ) 
	continue;

      s = p;
      q = exit_automaton_encode_succ_as_local_state(p);

      while( exit_automaton_is_local_state(q) && ea->is_final[p] != type && 
	     ! done[s] ) 
	{
	  set[set_size++] = s;
	  done[s] = 1;
	  q = exit_automaton_successor(ea,s,0);
	  s = exit_automaton_decode_succ_state(q);
	}
      
      in = exit_automaton_is_exit_state(q)
	?bind_sa[s]->automaton->is_final[bind_init[s]]
	:ea->is_final[s];
      
      if( (in && type) || (!in && ! type) )
	{
	  for(s = 0; s < set_size; s++)
	    ea->is_final[set[s]] = type;
	}
      memset(set,0,set_size*sizeof(uint32_t)); 
      set_size = 0;
    }
  ccl_delete(done);
  ccl_delete(set);
}

			/* --------------- */

void
sataf_shared_automaton_exists_closure(sataf_ea           ea,
				      sataf_sa     *bind_sa,
				      uint32_t *bind_init)
{  
  s_close(ea,bind_sa,bind_init,1);
}

			/* --------------- */

void
sataf_shared_automaton_forall_closure(sataf_ea           ea,
				      sataf_sa     *bind_sa,
				      uint32_t *bind_init)
{
  s_close(ea,bind_sa,bind_init,0);
}
