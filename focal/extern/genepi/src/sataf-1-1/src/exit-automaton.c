/* $Id: exit-automaton.c,v 1.3 2006/01/04 08:02:38 point Exp $ */
#include <ccl-assert.h>
#include <ccl-log.h>
#include <ccl-memory.h>
#include "hopcroft.h"
#include "exit-automaton.h"

# define EA_UT_INIT_TABLE_SIZE 1403641
# define EA_UT_FILL_DEGREE 5

			/* --------------- */

typedef struct msa_manager_st {
  uint32_t table_size;
  exit_automaton *table;
  uint32_t nb_elements;
} ea_manager;

			/* --------------- */

#define MUST_INCREASE_TABLE() \
( EA_MANAGER->nb_elements > EA_MANAGER->table_size*EA_UT_FILL_DEGREE)

#define MUST_DECREASE_TABLE() \
( EA_MANAGER->nb_elements < EA_MANAGER->table_size*EA_UT_FILL_DEGREE  \
 && EA_MANAGER->table_size > EA_UT_INIT_TABLE_SIZE)

			/* --------------- */


static ea_manager *EA_MANAGER = NULL;

			/* --------------- */

static void
s_ea_manager_resize_table(uint32_t new_size);

			/* --------------- */

void
exit_automaton_init(void)
{
  hopcroft_init();
  EA_MANAGER = ccl_new(ea_manager);
  EA_MANAGER->table_size = EA_UT_INIT_TABLE_SIZE;
  EA_MANAGER->table = ccl_new_array(exit_automaton,EA_MANAGER->table_size);
  EA_MANAGER->nb_elements = 0;
}

			/* --------------- */

void
exit_automaton_terminate(void)
{
  hopcroft_terminate();

  if( EA_MANAGER->nb_elements != 0 )
    {
      int i = EA_MANAGER->table_size;

      ccl_warning("WARNING: EA uniqueness table still contains elements\n");
      while( i-- )
	{
	  exit_automaton ea, next;

	  for(ea = EA_MANAGER->table[i]; ea; ea = next)
	    {
	      next = ea->next;
	      ccl_delete(ea);
	    }
	}
    }
  ccl_delete(EA_MANAGER->table);
  ccl_delete(EA_MANAGER);
}

			/* --------------- */

exit_automaton 
exit_automaton_create(uint32_t nb_states, 
		      uint32_t nb_exits, 
		      uint32_t alphabet_size)
{
  size_t         to_alloc;
  exit_automaton result;

  ccl_pre( nb_states > 0 && alphabet_size > 0 );

  to_alloc = sizeof(struct exit_automaton_st)+
    (nb_states/4+1)*sizeof(uint32_t)+
    (nb_states*alphabet_size)*sizeof(uint32_t);
    
  result = ccl_malloc(to_alloc);

  ccl_memzero(result,to_alloc);
  result->refcount = 1;
  result->alphabet_size = alphabet_size;
  result->nb_local_states = nb_states;
  result->nb_exit_states = nb_exits;
  result->is_final = (uint8_t*)(result->successor+(nb_states*alphabet_size));

  return result;
}

			/* --------------- */

exit_automaton 
exit_automaton_complement(exit_automaton ea)
{
  uint32_t i = ea->nb_local_states;
  exit_automaton R = exit_automaton_create(ea->nb_local_states,
					   ea->nb_exit_states,
					   ea->alphabet_size);
  while( i-- )
    R->is_final[i] = ! ea->is_final[i];

  ccl_memcpy(R->successor,ea->successor,
	    sizeof(uint32_t)*ea->nb_local_states*ea->alphabet_size);
  
  return R;
}

			/* --------------- */

exit_automaton 
exit_automaton_create_with_arrays(uint32_t nb_states, 
				  uint32_t nb_exits, 
				  uint32_t alphabet_size,
				  const uint8_t *is_final,
				  const uint32_t *succ)
{
  exit_automaton R = exit_automaton_create(nb_states,nb_exits,alphabet_size);

  ccl_memcpy(R->is_final,is_final,sizeof(uint8_t)*nb_states);
  ccl_memcpy(R->successor,succ,sizeof(uint32_t)*nb_states*alphabet_size);

  return R;
}

			/* --------------- */

exit_automaton
exit_automaton_add_reference(exit_automaton ea)
{
  ccl_pre( ea != NULL ); ccl_pre( (ea->refcount &0x7FFFFFFF) > 0 );

  ea->refcount++;
  
  return ea;
}

			/* --------------- */

static exit_automaton *
s_find_exit_automaton(exit_automaton ea)
{
  exit_automaton *pea;
  int index = exit_automaton_hashcode(ea) % EA_MANAGER->table_size;


  for(pea = EA_MANAGER->table+index; *pea && *pea != ea && 
	! exit_automaton_equals(*pea,ea); pea = &((*pea)->next))
    /* do nothing */;

  return pea;
}

			/* --------------- */


void
exit_automaton_del_reference(exit_automaton ea)     
{
  ccl_pre( ea != NULL ); ccl_pre( (ea->refcount &0x7FFFFFFF) > 0 );

  ea->refcount--;
  
  if( (ea->refcount & 0x7FFFFFFF) != 0 ) 
    return;

  if( (ea->refcount & 0x80000000) != 0 )
    {
      exit_automaton *pea = s_find_exit_automaton(ea);
      ccl_assert( *pea != NULL );
      *pea = ea->next;
      EA_MANAGER->nb_elements--;
      if( MUST_DECREASE_TABLE() )
	s_ea_manager_resize_table((EA_MANAGER->table_size-13)>>1);
    }

  ccl_delete(ea);
}


			/* --------------- */

void
exit_automaton_set_successor(exit_automaton ea, 
			     uint32_t    src, 
			     uint32_t letter,
			     uint32_t    tgt,
			     int          exit)
{
  ccl_pre( ea != NULL ); 
  ccl_pre( src < ea->nb_local_states );
  ccl_pre( letter < ea->alphabet_size );

  if( exit )
    ea->successor[ea->alphabet_size*src+letter] = 
      exit_automaton_encode_succ_as_exit_state(tgt);
  else
    ea->successor[ea->alphabet_size*src+letter] = 
      exit_automaton_encode_succ_as_local_state(tgt);
}

			/* --------------- */

void
exit_automaton_display_as_dot(exit_automaton ea, 
			      ccl_log_type log,
			      const char **alphabet)
{
  size_t state, letter;

  ccl_log(log,"digraph exit_automaton_%p {\n",ea);
  
  for(state = 0; state < ea->nb_local_states; state++)
    {
      const char *fmt = ea->is_final[state]
	?"L%d[shape=ellipse,peripheries=2];\n"
	:"L%d[shape=ellipse];\n";
      ccl_log(log,fmt,state);
    }
  for(state = 0; state < ea->nb_exit_states; state++)
    {
      ccl_log(log,"E%d[shape=octagon];\n",state);
    }

  for(state = 0; state < ea->nb_local_states; state++)
    {
      for(letter = 0; letter < ea->alphabet_size; letter++)
	{
	  uint32_t succ = exit_automaton_successor(ea,state,letter);
	  const char *fmt = "L%d -> L%d";

	  if( exit_automaton_is_exit_state(succ) )
	    {
	      fmt = "L%d -> E%d ";
	    }

	  ccl_log(log,fmt,state,exit_automaton_decode_succ_state(succ));

	  if( alphabet == NULL ) 
	    ccl_log(log,"[label=\"%d\"];\n",letter);
	  else
	    ccl_log(log,"[label=\"%s\"];\n",alphabet[letter]);
	}
    }

  ccl_log(log,"}\n");
}

			/* --------------- */

exit_automaton
exit_automaton_find_or_add(exit_automaton ea)
{
  exit_automaton result;
  exit_automaton *pea;

  ccl_pre( ea != NULL );
      
  if( (ea->refcount & 0x80000000) != 0 )
    result = exit_automaton_add_reference(ea);
  else if( *(pea = s_find_exit_automaton(ea)) != NULL )
    result = exit_automaton_add_reference(*pea);
  else
    {
      result = *pea = exit_automaton_add_reference(ea);
      result->refcount |= 0x80000000;
      EA_MANAGER->nb_elements++;
      if( MUST_INCREASE_TABLE() )
	s_ea_manager_resize_table((EA_MANAGER->table_size<<1)+13);
    }

  return result;
}

			/* --------------- */

int
exit_automaton_find(exit_automaton ea)
{
  return *s_find_exit_automaton(ea) != NULL;
}

			/* --------------- */

int
exit_automaton_equals(exit_automaton ea1, exit_automaton ea2)
{
  ccl_pre( ea1 != NULL ); ccl_pre( ea2 != NULL );

  if( ea1 == ea2 ) 
    return 1;

  return ea1->nb_local_states == ea2->nb_local_states
    &&   ea1->alphabet_size == ea2->alphabet_size
    &&   ea1->nb_exit_states == ea2->nb_exit_states
    &&   memcmp(ea1->successor,ea2->successor,
		sizeof(uint8_t)*ea1->nb_local_states+
		sizeof(uint32_t)*ea1->nb_local_states*ea1->alphabet_size) == 0;
}

			/* --------------- */

uint32_t
exit_automaton_hashcode(exit_automaton ea)
{
  uint32_t N, s, r = 0;

  ccl_pre( ea != NULL );
  N = ea->nb_local_states*ea->alphabet_size;

  for(s = 0; s < ea->nb_local_states; s++)
    r = 87719*r+19*ea->is_final[s];
  for(s = 0; s < N; s++)
    r = 10949*r+17*ea->successor[s];

  return r;
}

			/* --------------- */

void
exit_automaton_ut_statistics(ccl_log_type log)
{
  int i;
  uint32_t nb_queue = 0;
  uint32_t nb_refcount = 0;
  uint32_t min_refcount = 0xFFFFFFFF;
  uint32_t max_refcount = 0;
  uint32_t max_e_size = 0;

  for(i = 0; i < EA_UT_INIT_TABLE_SIZE; i++)
    {
      exit_automaton ea = EA_MANAGER->table[i];

      if( ea == NULL ) 
	continue;
      nb_queue++;
      for(; ea != NULL; ea = ea->next)
	{
	  uint32_t r = ea->refcount & 0x7FFFFFFF;

	  nb_refcount += r;
	  if( r < min_refcount )
	    min_refcount = r;
	  if( r > max_refcount )
	    max_refcount = r;
	  if( ea->nb_exit_states > max_e_size )
	    max_e_size = ea->nb_exit_states;
	}
    }

  ccl_log(log,"EA uniqueness table statistics :\n");
  ccl_log(log,"--------------------------------\n");
  ccl_log(log," init size = %d\n",EA_UT_INIT_TABLE_SIZE);
  ccl_log(log," fill degree = %d\n",EA_UT_FILL_DEGREE);
  ccl_log(log," table size = %d\n",EA_MANAGER->table_size);
  ccl_log(log," nb entries = %d\n",EA_MANAGER->nb_elements);
  ccl_log(log," mean collision list len = %4.2f\n",
	  (float)EA_MANAGER->nb_elements/(float)nb_queue);
  ccl_log(log," mean sharing count = %4.2f\n",
	  (float)nb_refcount/(float)EA_MANAGER->nb_elements);
  ccl_log(log," refcount range = [%d, %d]\n",min_refcount,max_refcount);
  ccl_log(log," max number of exit states  = %d\n",max_e_size);
  ccl_log(log,"\n");
}

			/* --------------- */

static void
s_ea_manager_resize_table(uint32_t new_size)
{
  uint32_t i = EA_MANAGER->table_size;
  exit_automaton *new_table = ccl_new_array(exit_automaton,new_size);
  exit_automaton *entry = EA_MANAGER->table;
  exit_automaton ea;
  exit_automaton next;
  uint32_t index;

  if( new_table == NULL )
    return;

  while( i-- )
    {
      for(ea = *(entry++); ea; ea = next)
	{
	  next  = ea->next;

	  index = exit_automaton_hashcode(ea) % new_size;
	  ea->next = new_table[index];
	  new_table[index] = ea;
	}
    }

  EA_MANAGER->table_size = new_size;
  ccl_delete(EA_MANAGER->table);
  EA_MANAGER->table = new_table;
}
