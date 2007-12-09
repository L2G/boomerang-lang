/* $Id: shared-automaton.c,v 1.3 2006/01/04 08:08:24 point Exp $ */
#include <ccl-assert.h>
#include "sataf-msa-p.h"
#include "shared-automaton.h"

			/* --------------- */

# define SA_UT_INIT_TABLE_SIZE 1403641
# define SA_UT_FILL_DEGREE     5

typedef struct sa_manager_st {
  uint32_t    table_size;
  shared_automaton  *table;
  uint32_t   nb_elements;
} sa_manager;

			/* --------------- */

#define MUST_INCREASE_TABLE() \
(SA_MANAGER->nb_elements > SA_MANAGER->table_size*SA_UT_FILL_DEGREE)

#define MUST_DECREASE_TABLE() \
(SA_MANAGER->nb_elements < SA_MANAGER->table_size*SA_UT_FILL_DEGREE  \
 && SA_MANAGER->table_size > SA_UT_INIT_TABLE_SIZE)

			/* --------------- */

static sa_manager *SA_MANAGER = NULL;

			/* --------------- */

static shared_automaton
s_create_shared_automaton(uint32_t          depth, 
			  exit_automaton         ea, 
			  shared_automaton *bind_sa,
			  uint32_t     *bind_init);

static void
s_sa_manager_resize_table(uint32_t new_size);

static uint32_t
HVALUE1(exit_automaton ea, shared_automaton *bind_sa, uint32_t *bind_init);

static uint32_t
HVALUE2(exit_automaton ea, sataf_msa *bind);

static void
s_display_as_dot(shared_automaton sa, ccl_log_type log, int marked_state,
		 const char **alphabet, ccl_list visited, ccl_list to_visit);

			/* --------------- */

void
shared_automaton_init(void)
{
  SA_MANAGER = ccl_new(sa_manager);

  SA_MANAGER->table_size  = SA_UT_INIT_TABLE_SIZE;
  SA_MANAGER->table = ccl_new_array(shared_automaton,SA_MANAGER->table_size);
  SA_MANAGER->nb_elements = 0;
}

			/* --------------- */

void
shared_automaton_terminate(void)
{
  if( SA_MANAGER->nb_elements != 0 )
    {
      int i = SA_MANAGER->table_size;

      ccl_warning("WARNING: SA uniqueness table still contains elements\n");
      while( i-- )
	{
	  shared_automaton sa, next;

	  for(sa = SA_MANAGER->table[i]; sa; sa = next)
	    {
	      int j = sa->automaton->nb_exit_states;

	      next = sa->next;
	      exit_automaton_del_reference(sa->automaton);
	      while( j-- )
		sataf_msa_del_reference(sa->bind[j]);
	      ccl_delete(sa);
	    }
	}
    }
  ccl_delete(SA_MANAGER->table);
  ccl_delete(SA_MANAGER);
}

			/* --------------- */

void
shared_automaton_del_reference_(shared_automaton sa)
{
  shared_automaton *psa;
  uint32_t index = HVALUE2(sa->automaton,sa->bind) % SA_MANAGER->table_size;

  for(psa = SA_MANAGER->table+index; *psa && *psa != sa; psa = &((*psa)->next))
    /* do nothing */;

  ccl_assert( *psa == sa );

  *psa = sa->next;

  for(index = 0; index < sa->automaton->nb_exit_states; index++)
    sataf_msa_del_reference(sa->bind[index]);
  exit_automaton_del_reference(sa->automaton);
  ccl_delete(sa);
  
  SA_MANAGER->nb_elements--;
  
  if( MUST_DECREASE_TABLE() )
    s_sa_manager_resize_table((SA_MANAGER->table_size-13)>>1);
}

			/* --------------- */

void
shared_automaton_display_as_dot(shared_automaton sa, ccl_log_type log, 
				int marked_state, const char **alphabet)
{
  int m;
  ccl_list to_visit = ccl_list_create();
  ccl_list visited = ccl_list_create();

  ccl_log(log,"digraph shared_automaton_%p {\n",sa);
  ccl_list_add(to_visit,sa);
  while( ccl_list_get_size(to_visit) > 0 )
    {
      shared_automaton a = (shared_automaton)ccl_list_take_first(to_visit);
      if( a == sa )
	m = marked_state;
      else 
	m = -1;
      s_display_as_dot(a,log,m,alphabet,visited,to_visit);
    }
  ccl_log(log,"}\n");
  ccl_list_delete(to_visit);
  ccl_list_delete(visited);
}

			/* --------------- */

void
shared_automaton_display_scc_as_dot(shared_automaton sa, ccl_log_type log)
{
  ccl_list to_visit = ccl_list_create();
  ccl_list visited = ccl_list_create();

  ccl_log(log,"digraph shared_automaton_%p {\n",sa);

  ccl_list_add(to_visit,sa);
  while( ccl_list_get_size(to_visit) > 0 )
    {
      uint32_t i;
      shared_automaton a = (shared_automaton)ccl_list_take_first(to_visit);
      
      ccl_list_add(visited,a);
      if( shared_automaton_is_zero(a) )
	ccl_log(log,"N%p [label=\"[0]\"];\n",a);
      else if( shared_automaton_is_one(a) )
	ccl_log(log,"N%p [label=\"[1]\"];\n",a);
      else for(i = 0; i < a->automaton->nb_exit_states; i++)
	{
	  ccl_log(log,"N%p -> N%p [label=\"%d\"];\n",a,a->bind[i]->A,
		  a->bind[i]->initial);
	  if( ! ccl_list_has(to_visit,a->bind[i]->A) &&
	      ! ccl_list_has(visited,a->bind[i]->A) )
	    ccl_list_add(to_visit,a->bind[i]->A);
	}
    }
  ccl_log(log,"}\n");

  ccl_list_delete(to_visit);
  ccl_list_delete(visited);
}

			/* --------------- */

shared_automaton
shared_automaton_create_one(uint32_t alphabet_size)
{
  shared_automaton R;
  exit_automaton ea = exit_automaton_create(1,0,alphabet_size);
  exit_automaton one;

  ea->is_final[0] = 1;

  one = exit_automaton_find_or_add(ea);
  R = shared_automaton_find_or_add(one,NULL,NULL);

  exit_automaton_del_reference(ea);
  exit_automaton_del_reference(one);

  return R;
}

			/* --------------- */

shared_automaton
shared_automaton_create_zero(uint32_t alphabet_size)
{
  shared_automaton R;
  exit_automaton ea = exit_automaton_create(1,0,alphabet_size);
  exit_automaton zero;

  ea->is_final[0] = 0;

  zero = exit_automaton_find_or_add(ea);
  R = shared_automaton_find_or_add(zero,NULL,NULL);

  exit_automaton_del_reference(ea);
  exit_automaton_del_reference(zero);

  return R;
}

			/* --------------- */

shared_automaton
shared_automaton_find_or_add(exit_automaton ea, 
			     shared_automaton *bind_sa,
			     uint32_t *bind_init)
{
  shared_automaton *psa;
  uint32_t          i;
  uint32_t         ne;
  uint32_t      index;
  uint32_t      depth;

  ne = ea->nb_exit_states;
  index = HVALUE1(ea,bind_sa,bind_init) % SA_MANAGER->table_size;
  depth = ne>0?bind_sa[0]->depth+1:0;

  ccl_assert( index < SA_MANAGER->table_size );

  for(psa = SA_MANAGER->table+index; *psa; psa = &((*psa)->next))
    {
      ccl_assert( (*psa)->refcount > 0 );

      if( (*psa)->depth != depth || (*psa)->automaton != ea )
	continue;
      i = 0;
      while( i < ne && bind_sa[i] == (*psa)->bind[i]->A &&
	     bind_init[i] == (*psa)->bind[i]->initial )
	i++;
      if( i == ne )
	return shared_automaton_add_reference(*psa);
    }

  *psa = s_create_shared_automaton(depth,ea,bind_sa,bind_init);
  SA_MANAGER->nb_elements++;

  if( MUST_INCREASE_TABLE() )
    s_sa_manager_resize_table((SA_MANAGER->table_size<<1)+13);

  return *psa;
}


			/* --------------- */

static shared_automaton
s_create_shared_automaton(uint32_t          depth, 
			  exit_automaton         ea, 
			  shared_automaton *bind_sa,
			  uint32_t     *bind_init)
{
  uint32_t            i;
  size_t         to_alloc;
  shared_automaton result;

  ccl_pre( ea != NULL );

  to_alloc = sizeof(struct shared_automaton_st);
  if( ea->nb_exit_states > 1 )
    to_alloc += sizeof(sataf_msa)*(ea->nb_exit_states-1);

  result = (shared_automaton)ccl_calloc(to_alloc,1);
  result->next     = NULL;
  result->refcount = 1;
  result->automaton = exit_automaton_add_reference(ea);
  result->depth     = depth;
  for(i = 0; i < ea->nb_exit_states; i++)
    result->bind[i] = sataf_msa_find_or_add(bind_sa[i],bind_init[i]);
  
  return result;
}

			/* --------------- */


static void
s_sa_manager_resize_table(uint32_t new_size)
{
  uint32_t                i;
  shared_automaton *new_table = ccl_new_array(shared_automaton,new_size);
  shared_automaton         sa;
  shared_automaton       next;
  uint32_t            index;

  if( new_table == NULL )
    return;

  for(i = 0; i < SA_MANAGER->table_size; i++)
    {
      for(sa = SA_MANAGER->table[i]; sa; sa = next)
	{
	  next  = sa->next;
	  index = HVALUE2(sa->automaton,sa->bind) % new_size;
	  sa->next = new_table[index];
	  new_table[index] = sa;
	}
    }
  SA_MANAGER->table_size = new_size;
  ccl_delete(SA_MANAGER->table);
  SA_MANAGER->table = new_table;
}

			/* --------------- */

static uint32_t
HVALUE1(exit_automaton ea, shared_automaton *bind_sa, uint32_t *bind_init)
{
  uint32_t i;
  uint32_t r = (uint32_t)ea;
  
  for(i = 0; i < ea->nb_exit_states; i++)
    r = 19*r+MSA_HVALUE(bind_sa[i],bind_init[i]);

  return r;
}

			/* --------------- */

static uint32_t
HVALUE2(exit_automaton ea, sataf_msa *bind)
{
  uint32_t i;
  uint32_t r = (uint32_t)ea;
  
  for(i = 0; i < ea->nb_exit_states; i++)
    r = 19*r+MSA_HVALUE(bind[i]->A,bind[i]->initial);

  return r;
}

			/* --------------- */

static void
s_display_as_dot(shared_automaton sa, ccl_log_type log, int marked_state,
		 const char **alphabet, ccl_list visited, ccl_list to_visit)
{
  uint32_t state, letter;
  
  ccl_list_add(visited,sa);
  
  for(state = 0; state < sa->automaton->nb_local_states; state++)
    {
      const char *color = "white";
      const char   *fmt = sa->automaton->is_final[state]
	?"\"%p-%d\"[style=filled,fillcolor=%s,label=\"L@%d-%d\\n%p\","
	"shape=ellipse,peripheries=2];\n"
	:"\"%p-%d\"[style=filled,fillcolor=%s,label=\"L@%d-%d\\n%p\","
	"shape=ellipse];\n";

      if( state == (uint32_t)marked_state )
	color = "grey";

      ccl_log(log,fmt,sa,exit_automaton_encode_succ_as_local_state(state),
	     color,sa->depth,state,sa);
    }

  for(state = 0; state < sa->automaton->nb_local_states; state++)
    {
      for(letter = 0; letter < sa->automaton->alphabet_size; letter++)
	{
	  uint32_t succ = exit_automaton_successor(sa->automaton,state,letter);
	  const char *fmt = "\"%p-%d\" -> \"%p-%d\"";

	  if( exit_automaton_is_exit_state(succ) )
	    {
	      sataf_msa msa = 
		sa->bind[exit_automaton_decode_succ_state(succ)];

	      ccl_log(log,fmt,
		      sa,
		      exit_automaton_encode_succ_as_local_state(state),
		      msa->A,
		      exit_automaton_encode_succ_as_local_state(msa->initial));
	    }
	  else
	    {
	      ccl_log(log,fmt,
		      sa,
		      exit_automaton_encode_succ_as_local_state(state),
		      sa,
		      succ);
	    }

	  if( alphabet == NULL ) 
	    ccl_log(log,"[label=\"%d\"];\n",letter);
	  else
	    ccl_log(log,"[label=\"%s\"];\n",alphabet[letter]);
	}
    }

  for(state = 0; state < sa->automaton->nb_exit_states; state++)
    {
      if( ! ccl_list_has(visited,sa->bind[state]->A) &&
	  ! ccl_list_has(to_visit,sa->bind[state]->A) )
	ccl_list_add(to_visit,sa->bind[state]->A);
    }
}

			/* --------------- */

void
shared_automaton_table_statistics(ccl_log_type log)
{
  int i;
  uint32_t nb_queue = 0;
  uint32_t nb_refcount = 0;
  uint32_t min_refcount = 0xFFFFFFFF;
  uint32_t max_refcount = 0;

  for(i = 0; i < SA_UT_INIT_TABLE_SIZE; i++)
    {
      shared_automaton sa = SA_MANAGER->table[i];

      if( sa == NULL ) 
	continue;
      nb_queue++;
      for(; sa != NULL; sa = sa->next)
	{
	  nb_refcount += sa->refcount;
	  if( sa->refcount < min_refcount )
	    min_refcount = sa->refcount;
	  if( sa->refcount > max_refcount )
	    max_refcount = sa->refcount;
	}
    }

  ccl_log(log,"SA uniqueness table statistics :\n");
  ccl_log(log,"--------------------------------\n");
  ccl_log(log,"init size = %d\n",SA_UT_INIT_TABLE_SIZE);
  ccl_log(log,"fill degree = %d\n",SA_UT_FILL_DEGREE);
  ccl_log(log,"table size = %d\n",SA_MANAGER->table_size);
  ccl_log(log,"nb entries = %d\n",SA_MANAGER->nb_elements);
  ccl_log(log,"mean collision list len = %4.2f\n",
	  (float)SA_MANAGER->nb_elements/(float)nb_queue);
  ccl_log(log,"mean sharing count = %4.2f\n",
	  (float)nb_refcount/(float)SA_MANAGER->nb_elements);
  ccl_log(log,"refcount range = [%d, %d]\n",min_refcount,max_refcount);
  ccl_log(log,"\n");
}
