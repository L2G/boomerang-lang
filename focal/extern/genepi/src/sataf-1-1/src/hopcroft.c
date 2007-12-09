/* $Id: hopcroft.c,v 1.2 2006/01/04 08:05:27 point Exp $ */
#include <ccl-assert.h>
#include <ccl-list.h>
#include <ccl-memory.h>
#include <ccl-bittable.h>
#include "hopcroft.h"
#include "hopcroft2.h"
#include "exit-automaton.h"

# undef DOUBLE_HOPCROFT 
# define PAIR_PAGE_SIZE 10000

typedef struct pred_st pred;
struct pred_st {
  pred       *next;
  uint32_t state;
};

typedef struct  pair_st pair;
struct pair_st {
  int next;
  uint32_t c;
  uint32_t a;
};

typedef struct class_element_st class_element;
struct class_element_st {
  class_element *prev;
  class_element *next;
  uint32_t state;
  uint32_t classid;
  pred **preds;
};

			/* --------------- */

typedef struct equiv_class_st equiv_class;
struct equiv_class_st {
  class_element *elements;
  uint32_t         size;
  uint32_t        share;
  uint32_t         next;
  uint32_t         twin;
};

			/* --------------- */

typedef struct hopcroft_data_st {
  exit_automaton ea;
  class_element *elements;
  equiv_class *classes;
  uint32_t nb_classes;
  uint32_t classes_sz;
  pred *free_pred;
  pred **free_pred_table;
  int L;
  ccl_bittable in_L;
  ccl_list tmp[2];
  pred preds[1];
} hopcroft_data;

			/* --------------- */

# define PAIR_POOL_INIT_SIZE 10000

static pair *PAIR_POOL = NULL;
static int PAIR_POOL_SIZE = 0;
static int FREE_PAIR = 0;

			/* --------------- */

static hopcroft_data *
s_hopcroft_init(exit_automaton ea);

static exit_automaton
s_hopcroft_terminate(hopcroft_data *hd, uint32_t *h);

static equiv_class *
s_new_class(hopcroft_data *hd);

static void
s_initial_refinements(hopcroft_data *hd);

static void
s_refine_wrt_block(hopcroft_data *hd, uint32_t cindex, uint32_t a);

static void
s_increase_pair_pool_size(void);

static void
s_move_state_to(hopcroft_data *hd, uint32_t s, uint32_t to);

			/* --------------- */

#if 0
static void s_print_classes(hopcroft_data *hd)
{
  int i;

  for(i = 0; i < hd->nb_classes; i++)
    {
      class_element *e;

      ccl_log(CCL_LOG_ERROR,"C%d = { ",i);
      for(e = hd->classes[i].elements->next; e != hd->classes[i].elements;
	  e = e->next)
	{
	  ccl_log(CCL_LOG_ERROR,"%d ",e->state);
	}
      ccl_log(CCL_LOG_ERROR,"}\n");
    }
ccl_log(CCL_LOG_ERROR,"\n");
}
#endif
			/* --------------- */

static void s_check_elements(hopcroft_data *hd)
{
  size_t i;
  class_element *e, *se;

  for(i = 0; i< hd->ea->nb_local_states+hd->ea->nb_exit_states; i++)
    {
      e = hd->elements+i;
      for(se = hd->classes[e->classid].elements->next;
	  se != hd->classes[e->classid].elements; se = se->next)
	{
	  if( se == e )
	    break;
	}
      ccl_assert( se != hd->classes[e->classid].elements );
    }
}

			/* --------------- */


void
hopcroft_init(void)
{
  PAIR_POOL = ccl_new_array(pair,PAIR_POOL_INIT_SIZE);
  PAIR_POOL_SIZE = PAIR_POOL_INIT_SIZE;
  hopcroft_init2();
}

			/* --------------- */

void
hopcroft_terminate(void)
{
  hopcroft_terminate2();
  if( PAIR_POOL != NULL )
    {
      ccl_delete(PAIR_POOL);
      PAIR_POOL = NULL;
      PAIR_POOL_SIZE = 0;
    }
}

			/* --------------- */

#ifdef DOUBLE_HOPCROFT
static exit_automaton
s_minimize(exit_automaton ea, uint32_t *h)
#else
exit_automaton
exit_automaton_minimize(exit_automaton ea, uint32_t *h)
#endif
{
  uint32_t          c;
  uint32_t          a;
  exit_automaton      R;
  hopcroft_data     *hd;
  pair               *p;
  
  FREE_PAIR = 0;

#ifndef DOUBLE_HOPCROFT
  if( ea->nb_local_states == 1 && ea->nb_exit_states == 0 )
    {
      h[0] = 0;

      return exit_automaton_add_reference(ea);
    }

  if( exit_automaton_find(ea) )
    {
      uint32_t i;
      for(i = 0; i < ea->nb_local_states; i++)
	h[i] = i;
      return exit_automaton_find_or_add(ea);
    }
#endif

  hd = s_hopcroft_init(ea);

  while( hd->L >= 0 )
    {
      p = PAIR_POOL+hd->L;
      hd->L = p->next;

      c = p->c;
      a = p->a;

      ccl_assert( ccl_bittable_has(hd->in_L,c*ea->alphabet_size+a) );

      ccl_bittable_unset(hd->in_L,c*ea->alphabet_size+a);

      s_refine_wrt_block(hd,p->c,p->a);
      if( 0 ) s_check_elements(hd);
    }

  R = s_hopcroft_terminate(hd,h);

  if(0){
    uint32_t i;
    uint32_t *h2 = ccl_new_array(uint32_t,ea->nb_local_states);
    exit_automaton R2 = exit_automaton_minimize2(ea,h2);

    ccl_assert( exit_automaton_equals(R,R2) );
    for(i = 0; i < ea->nb_local_states; i++)
      ccl_assert( h2[i] == h[i] );
    exit_automaton_del_reference(R2);
    ccl_delete(h2);
  }

  return R;
}

			/* --------------- */

#ifdef DOUBLE_HOPCROFT
exit_automaton
exit_automaton_minimize(exit_automaton ea, uint32_t *h)
{
  uint32_t i;
  exit_automaton R;

  if( exit_automaton_is_zero_or_one(ea) )
    {
      R = exit_automaton_add_reference(ea);
      for(i = 0; i < ea->nb_local_states; i++)
	h[i] = 0;
    }
  else
    {
      uint32_t *h1 = ccl_new_array(uint32_t,ea->nb_local_states);
      exit_automaton min1 = s_minimize(ea,h1);
      uint32_t *h2 = ccl_new_array(uint32_t,min1->nb_local_states);

      R = s_minimize(min1,h2);
      
      for(i = 0; i < ea->nb_local_states; i++)
	h[i] = h2[h1[i]];
      ccl_delete(h2);
      ccl_delete(h1);
      exit_automaton_del_reference(min1);
    }
  return R;
}
#endif
			/* --------------- */

static hopcroft_data *
s_hopcroft_init(exit_automaton ea)
{  
  hopcroft_data *hd;
  class_element  *e;
  uint32_t      s;
  uint32_t      a;
  equiv_class    *C;

  hd = ccl_calloc(sizeof(hopcroft_data)+
		  sizeof(pred)*((ea->nb_local_states+ea->nb_exit_states)*ea->alphabet_size)+
		  sizeof(pred*)*((ea->nb_local_states+ea->nb_exit_states)*ea->alphabet_size),1);
  hd->ea = ea;
  hd->elements = ccl_new_array(class_element,(ea->nb_local_states+ea->nb_exit_states)); 
  hd->classes = ccl_new_array(equiv_class,ea->nb_local_states+ea->nb_exit_states);
  hd->nb_classes = 0;
  hd->classes_sz = (ea->nb_local_states+ea->nb_exit_states);

  hd->L = -1;
  hd->in_L = ccl_bittable_create((ea->nb_local_states+ea->nb_exit_states)*ea->alphabet_size);

  hd->free_pred = hd->preds;
  hd->free_pred_table = (pred **)
    (hd->preds+((ea->nb_local_states+ea->nb_exit_states)*ea->alphabet_size));
  
  for(e = hd->elements, s = 0; s < ea->nb_local_states+ea->nb_exit_states; s++, e++)
    {
      e->prev = e-1;
      e->next = e+1;
      e->state = s;
      e->classid = 0;
      
      if( e->preds == NULL )
	{
	  ccl_assert( s < ea->nb_local_states );

	  e->preds = hd->free_pred_table;
	  hd->free_pred_table += ea->alphabet_size;
	  ccl_memzero(e->preds,ea->alphabet_size*sizeof(pred *));
	}
      
      if( s < ea->nb_local_states )
	{
	  for(a = 0; a < ea->alphabet_size; a++)
	    {
	      pred *p;
	      class_element *esucc;
	      uint32_t succ = exit_automaton_successor(ea,s,a);

	      if( exit_automaton_is_exit_state(succ) )
		succ = ea->nb_local_states+
		  exit_automaton_decode_succ_state(succ);
	      else
		succ = exit_automaton_decode_succ_state(succ);
	      esucc = hd->elements+succ;
	      
	      if( esucc->preds == NULL )
		{
		  esucc->preds = hd->free_pred_table;
		  hd->free_pred_table += ea->alphabet_size;
		  ccl_memzero(esucc->preds,ea->alphabet_size*sizeof(pred *));
		}
	      
	      p = hd->free_pred++;
	      p->next = esucc->preds[a];
	      esucc->preds[a] = p;
	      p->state = s;
	    }
	}
    }

  C = s_new_class(hd);
  C->size = ea->nb_local_states+ea->nb_exit_states;

  C->elements->next = hd->elements;
  C->elements->prev = &(hd->elements[ea->nb_local_states+ea->nb_exit_states-1]);

  hd->elements->prev = C->elements;
  hd->elements[ea->nb_local_states+ea->nb_exit_states-1].next = C->elements;

  {
    for(s = 0; s < ea->nb_exit_states; s++)
      {
	uint32_t sz = ea->alphabet_size;
	uint32_t cid = hd->nb_classes;

	C = s_new_class(hd);
	s_move_state_to(hd,ea->nb_local_states+s,cid);

	for(a = 0; a < sz; a++)
	  {
	    pair *p;
	    int pi;

	    if( FREE_PAIR == PAIR_POOL_SIZE )
	      s_increase_pair_pool_size();

	    pi = FREE_PAIR++;
	    p = PAIR_POOL+pi;
	    p->a = a;
	    p->next = hd->L;
	    hd->L = pi;

	    ccl_bittable_set(hd->in_L,cid*sz+a);		  
	    p->c = cid;
	  }
      }
  }

  ccl_assert( hd->classes[0].size > 0 );

  hd->tmp[0] = ccl_list_create();
  hd->tmp[1] = ccl_list_create();

  s_initial_refinements(hd);

  return hd;
}

			/* --------------- */

static exit_automaton
s_hopcroft_terminate(hopcroft_data *hd, uint32_t *h)
{
  uint32_t s;
  uint32_t a;
  exit_automaton result;
  uint32_t nb_classes = hd->nb_classes;
  uint32_t *aci = NULL;

  if( hd->ea->nb_exit_states )
    {
      uint32_t k = 0;
      aci = ccl_new_array(uint32_t,nb_classes);
      for(s = 0; s < nb_classes; s++)
	{
	  if( hd->classes[s].elements->next->state >= hd->ea->nb_local_states )
	    continue;
	  aci[s] = k++;
	}
      nb_classes -= hd->ea->nb_exit_states;
    }

  result = exit_automaton_create(nb_classes,hd->ea->nb_exit_states,
				 hd->ea->alphabet_size);

  for(s = 0; s < hd->ea->nb_local_states; s++)
    {
      uint32_t src = aci?aci[hd->elements[s].classid]
	:hd->elements[s].classid; 

      h[s] = src;
      result->is_final[src] = hd->ea->is_final[s];

      for(a = 0; a < hd->ea->alphabet_size; a++)
	{
	  uint32_t  succ = exit_automaton_successor(hd->ea,s,a);
	  uint32_t index = exit_automaton_decode_succ_state(succ);

	  if( exit_automaton_is_exit_state(succ) )
	    {
	      exit_automaton_set_successor(result,src,a,index,1);
	    }
	  else if( aci ) 
	    {
	      exit_automaton_set_successor(result,src,a,
					   aci[hd->elements[index].classid],0);
	    }
	  else 
	    {
	      exit_automaton_set_successor(result,src,a,
					   hd->elements[index].classid,0);
	    }
	}
    }

  if( aci != NULL )
    ccl_delete(aci);

  for(s = 0; s < hd->nb_classes; s++)
    ccl_delete(hd->classes[s].elements);

  ccl_bittable_delete(hd->in_L);
  ccl_delete(hd->elements);
  ccl_delete(hd->classes);

  ccl_list_delete(hd->tmp[0]);
  ccl_list_delete(hd->tmp[1]);

  ccl_delete(hd);

  return result;
}


			/* --------------- */

static equiv_class *
s_new_class(hopcroft_data *hd)
{
  equiv_class *C;

  ccl_pre( hd->nb_classes < hd->classes_sz );

  if( hd->nb_classes == hd->classes_sz )
    {
      uint32_t           i;
      equiv_class *new_table = ccl_new_array(equiv_class,hd->classes_sz<<1);

      for(i = 0; i < hd->classes_sz; i++)
	new_table[i] = hd->classes[i];

      ccl_delete(hd->classes);
      hd->classes = new_table;
      hd->classes_sz <<= 1; 
    }

  C = hd->classes+hd->nb_classes++;

  C->elements = ccl_new(class_element);
  C->elements->prev = C->elements->next = C->elements;
  C->size = 0;

  return C;
}


			/* --------------- */

static void
s_move_state_to(hopcroft_data *hd, uint32_t s, uint32_t to)
{
  class_element     *e = hd->elements+s;
  class_element  *next = e->next;
  class_element  *prev = e->prev;
  equiv_class       *c = hd->classes+e->classid;
  equiv_class   *ctwin = hd->classes+to;

  /* remove e from c */
  c->size--;
  prev->next = e->next;
  next->prev = prev;

  /* insert e into ctwin */
  ctwin->size++;
  e->classid    = to;
  e->next       = ctwin->elements->next;
  e->prev       = ctwin->elements;
  e->next->prev = e;
  e->prev->next = e;
}

			/* --------------- */

static void
s_refine_wrt_predicate(hopcroft_data *hd, int final_states, uint32_t eas, 
		       uint32_t al)
{
  int test;
  uint32_t     i ;
  uint32_t   max = hd->nb_classes;
  class_element *e;
  class_element *next;
  equiv_class   *c;
  equiv_class   *ctwin;
  
  for(i = 0; i < max; i++)
    {
      c     = hd->classes+i;
      ctwin = NULL;
      e     = c->elements->next;

      if( c->size == 1 ) 
	continue;
      while( e != c->elements )
	{
	  next = e->next;

	  if( final_states )
	    test = e->state < hd->ea->nb_local_states && 
	      hd->ea->is_final[e->state];
	  else
	    test = (exit_automaton_successor(hd->ea,e->state,al) == eas);
	  if( test )
	    {
	      if( ctwin == NULL )
		{
		  ctwin = s_new_class(hd);
		  c = hd->classes+i;
		}
	      
	      s_move_state_to(hd,e->state,hd->nb_classes-1);
	    }

	  e = next;
	}
      
      if( c->size == 0 )
	{
	  hd->nb_classes--;
	  ccl_delete(c->elements);
	  c->elements = ctwin->elements;
	  c->size = ctwin->size;
	  for(e = c->elements->next; e != c->elements; e = e->next)
	    e->classid = i;
	  ctwin = NULL;
	}

      if( ctwin != NULL )
	{
	  uint32_t ctwini = hd->nb_classes-1;
	  uint32_t sz = hd->ea->alphabet_size;
	  uint32_t a;

	  for(a = 0; a < sz; a++)
	    {
	      pair *p;
	      int pi;

	      if( FREE_PAIR == PAIR_POOL_SIZE )
		s_increase_pair_pool_size();

	      pi = FREE_PAIR++;
	      p = PAIR_POOL+pi;
	      p->a = a;
	      p->next = hd->L;
	      hd->L = pi;


	      if( ccl_bittable_has(hd->in_L,i*sz+a) )
		{
		  ccl_bittable_set(hd->in_L,ctwini*sz+a);		  
		  p->c = ctwini;
		}
#ifdef DOUBLE_HOPCROFT
	      else if( c->size <= ctwin->size )
		{
		  ccl_bittable_set(hd->in_L,i*sz+a);
		  p->c = i;
		}
#endif
	      else
		{
		  ccl_bittable_set(hd->in_L,ctwini*sz+a);
		  p->c = ctwini;
		}
	    }
	}
    }
}

			/* --------------- */

static void
s_refine_wrt_exits_states(hopcroft_data *hd)
{  
  uint32_t s, smax = hd->ea->nb_exit_states;
  uint32_t eas;
  uint32_t a, amax = hd->ea->alphabet_size;

  for(s = 0; s < smax; s++)
    {
      eas = exit_automaton_encode_succ_as_exit_state(s);
      for(a = 0; a < amax; a++)
	{
	  s_refine_wrt_predicate(hd,0,eas,a);
	  if( hd->nb_classes == hd->ea->nb_local_states )
	    return;
	}

    }
}

			/* --------------- */

static void
s_refine_wrt_final_states(hopcroft_data *hd)
{
  s_refine_wrt_predicate(hd,1,0,0);
}

			/* --------------- */

static void
s_initial_refinements(hopcroft_data *hd)
{
  s_refine_wrt_final_states(hd);
  if(0 ) s_refine_wrt_exits_states(hd);
}

			/* --------------- */

static void
s_refine_wrt_block(hopcroft_data *hd, uint32_t Pindex, uint32_t a)
{
  int qi;
  pred *q;
  ccl_pair      pc;
  equiv_class   *P = hd->classes+Pindex;
  class_element *p;
  uint32_t    CR = 0xFFFFFFFF;
  uint32_t    cr;

  /* 
   * Compute blocks 'B' s.t B\cap pred(P,a) is not empty.
   * Each counter 'share' indicates the number of states shared between
   * B and pred(P,a). If 'share' == 'B.size' then 'B' is a subset of 
   * pred(P,a).
   */
  for(p = P->elements->next; p != P->elements; p = p->next)
    {
      for(q = p->preds[a]; q; q = q->next)
	{
	  int i = hd->elements[q->state].classid;
	  if( hd->classes[i].share == 0 )
	    {
	      hd->classes[i].next = CR;
	      CR = i;
	    }
	  hd->classes[i].share++;
	}
    }

  /* 
   * No block shares a state with pred(P,a).
   */
  if( CR == 0xFFFFFFFF )
    return;

  for(cr = CR; cr != 0xFFFFFFFF; cr = hd->classes[cr].next)
    {
      if( hd->classes[cr].share != hd->classes[cr].size )
	ccl_list_add(hd->tmp[0],(void *)cr);
    }

  if( ! ccl_list_is_empty(hd->tmp[0]) )
    {
      ccl_list_sort(hd->tmp[0], NULL);
      for(pc = FIRST(hd->tmp[0]); pc; pc = CDR(pc))
	{
	  int cr = (int)CAR(pc);

	  ccl_assert( hd->classes[cr].twin == 0 );
	  
	  hd->classes[cr].twin = hd->nb_classes;
	  s_new_class(hd);
	}
    }

  P = hd->classes+Pindex;

  for(p = P->elements->next; p != P->elements; p = p->next)
    {
      for(q = p->preds[a]; q; q = q->next)
	{
	  int i = hd->elements[q->state].classid;

	  if( hd->classes[i].share == hd->classes[i].size )
	    continue;

	  ccl_assert( hd->classes[i].twin != 0 );
	  ccl_assert( ! ccl_list_has(hd->tmp[1],(void *)q->state) );
	  ccl_list_add(hd->tmp[1],(void *)q->state);
	}
    }

  if( ! ccl_list_is_empty(hd->tmp[1]) )
    {
      /* 
       * For each block 'B' that is not a subset of pred(P,a) we split it
       * and move elements of the intersection into the twin of 'B'.
       */
      for(pc = FIRST(hd->tmp[1]); pc; pc = CDR(pc))
	{
	  qi = (int)CAR(pc);
	  s_move_state_to(hd,qi,hd->classes[hd->elements[qi].classid].twin);
	}
  
      ccl_list_clear(hd->tmp[1],NULL);
    }

  if( ! ccl_list_is_empty(hd->tmp[0]) )
    {
      int twin, l, sz = hd->ea->alphabet_size;
      for(pc = FIRST(hd->tmp[0]); pc; pc = CDR(pc))
	{
	  cr = (uint32_t)CAR(pc);
      
	  twin = hd->classes[cr].twin;
      
	  {
	    for(l = 0; l < sz; l++)
	      {
		int pi;
		pair *p;
	    
		if( FREE_PAIR == PAIR_POOL_SIZE )
		  s_increase_pair_pool_size();
	    
		pi = FREE_PAIR++;
		p = PAIR_POOL+pi;
		p->a = l;
		p->next = hd->L;
		hd->L = pi;
	      
		if( ccl_bittable_has(hd->in_L,cr*sz+l) )
		  {
		    ccl_bittable_set(hd->in_L,twin*sz+l);
		    p->c  = twin;
		  }
# ifdef DOUBLE_HOPCROFT
		else if( hd->classes[cr].size <= hd->classes[twin].size )
		  {
		    ccl_bittable_set(hd->in_L,cr*sz+l);
		    p->c = cr;
		  }
# endif
		else
		  {
		    ccl_bittable_set(hd->in_L,twin*sz+l);
		    p->c = twin;
		  }
	      }
	  }
	}
      ccl_list_clear(hd->tmp[0],NULL);
    }

  while( CR != 0xFFFFFFFF )
    {
      a = hd->classes[CR].next;
      hd->classes[CR].next  = 0xFFFFFFFF;
      hd->classes[CR].share = 0;
      hd->classes[CR].twin  = 0;
      CR = a;
    }
}

			/* --------------- */

static void
s_increase_pair_pool_size(void)
{
  PAIR_POOL_SIZE <<= 1;
  PAIR_POOL = ccl_realloc(PAIR_POOL,PAIR_POOL_SIZE*sizeof(pair));
}
