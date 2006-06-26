/* $Id: sataf-automaton.c,v 1.3 2006/01/25 16:14:53 point Exp $ */
#include <ccl-pool.h>
#include "exit-automaton.h"
#include "sataf-memo.h"
#include "sataf-automaton.h"

# define MAX_SA_SIZE_DIV_2 100
# define SA_POOL_PAGE_SIZE 10000

typedef struct sea_automaton_st {
  struct sataf_automaton_st super;
  exit_automaton               ea;
  uint32_t                    s;
} *sea_automaton;

			/* --------------- */

static void
s_sea_destroy(sataf_automaton self);

static uint32_t
s_sea_get_alphabet_size(sataf_automaton self);

# define s_sea_to_string NULL

static sataf_automaton
s_sea_succ(sataf_automaton self, uint32_t letter);

static int
s_sea_is_final(sataf_automaton self);

static int
s_sea_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_sea_hashcode(sataf_automaton self);

# define s_sea_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st SEA_METHODS = {
  "SA-SEA",
  sizeof(struct sea_automaton_st),
  s_sea_destroy,
  s_sea_get_alphabet_size,
  s_sea_to_string,
  s_sea_succ,
  s_sea_is_final,
  s_sea_is_equal_to,
  s_sea_hashcode,
  s_sea_to_dot
};

			/* --------------- */

static ccl_pool POOLS[MAX_SA_SIZE_DIV_2];

			/* --------------- */

static sataf_automaton
s_create_from_exit_automaton(exit_automaton ea, uint32_t s0);

static void *
s_allocate_automaton(uint32_t size);

static void 
s_delete_automaton(sataf_automaton a);

			/* --------------- */

void
sataf_automaton_init(void)
{  
  int i;
  
  for(i = 0; i < MAX_SA_SIZE_DIV_2; i++)
    POOLS[i] = NULL;
}

			/* --------------- */

void
sataf_automaton_terminate(void)
{
  int i;
  
  for(i = 0; i < MAX_SA_SIZE_DIV_2; i++)
    ccl_zdelete(ccl_pool_delete,POOLS[i]);
}

			/* --------------- */

sataf_automaton
sataf_automaton_create(size_t                     size, 
		       sataf_automaton_methods methods)
{
  sataf_automaton result;

  ccl_pre( size >= sizeof(struct sataf_automaton_st) );
  ccl_pre( size == methods->size );
  ccl_pre( methods != NULL );

  result = s_allocate_automaton(size);

  result->methods = methods;
  result->refcount = 1;
 
  return result;
}

			/* --------------- */

void
sataf_automaton_del_reference_(sataf_automaton a)
{
  if( a->methods->destroy != NULL )
    a->methods->destroy(a);
  
  s_delete_automaton(a);
}

			/* --------------- */

char *
sataf_automaton_to_string(sataf_automaton self)
{
  char *result;

  ccl_pre( self != NULL );

  if( self->methods->to_string == NULL )
    {
      result = ccl_string_format_new("%s@%p",
				     sataf_automaton_get_type(self),
				     self);
    }
  else
    {
      result = self->methods->to_string(self);
    }

  return result;
}

			/* --------------- */

uint32_t 
sataf_automaton_get_alphabet_size(sataf_automaton self)
{
  ccl_pre( self != NULL );
  ccl_pre( self->methods->get_alphabet_size != NULL );

  return self->methods->get_alphabet_size(self);
}

			/* --------------- */

sataf_automaton
sataf_automaton_succ(sataf_automaton self, uint32_t letter)
{
  ccl_pre( self != NULL );
  ccl_pre( letter <  sataf_automaton_get_alphabet_size(self) );
  ccl_pre( self->methods->succ != NULL );

  return self->methods->succ(self,letter);
}

			/* --------------- */

int
sataf_automaton_is_final(sataf_automaton self)
{
  ccl_pre( self != NULL );
  if( self->methods->is_final == NULL )
    return 0;

  return self->methods->is_final(self);
}

			/* --------------- */

int
sataf_automaton_equals(sataf_automaton self, 
		       sataf_automaton other)
{
  ccl_pre( self != NULL );
  ccl_pre( other != NULL );

  if( sataf_automaton_get_type(self) != 
      sataf_automaton_get_type(other) )
    return 0;

  if( sataf_automaton_get_alphabet_size(self) != 
      sataf_automaton_get_alphabet_size(other) )
    return 0;

  if( self->methods->equals == NULL )
    return self == other;

  return self->methods->equals(self,other);
}

			/* --------------- */

uint32_t
sataf_automaton_hashcode(sataf_automaton self)     
{
  uint32_t val;

  ccl_pre( self != NULL );
  if( self->methods->hashcode == NULL )
    val = (uint32_t)self;
  else 
    val = self->methods->hashcode(self);
  
  return val;
}

			/* --------------- */

void
sataf_automaton_to_dot(sataf_automaton   self,
		       const char  **alphabet,
		       const char *graph_name,
		       const char *graph_type)
{
  ccl_pre( self != NULL );

  if( self->methods->to_dot == NULL )
    sataf_automaton_default_to_dot(self,alphabet,graph_name,graph_type);
  else
    self->methods->to_dot(self,alphabet,graph_name,graph_type);
}


			/* --------------- */

typedef struct memo_st MEMO;
struct memo_st {
  sataf_memo   super;
  int visited_and_id;
  MEMO         *next;
};

			/* --------------- */

static void
s_init_memo(sataf_memo *m, void *data)
{
  MEMO        *mptr = (MEMO *)m;
  uint32_t *idptr = (uint32_t *)data;

  mptr->next = NULL;
  mptr->visited_and_id = *idptr << 1;
  (*idptr)++;
}

			/* --------------- */

void
sataf_automaton_default_to_dot(sataf_automaton   self, 
			       const char     **sigma,
			       const char *graph_name,
			       const char *graph_type)
{
  MEMO *to_visit;
  uint32_t sigma_size;
  uint32_t state_id;
  sataf_memorizer M;

  if( graph_name == NULL )
    ccl_log(CCL_LOG_DISPLAY,"%s sataf_automaton_%p {\n",graph_type,self);
  else
    ccl_log(CCL_LOG_DISPLAY,"%s %s {\n",graph_type,graph_name);

  state_id   = 0;
  M          = sataf_memorizer_create(sizeof(MEMO),s_init_memo,&state_id,NULL);
  to_visit   = (MEMO *)sataf_memorizer_remind(M,self);
  sigma_size = sataf_automaton_get_alphabet_size(self);

  while( to_visit != NULL )
    {
      uint32_t label;
      uint32_t l;
      MEMO *m = to_visit;

      to_visit = m->next;
      m->next = NULL;

      m->visited_and_id |= 1;
      label = m->visited_and_id>>1;

      if( self->methods->to_string != NULL )
	{
	  char *s = sataf_automaton_to_string(m->super.A);
	  ccl_log(CCL_LOG_DISPLAY,"N%p[shape=circle,label=\"%s\"",m,s);
	  ccl_delete(s);
	}
      else
	{
	  ccl_log(CCL_LOG_DISPLAY,"N%p[shape=circle,label=\"%d\"",m,label);
	}

      if( m->super.A == self ) 
	ccl_log(CCL_LOG_DISPLAY,",style=filled,fillcolor=grey");
      if( sataf_automaton_is_final(m->super.A) )
	ccl_log(CCL_LOG_DISPLAY,",peripheries=2");

      ccl_log(CCL_LOG_DISPLAY,"]\n");

      for(l = 0; l < sigma_size; l++)
	{
	  MEMO *succ = (MEMO *)sataf_memorizer_succ(M,m->super.A,l);
	  	  
	  if( sigma == NULL )
	    ccl_log(CCL_LOG_DISPLAY,"N%p->N%p[label=\"%d\"]\n",m,succ,l);
	  else
	    ccl_log(CCL_LOG_DISPLAY,"N%p->N%p[label=\"%s\"]\n",m,succ,
		    sigma[l]);

	  if( (succ->visited_and_id & 0x1) == 0 && 
	      (succ->next == NULL && to_visit != succ) )
	    {
	      /* not visited and not in the queue */
	      succ->next = to_visit;
	      to_visit = succ;
	    }
	}
    }

  sataf_memorizer_delete(M);

  ccl_log(CCL_LOG_DISPLAY,"}\n");
}


			/* --------------- */

sataf_automaton
sataf_automaton_create_zero(uint32_t alphabet_size)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(1,0,alphabet_size);
  sataf_automaton R;

  ea->is_final[0] = 0;
  for(i = 0; i< alphabet_size; i++)
    exit_automaton_set_successor(ea,0,i,0,0);

  R = s_create_from_exit_automaton(ea,0);
  exit_automaton_del_reference(ea);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_one(uint32_t alphabet_size)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(1,0,alphabet_size);
  sataf_automaton R;

  ea->is_final[0] = 1;
  for(i = 0; i< alphabet_size; i++)
    exit_automaton_set_successor(ea,0,i,0,0);

  R = s_create_from_exit_automaton(ea,0);
  exit_automaton_del_reference(ea);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_epsilon(uint32_t alphabet_size)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(2,0,alphabet_size);
  sataf_automaton R;

  ea->is_final[0] = 1;
  ea->is_final[1] = 0;
  for(i = 0; i< alphabet_size; i++)
    {
      exit_automaton_set_successor(ea,0,i,1,0);
      exit_automaton_set_successor(ea,1,i,1,0);
    }

  R = s_create_from_exit_automaton(ea,0);
  exit_automaton_del_reference(ea);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_letter(uint32_t alphabet_size, uint32_t a)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(3,0,alphabet_size);
  sataf_automaton R;

  ea->is_final[0] = 0;
  ea->is_final[1] = 1;
  ea->is_final[2] = 0;

  for(i = 0; i < alphabet_size; i++)
    {
      if( i == a ) exit_automaton_set_successor(ea,0,i,1,0);
      else         exit_automaton_set_successor(ea,0,i,2,0);
      exit_automaton_set_successor(ea,1,i,2,0);
      exit_automaton_set_successor(ea,2,i,2,0);
    }

  R = s_create_from_exit_automaton(ea,0);
  exit_automaton_del_reference(ea);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_alphabet(uint32_t alphabet_size)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(3,0,alphabet_size);
  sataf_automaton R;

  ea->is_final[0] = 0;
  ea->is_final[1] = 1;
  ea->is_final[2] = 0;

  for(i = 0; i < alphabet_size; i++)
    {
      exit_automaton_set_successor(ea,0,i,1,0);
      exit_automaton_set_successor(ea,1,i,2,0);
      exit_automaton_set_successor(ea,2,i,2,0);
    }

  R = s_create_from_exit_automaton(ea,0);
  exit_automaton_del_reference(ea);

  return R;

}

			/* --------------- */

sataf_automaton
sataf_automaton_create_from_arrays(uint32_t      nb_states,
				   uint32_t  alphabet_size,
				   uint32_t             s0,
				   const uint8_t *is_final,
				   const uint32_t    *succ)
{
  uint32_t      i;
  exit_automaton ea = exit_automaton_create(nb_states,0,alphabet_size);
  sataf_automaton R = s_create_from_exit_automaton(ea,s0);

  ccl_memcpy(ea->is_final,is_final,nb_states);
  for(i = 0; i < alphabet_size*nb_states; i++)
    ea->successor[i] = exit_automaton_encode_succ_as_local_state(succ[i]);

  exit_automaton_del_reference(ea);

  return R;
}

			/* --------------- */

static void
s_sea_destroy(sataf_automaton self)
{
  sea_automaton sea = (sea_automaton)self;

  exit_automaton_del_reference(sea->ea);
}

			/* --------------- */

static uint32_t
s_sea_get_alphabet_size(sataf_automaton self)
{
  sea_automaton sea = (sea_automaton)self;

  return sea->ea->alphabet_size;
}

			/* --------------- */

static sataf_automaton
s_sea_succ(sataf_automaton self, uint32_t letter)
{
  sea_automaton sea = (sea_automaton)self;
  uint32_t    succ = exit_automaton_successor(sea->ea,sea->s,letter);

  succ = exit_automaton_decode_succ_state(succ);

  return s_create_from_exit_automaton(sea->ea,succ);
}

			/* --------------- */
static int
s_sea_is_final(sataf_automaton self)
{
  sea_automaton sea = (sea_automaton)self;

  return sea->ea->is_final[sea->s];
}

			/* --------------- */

static int
s_sea_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  sea_automaton sea1 = (sea_automaton)self;
  sea_automaton sea2 = (sea_automaton)other;

  return sea1->ea == sea2->ea 
    &&   sea1->s  == sea2->s;
}

			/* --------------- */

static uint32_t
s_sea_hashcode(sataf_automaton self)
{
  sea_automaton sea = (sea_automaton)self;

  return 11*(uint32_t)sea->ea+13*(uint32_t)sea->s;
}

			/* --------------- */

static sataf_automaton
s_create_from_exit_automaton(exit_automaton ea, uint32_t s0)
{
  sea_automaton result = (sea_automaton)
    sataf_automaton_create(sizeof(struct sea_automaton_st),&SEA_METHODS);

  ccl_pre( ea->nb_exit_states == 0 );

  result->ea = exit_automaton_add_reference(ea);
  result->s  = s0;

  return (sataf_automaton)result;
}

			/* --------------- */

static void *
s_allocate_automaton(uint32_t size)
{
  void *r;
  uint32_t szd2 = (size+1)>>1;
  ccl_pre( size != 0 ); ccl_pre( szd2 < MAX_SA_SIZE_DIV_2 );

  if( POOLS[szd2] == NULL )
    POOLS[szd2] = ccl_pool_create("SA-Pool",size,SA_POOL_PAGE_SIZE);

  r = ccl_pool_alloc(POOLS[szd2]);
  ccl_memzero(r,size);

  return r;
}

			/* --------------- */

static void 
s_delete_automaton(sataf_automaton a)
{
  uint32_t size = (a->methods->size+1)>>1;

  ccl_assert( POOLS[size] != NULL );
  ccl_pool_release(POOLS[size],a);
}
