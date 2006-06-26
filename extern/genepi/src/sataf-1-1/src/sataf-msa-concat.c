/* $Id: sataf-msa-concat.c,v 1.1 2006/03/09 16:34:36 point Exp $ */
#include <ccl-memory.h>
#include <ccl-array.h>
#include <ccl-assert.h>
#include "sataf-msa.h"

typedef struct concat_st {
  sataf_msa L1;
  sataf_msa L2;
} concat;

typedef CCL_ARRAY(concat) concat_array;

typedef struct concat_automaton_st {
  struct sataf_automaton_st super;
  concat_array operands;
} concat_automaton;

			/* --------------- */

static sataf_automaton
s_create_concat(sataf_msa a1, sataf_msa a2);

static void
s_concat_automaton_destroy(sataf_automaton self);

static uint32_t
s_concat_automaton_get_alphabet_size(sataf_automaton self);

# define s_concat_automaton_to_string NULL

static sataf_automaton 
s_concat_automaton_succ(sataf_automaton self, uint32_t letter);

static int 
s_concat_automaton_is_final(sataf_automaton self);

static int
s_concat_automaton_equals(sataf_automaton self, sataf_automaton other);

static uint32_t
s_concat_automaton_hashcode(sataf_automaton self);

# define s_concat_automaton_to_dot NULL

			/* --------------- */

static struct sataf_automaton_methods_st CONCAT_METHODS = {
  "SA-CONCAT-OP",				
  sizeof(struct concat_automaton_st),
  s_concat_automaton_destroy,
  s_concat_automaton_get_alphabet_size,
  s_concat_automaton_to_string, 
  s_concat_automaton_succ, 
  s_concat_automaton_is_final,
  s_concat_automaton_equals, 
  s_concat_automaton_hashcode,
  s_concat_automaton_to_dot
};

			/* --------------- */

static uint32_t
s_concat_hashcode(const concat *c);

static int
s_concat_compare(const concat *c1, const concat *c2);

static void
s_concat_add_in_array(concat *a, int *size, const concat *c);

			/* --------------- */

sataf_msa
sataf_msa_concat(sataf_msa a1, sataf_msa a2)
{
  sataf_msa R;

  if( sataf_msa_is_zero(a1) || sataf_msa_is_zero(a2) ) 
    R = sataf_msa_zero(sataf_msa_get_alphabet_size(a1));
  else 
    {
      sataf_automaton aut = s_create_concat(a1,a2);
      R = sataf_msa_compute(aut);
      sataf_automaton_del_reference(aut);
    }

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_concat(sataf_msa a1, sataf_msa a2)
{
  concat_automaton *R = (concat_automaton *)
    sataf_automaton_create(sizeof(concat_automaton),&CONCAT_METHODS);
  ccl_array_init_with_size(R->operands,1);
  R->operands.data->L1 = sataf_msa_add_reference(a1);
  R->operands.data->L2 = sataf_msa_add_reference(a2);

  return (sataf_automaton)R;
}

			/* --------------- */

static void
s_concat_automaton_destroy(sataf_automaton self)
{
  int i;
  concat_automaton *a = (concat_automaton *)self;

  for(i = 0; i < a->operands.size; i++)
    {
      ccl_zdelete(sataf_msa_del_reference,a->operands.data[i].L1);
      sataf_msa_del_reference(a->operands.data[i].L2);
    }
  ccl_array_delete(a->operands);
}

			/* --------------- */

static uint32_t
s_concat_automaton_get_alphabet_size(sataf_automaton self)
{
  concat_automaton *a = (concat_automaton *)self;

  return sataf_msa_get_alphabet_size(a->operands.data->L2);
}

			/* --------------- */

static sataf_automaton 
s_concat_automaton_succ(sataf_automaton self, uint32_t letter)
{
  concat s;
  int i, nb_c = 0;
  concat_automaton *a = (concat_automaton *)self;
  concat *c = a->operands.data;
  concat_array operands;
  sataf_automaton R;
  
  ccl_array_init_with_size(operands,2*a->operands.size);

  for(i = a->operands.size; i; i--, c++)
    {     
      if( c->L1 != NULL )
	{
	  s.L1 = sataf_msa_succ(c->L1,letter);
	  s.L2 = sataf_msa_add_reference(c->L2);

	  s_concat_add_in_array(operands.data,&nb_c,&s);

	  if( sataf_msa_is_final(c->L1) ) 
	    {
	      s.L1 = NULL;
	      s.L2 = sataf_msa_succ(c->L2,letter);
	      s_concat_add_in_array(operands.data,&nb_c,&s);
	    }
	}
      else
	{
	  s.L1 = NULL;
	  s.L2 = sataf_msa_succ(c->L2,letter);

	  s_concat_add_in_array(operands.data,&nb_c,&s);
	}
    }

  ccl_assert( nb_c  <= 2*a->operands.size );

  if( nb_c  ) 
    {
      concat_automaton *ca = (concat_automaton *)
	sataf_automaton_create(sizeof(concat_automaton),&CONCAT_METHODS);

      ccl_array_trim(operands,nb_c);
      ccl_assert( operands.size == nb_c );
      ca->operands = operands;
      R = (sataf_automaton)ca;
    }
  else
    {
      int asize = sataf_msa_get_alphabet_size(a->operands.data->L1);
      R = sataf_automaton_create_zero(asize);

      ccl_array_delete(operands);
    }


  return(R);
}

			/* --------------- */

static int 
s_concat_automaton_is_final(sataf_automaton self)
{
  concat_automaton *a = (concat_automaton *)self;
  int nb_c = a->operands.size;
  concat *c = a->operands.data;
    
  for(; nb_c--; c++)
    {
      if( c->L1 != NULL )
	{
	  if( sataf_msa_is_final(c->L1) && sataf_msa_is_final(c->L2) )
	    return 1;
	}
      else if( sataf_msa_is_final(c->L2) )
	{
	  return 1;
	}
    }

  return 0;
}

			/* --------------- */

static int
s_concat_automaton_equals(sataf_automaton self, sataf_automaton other)
{
  concat_automaton *a1 = (concat_automaton *)self;
  concat_automaton *a2 = (concat_automaton *)other;
  int nb_c = a1->operands.size;
  concat *c1 = a1->operands.data;
  concat *c2 = a2->operands.data;

  if( nb_c != a2->operands.size )
    return 0;

  while( nb_c-- )
    {
      if( c1->L1 != c2->L1 || c1->L2 != c2->L2 )
	return 0;
      c1++; c2++;
    }

  return 1;
}

			/* --------------- */

static uint32_t
s_concat_automaton_hashcode(sataf_automaton self)
{
  concat_automaton *a = (concat_automaton *)self;
  int nb_c = a->operands.size;
  uint32_t r = nb_c;
  concat *c = a->operands.data;

  while( nb_c-- )
    r = 19*r+717*s_concat_hashcode(c++);

  return r;
}

			/* --------------- */

static uint32_t
s_concat_hashcode(const concat *c)
{
  return 17*(uint32_t)(c->L1)+71*(uint32_t)(c->L2);
}

			/* --------------- */

static int
s_concat_compare(const concat *c1, const concat *c2)
{
  if( c1->L1 < c2->L1 ) return -1;
  else if( c1->L1 > c2->L1 ) return 1;
  else if ( c1->L2 < c2->L2 ) return -1;
  else if ( c1->L2 > c2->L2 ) return 1;
  
  return 0;
}

			/* --------------- */

static void
s_concat_add_in_array(concat *a, int *size, const concat *c)
{
  int i;
  int cmp = 0;
  int sz = *size;

  if( (c->L1 != NULL && sataf_msa_is_zero(c->L1)) || sataf_msa_is_zero(c->L2) )
    {
      ccl_zdelete(sataf_msa_del_reference,c->L1);
      sataf_msa_del_reference(c->L2);
      return;
    }

  for(i = 0; i < sz && (cmp=s_concat_compare(a+i,c)) < 0; i++)
    continue;

  if( i == sz )
    a[sz++] = *c;
  else if( cmp == 0 ) 
    {
      ccl_zdelete(sataf_msa_del_reference,c->L1);
      sataf_msa_del_reference(c->L2);
    }
  else
    {
      int k;

      for(k = sz; k > i; k--)
	a[k] = a[k-1];
      a[i] = *c;
      sz++;
    }

  *size = sz;
}
