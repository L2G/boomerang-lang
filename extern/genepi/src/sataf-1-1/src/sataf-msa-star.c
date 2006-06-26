/* $Id: sataf-msa-star.c,v 1.1 2006/03/09 16:34:36 point Exp $ */
#include <ccl-assert.h>
#include <ccl-array.h>
#include "sataf-automaton.h"
#include "sataf-msa.h"

typedef CCL_ARRAY(sataf_msa) msa_array;

typedef struct star_automaton_st {
  struct sataf_automaton_st super;
  msa_array operands;
  sataf_msa A;
} star_automaton;

			/* --------------- */

static sataf_automaton 
s_create_star(sataf_msa a);

static void
s_star_automaton_destroy(sataf_automaton self);

static uint32_t
s_star_automaton_get_alphabet_size(sataf_automaton self);

static char *
s_star_automaton_to_string(sataf_automaton self);

static sataf_automaton 
s_star_automaton_succ(sataf_automaton self, uint32_t letter);

static int 
s_star_automaton_is_final(sataf_automaton self);

static int
s_star_automaton_equals(sataf_automaton self, sataf_automaton other);

static uint32_t
s_star_automaton_hashcode(sataf_automaton self);

# define s_star_automaton_to_dot NULL

			/* --------------- */

static struct sataf_automaton_methods_st STAR_METHODS = {
  "SA-STAR-OP",				
  sizeof(struct star_automaton_st),
  s_star_automaton_destroy,
  s_star_automaton_get_alphabet_size,
  s_star_automaton_to_string, 
  s_star_automaton_succ, 
  s_star_automaton_is_final,
  s_star_automaton_equals, 
  s_star_automaton_hashcode,
  s_star_automaton_to_dot
};

			/* --------------- */

static void
s_star_add_in_array(sataf_msa *a, int *size, sataf_msa c);

			/* --------------- */

sataf_msa
sataf_msa_star(sataf_msa a)
{
  sataf_msa R;

  if( sataf_msa_is_zero_or_one(a) )
    R = sataf_msa_add_reference(a);
  else 
    {
      sataf_automaton aut = s_create_star(a);
      R = sataf_msa_compute(aut);
      sataf_automaton_del_reference(aut);
    }

  return R;
}

			/* --------------- */

static sataf_automaton 
s_create_star(sataf_msa a)
{
  star_automaton *R = (star_automaton *)
    sataf_automaton_create(sizeof(star_automaton),&STAR_METHODS);

  ccl_array_init_with_size(R->operands,1);
  R->operands.data[0] = sataf_msa_add_reference(a);
  R->A = sataf_msa_add_reference(a);

  return (sataf_automaton)R;
}

			/* --------------- */

static void
s_star_automaton_destroy(sataf_automaton self)
{
  int i;
  star_automaton *a = (star_automaton *)self;

  for(i = 0; i < a->operands.size; i++)
    sataf_msa_del_reference(a->operands.data[i]);
  ccl_array_delete(a->operands);
  sataf_msa_del_reference(a->A);
}

			/* --------------- */

static uint32_t
s_star_automaton_get_alphabet_size(sataf_automaton self)
{
  star_automaton *a = (star_automaton *)self;

  return sataf_msa_get_alphabet_size(a->A);
}

			/* --------------- */

static char *
s_star_automaton_to_string(sataf_automaton self)
{
  int i;
  star_automaton *a = (star_automaton *)self;
  char *result = NULL;

  for(i = 0; i < a->operands.size; i++)
    ccl_string_format_append(&result,"%p ",a->operands.data[i]);

  return result;
}

			/* --------------- */

static sataf_automaton 
s_star_automaton_succ(sataf_automaton self, uint32_t letter)
{
  sataf_msa s;
  int i, nb_c = 0;
  star_automaton *a = (star_automaton *)self;
  sataf_msa *c = a->operands.data;
  msa_array operands;
  sataf_automaton R;
  
  ccl_array_init_with_size(operands,2*a->operands.size);

  for(i = a->operands.size; i; i--, c++)
    {     
      s = sataf_msa_succ(*c,letter);
      s_star_add_in_array(operands.data,&nb_c,s);

      if( sataf_msa_is_final(*c) ) 
	{	  
	  s = sataf_msa_succ(a->A,letter);
	  s_star_add_in_array(operands.data,&nb_c,s);
	}
    }

  ccl_assert( 0 < nb_c && nb_c  <= 2*a->operands.size );

  {
    star_automaton *pa = (star_automaton *)
      sataf_automaton_create(sizeof(star_automaton),&STAR_METHODS);
    
    ccl_array_trim(operands,nb_c);
    ccl_assert( operands.size == nb_c );
    pa->operands = operands;
    pa->A = sataf_msa_add_reference(a->A);
    R = (sataf_automaton)pa;
  }


  return(R);
}

			/* --------------- */

static int 
s_star_automaton_is_final(sataf_automaton self)
{
  star_automaton *a = (star_automaton *)self;
  int nb_c = a->operands.size;
  sataf_msa *c = a->operands.data;
    
  for(; nb_c--; c++)
    {
      if( sataf_msa_is_final(*c) || *c == a->A )
	return 1;
    }

  return 0;
}

			/* --------------- */

static int
s_star_automaton_equals(sataf_automaton self, sataf_automaton other)
{
  star_automaton *a1 = (star_automaton *)self;
  star_automaton *a2 = (star_automaton *)other;
  int nb_c = a1->operands.size;
  sataf_msa *c1 = a1->operands.data;
  sataf_msa *c2 = a2->operands.data;

  if( nb_c != a2->operands.size )
    return 0;

  if( a1->A != a2->A )
    return 0;

  while( nb_c-- )
    {
      if( *c1 != *c2 )
	return 0;
      c1++; c2++;
    }

  return 1;
}

			/* --------------- */

static uint32_t
s_star_automaton_hashcode(sataf_automaton self)
{
  star_automaton *a = (star_automaton *)self;
  int nb_c = a->operands.size;
  uint32_t r = nb_c;
  sataf_msa *c = a->operands.data;

  while( nb_c-- )
    {
      r = 19*r+717*(uint32_t)(*c);
      c++;
    }

  return r;
}

			/* --------------- */

static void
s_star_add_in_array(sataf_msa *a, int *size, sataf_msa c)
{
  int i;
  int sz = *size;

  for(i = 0; i < sz && a[i] < c ; i++)
    continue;

  if( i == sz )
    a[sz++] = c;
  else if( a[i] == c ) 
    sataf_msa_del_reference(c);
  else
    {
      int k;

      for(k = sz; k > i; k--)
	a[k] = a[k-1];
      a[i] = c;
      sz++;
    }

  *size = sz;
}
