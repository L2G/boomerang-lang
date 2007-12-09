/* $Id: sataf-msa-power.c,v 1.1 2006/03/09 16:34:36 point Exp $ */
#include <ccl-memory.h>
#include <ccl-array.h>
#include <ccl-assert.h>
#include "sataf-msa.h"

typedef struct power_st {
  sataf_msa L;
  int p;
} power;

typedef CCL_ARRAY(power) power_array;

typedef struct power_automaton_st {
  struct sataf_automaton_st super;
  power_array operands;
  sataf_msa A;
} power_automaton;

			/* --------------- */

static sataf_automaton 
s_create_power(sataf_msa a, int N);

static void
s_power_automaton_destroy(sataf_automaton self);

static uint32_t
s_power_automaton_get_alphabet_size(sataf_automaton self);

static char *
s_power_automaton_to_string(sataf_automaton self);

static sataf_automaton 
s_power_automaton_succ(sataf_automaton self, uint32_t letter);

static int 
s_power_automaton_is_final(sataf_automaton self);

static int
s_power_automaton_equals(sataf_automaton self, sataf_automaton other);

static uint32_t
s_power_automaton_hashcode(sataf_automaton self);

# define s_power_automaton_to_dot NULL

			/* --------------- */

static struct sataf_automaton_methods_st POWER_METHODS = {
  "SA-POWER-OP",				
  sizeof(struct power_automaton_st),
  s_power_automaton_destroy,
  s_power_automaton_get_alphabet_size,
  s_power_automaton_to_string, 
  s_power_automaton_succ, 
  s_power_automaton_is_final,
  s_power_automaton_equals, 
  s_power_automaton_hashcode,
  s_power_automaton_to_dot
};

			/* --------------- */

static uint32_t
s_power_hashcode(const power *c);

static int
s_power_compare(const power *c1, const power *c2);

static void
s_power_add_in_array(power *a, int *size, const power *c);

			/* --------------- */

sataf_msa
sataf_msa_power(sataf_msa a, int N)
{
  sataf_msa R;

  if( sataf_msa_is_zero_or_one(a) || N == 1 )
    R = sataf_msa_add_reference(a);
  else if( N == 0 ) 
    R = sataf_msa_epsilon(sataf_msa_get_alphabet_size(a));
  else if( N == 2 ) 
    R = sataf_msa_concat(a,a);
  else 
    {
      sataf_automaton aut = s_create_power(a,N);
      R = sataf_msa_compute(aut);
      sataf_automaton_del_reference(aut);
    }

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_power(sataf_msa a, int N)
{
  power_automaton *R = (power_automaton *)
    sataf_automaton_create(sizeof(power_automaton),&POWER_METHODS);

  ccl_assert( N >= 2 );

  if( sataf_msa_is_final(a) ) 
    {
      int i;

      ccl_array_init_with_size(R->operands,N);
      for(i = 0; i < N; i++)
	{
	  R->operands.data[i].L = sataf_msa_add_reference(a);
	  R->operands.data[i].p = i;
	}
    }
  else
    {
      ccl_array_init_with_size(R->operands,1);
      R->operands.data->L = sataf_msa_add_reference(a);
      R->operands.data->p = N-1;
    }      
  R->A = sataf_msa_add_reference(a);
      
  return (sataf_automaton)R;
}

			/* --------------- */

static void
s_power_automaton_destroy(sataf_automaton self)
{
  int i;
  power_automaton *a = (power_automaton *)self;

  for(i = 0; i < a->operands.size; i++)
    sataf_msa_del_reference(a->operands.data[i].L);
  ccl_array_delete(a->operands);
  sataf_msa_del_reference(a->A);
}

			/* --------------- */

static uint32_t
s_power_automaton_get_alphabet_size(sataf_automaton self)
{
  power_automaton *a = (power_automaton *)self;

  return sataf_msa_get_alphabet_size(a->A);
}

			/* --------------- */

static char *
s_power_automaton_to_string(sataf_automaton self)
{
  int i;
  power_automaton *a = (power_automaton *)self;
  char *result = NULL;

  for(i = 0; i < a->operands.size; i++)
    {
      ccl_string_format_append(&result,
			       "(%p,%d) ",a->operands.data[i].L,
			       a->operands.data[i].p);
    }
  return result;
}

			/* --------------- */

static sataf_automaton 
s_power_automaton_succ(sataf_automaton self, uint32_t letter)
{
  power s;
  int i, nb_c = 0;
  power_automaton *a = (power_automaton *)self;
  power *c = a->operands.data;
  power_array operands;
  sataf_automaton R;
  
  ccl_array_init(operands);

  for(i = a->operands.size; i; i--, c++)
    {     
      s.L = sataf_msa_succ(c->L,letter);
      s.p = c->p;

      ccl_array_ensure_size_plus_one(operands);
      s_power_add_in_array(operands.data,&nb_c,&s);

      if( c->p != 0 && sataf_msa_is_final(c->L) ) 
	{	  
	  if( sataf_msa_is_final(a->A) )
	    {
	      int j;

	      for(j = 0; j < c->p-1; j++)
		{
		  s.L = sataf_msa_succ(a->A,letter);
		  s.p = j;
		  ccl_array_ensure_size_plus_one(operands);
		  s_power_add_in_array(operands.data,&nb_c,&s);
		}
	    }

	  s.L = sataf_msa_succ(a->A,letter);
	  s.p = c->p-1;
	  ccl_array_ensure_size_plus_one(operands);
	  s_power_add_in_array(operands.data,&nb_c,&s);
	}
    }

  ccl_assert( 0 < nb_c && nb_c  <= 2*a->operands.size );

    {
      power_automaton *pa = (power_automaton *)
	sataf_automaton_create(sizeof(power_automaton),&POWER_METHODS);

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
s_power_automaton_is_final(sataf_automaton self)
{
  power_automaton *a = (power_automaton *)self;
  int nb_c = a->operands.size;
  power *c = a->operands.data;
    
  for(; nb_c--; c++)
    {
      if( sataf_msa_is_final(c->L) && c->p == 0 )
	return 1;
    }

  return 0;
}

			/* --------------- */

static int
s_power_automaton_equals(sataf_automaton self, sataf_automaton other)
{
  power_automaton *a1 = (power_automaton *)self;
  power_automaton *a2 = (power_automaton *)other;
  int nb_c = a1->operands.size;
  power *c1 = a1->operands.data;
  power *c2 = a2->operands.data;

  if( nb_c != a2->operands.size )
    return 0;

  if( a1->A != a2->A )
    return 0;

  while( nb_c-- )
    {
      if( c1->L != c2->L || c1->p != c2->p )
	return 0;
      c1++; c2++;
    }

  return 1;
}

			/* --------------- */

static uint32_t
s_power_automaton_hashcode(sataf_automaton self)
{
  power_automaton *a = (power_automaton *)self;
  int nb_c = a->operands.size;
  uint32_t r = nb_c;
  power *c = a->operands.data;

  while( nb_c-- )
    {
      r = 19*r+717*(uint32_t)c->L+31415*c->p;
      c++;
    }

  return r;
}

			/* --------------- */

static int
s_power_compare(const power *c1, const power *c2)
{
  if( c1->L < c2->L ) return -1;
  else if( c1->L > c2->L ) return 1;
  else return c1->p-c2->p;
}

			/* --------------- */

static void
s_power_add_in_array(power *a, int *size, const power *c)
{
  int i;
  int cmp = 0;
  int sz = *size;

  for(i = 0; i < sz && (cmp=s_power_compare(a+i,c)) < 0; i++)
    continue;

  if( i == sz )
    a[sz++] = *c;
  else if( cmp == 0 ) 
    sataf_msa_del_reference(c->L);
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
