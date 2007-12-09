/* $Id: apply-automaton.c,v 1.5 2006/01/30 13:17:21 point Exp $ */
/**
 * xRx' is encoded with this ordering : x'_1x_1 x'_2x_2 ... x'_nx_n
 * ----------------------------------------------------------------------
 * apply(R,A) = { x' | \exists x\in A s.t. x R x' }
 * applyinv(R,A) = { x | \exists x'\in A s.t. x R x' }
 * ----------------------------------------------------------------------
 */
#include <ccl-list.h>
#include <shared-automaton.h>
#include "apply-automaton.h"

#undef APPLY_SHOW_WIDTH

typedef struct apply_couple_st {
  sataf_msa R;
  sataf_msa A;  
} apply_couple;

			/* --------------- */

typedef struct apply_st {
  struct sataf_automaton_st super;
  apply_couple *C;
  int width;
} apply;

			/* --------------- */

typedef struct apply_methods_st {
  struct sataf_automaton_methods_st super;
  void (*couple_succ)(apply_couple *c, uint32_t a, uint32_t b, 
		      apply_couple *res);
} apply_methods;

			/* --------------- */

static void
s_apply_destroy(sataf_automaton self)
{
  apply *app = (apply *)self;
  int w = app->width;
  apply_couple *c = app->C;

  while( w-- )
    {
      sataf_msa_del_reference(c->R);
      sataf_msa_del_reference(c->A);
      c++;
    }
  ccl_zdelete(ccl_delete,app->C);
}

			/* --------------- */

static uint32_t
s_apply_get_alphabet_size(sataf_automaton self)
{
  return 2;
}

			/* --------------- */

#if 1
static char *
s_apply_to_string(sataf_automaton self)
{
  int i;
  char *result = ccl_string_dup("{ ");
  apply *app = (apply *)self;

  for(i = 0; i < app->width; i++)
    {
      const char *fmt = "(%p,%p), ";
      if( i == app->width-1 ) 
	fmt = "(%p,\\n%p)";
      ccl_string_format_append(&result,fmt,app->C[i].R,app->C[i].A);
    }
  ccl_string_format_append(&result," }");

  return result;
}
#else
# define s_apply_to_string NULL
#endif

			/* --------------- */

static sataf_automaton
s_apply_create(int width, apply_couple *C, sataf_automaton_methods methods);

			/* --------------- */

static void
s_couple_succ(apply_couple *c, uint32_t a, uint32_t r, apply_couple *res)
{
  sataf_msa Rr = sataf_msa_succ(c->R,r);
  sataf_msa Rra = sataf_msa_succ(Rr,a);
  sataf_msa Aa = sataf_msa_succ(c->A,a);

  res->R = Rra;
  res->A = Aa;

  sataf_msa_del_reference(Rr);
}

			/* --------------- */

static void
s_couple_inv_succ(apply_couple *c, uint32_t a, uint32_t r, apply_couple *res)
{
  sataf_msa Ra = sataf_msa_succ(c->R,a);
  sataf_msa Rar = sataf_msa_succ(Ra,r);
  sataf_msa Aa = sataf_msa_succ(c->A,a);

  res->R = Rar;
  res->A = Aa;

  sataf_msa_del_reference(Ra);
}

			/* --------------- */

#define C_LT(c1,c2) \
  (((c1)->R < (c2)->R) || ((c1)->R == (c2)->R && (c1)->A < (c2)->A))

#define C_EQ(c1,c2) ((c1)->R == (c2)->R && (c1)->A == (c2)->A)

static int cmp(const void *p1, const void *p2)
{
  const apply_couple *c1 = p1;
  const apply_couple *c2 = p2;

  if( C_LT(c1,c2) ) return -1;
  if( C_EQ(c1,c2) ) return 0;

  return 1;
}

			/* --------------- */

static sataf_automaton
s_apply_succ2(sataf_automaton self, uint32_t letter)
{
  int a;
  int i, j;
  sataf_automaton result;
  apply *app = (apply *)self;
  int w = app->width;
  apply_couple *c = app->C;
  apply_couple *newc;
  int succ_couples_size = 2*w;
  apply_couple *succ_couples = ccl_new_array(apply_couple,succ_couples_size);
  int succ_width = 0;
  int found;

  for(; w--; c++)
    {
      for(a = 0; a < 2; a++)
	{
	  for(i = 0; i < succ_width-1; i++)
	    {
	      ccl_assert( C_LT(&(succ_couples[i]),&(succ_couples[i+1])) );
	    }

	  newc = succ_couples+succ_width;

	  ((apply_methods *)self->methods)->couple_succ(c,a,letter,newc);

	  if( sataf_msa_is_zero(newc->R) || sataf_msa_is_zero(newc->A) )
	    {
	      sataf_msa_del_reference(newc->R);
	      sataf_msa_del_reference(newc->A);
	      continue;
	    }
	  
	  for(found = 0, i = 0; i < succ_width; i++)
	    {
	      if( C_LT(newc,succ_couples+i) )
		{
		  apply_couple aux = *newc;
		  for(j = succ_width; j > i ; j--)
		    {
		      succ_couples[j] = succ_couples[j-1];
		    }
		  ccl_assert( j == i );
		  succ_couples[j] = aux;
		  succ_width++;
		  found = 1;
		  break;
		}
	      else if( C_EQ(newc,succ_couples+i) )
		{
		  sataf_msa_del_reference(newc->R);
		  sataf_msa_del_reference(newc->A);
		  found = 1;
		  break;
		}
	    }

	  if( ! found )
	    succ_width++;
	}
    }

  {
    int k;
    for(k = 0; k < succ_width-1; k++)
      {
	ccl_assert( C_LT(&(succ_couples[k]),&(succ_couples[k+1])) );
      }
  }

  ccl_assert( succ_width <= succ_couples_size );
  

  if( succ_width == 0 )
    {
      result = sataf_automaton_create_zero(2);
      ccl_delete(succ_couples);
    }
  else 
    {
      if( succ_width < succ_couples_size )
	succ_couples = (apply_couple *)
	  ccl_realloc(succ_couples,sizeof(apply_couple)*succ_width);
      result = s_apply_create(succ_width,succ_couples,self->methods);
    }
  
  return result;
}

			/* --------------- */

static sataf_automaton
s_apply_succ(sataf_automaton self, uint32_t letter)
{
  int a;
  int i, j;
  sataf_automaton result;
  apply *app = (apply *)self;
  int w = app->width;
  apply_couple *c = app->C;
  apply_couple *newc;
  int succ_couples_size = 2*w;
  apply_couple *succ_couples;
  int succ_width = 0;
  int found;
  ccl_list couples;

  if( w == 0 )
    return sataf_automaton_add_reference(self);

  couples = ccl_list_create();
  for(; w--; c++)
    {
      for(a = 0; a < 2; a++)
	{
	  newc = ccl_new(apply_couple);

	  ((apply_methods *)self->methods)->couple_succ(c,a,letter,newc);

	  if( sataf_msa_is_zero(newc->R) || sataf_msa_is_zero(newc->A) ||
	      ccl_list_get_index(couples,newc,cmp) >= 0 )
	    {
	      sataf_msa_del_reference(newc->R);
	      sataf_msa_del_reference(newc->A);
	      ccl_delete(newc);
	      continue;
	    }	  

	    {
	      ccl_pair p;
	      int found = 0;

	      for(p = FIRST(couples); p && ! found ; p = CDR(p))
		{
		  apply_couple *ac = CAR(p);

		  if( sataf_msa_is_included_in(ac->R,newc->R) &&
		      sataf_msa_is_included_in(ac->A,newc->A) )
		    {
		      found = 1;
		      CAR(p) = newc;
		      newc = ac;
		    }
		  else if( sataf_msa_is_included_in(newc->R,ac->R) &&
			   sataf_msa_is_included_in(newc->A,ac->A) )
		    {
		      found = 1;
		    }

		}

	      if( found )
		{
		  sataf_msa_del_reference(newc->R);
		  sataf_msa_del_reference(newc->A);
		  ccl_delete(newc);
		}
	      else
		ccl_list_add(couples,newc);
	    }
	}
    }
  ccl_list_sort(couples,cmp);

  if( (succ_width = ccl_list_get_size(couples)) == 0 )
    result = sataf_automaton_create_zero(2);
  else
    {
      ccl_pair p;
      succ_couples = ccl_new_array(apply_couple,succ_width);
      for(i = 0, p = FIRST(couples); p; p = CDR(p), i++)
	{
	  apply_couple *ac = CAR(p);
	  succ_couples[i] = *ac;
	  if( i> 0 )
	    ccl_assert( C_LT(&(succ_couples[i-1]),&(succ_couples[i])) );
	  ccl_delete(ac);
	}

      result = s_apply_create(succ_width,succ_couples,self->methods);
    }
  ccl_list_delete(couples);
  
  return result;
}

			/* --------------- */

static int 
s_apply_is_final(sataf_automaton self)
{
  apply *app = (apply *)self;
  int w = app->width;
  apply_couple *c = app->C;

  if( w == 0 )
    return 0;

  for(; w--; c++)
    if( sataf_msa_is_final(c->R) && sataf_msa_is_final(c->A) )
      return 1;

  return 0;
}

			/* --------------- */

static int
s_apply_equals(sataf_automaton self, sataf_automaton other)
{
  apply *app1 = (apply *)self;
  apply *app2 = (apply *)other;

  if( app1->width != app2->width ) 
    return 0;
  
  return app1->width == 0 ||
    memcmp(app1->C,app2->C,sizeof(apply_couple)*app1->width) == 0;
}

			/* --------------- */

static const int PRIME_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361,
   2729, 5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641,
   2807303, 5614657, 11229331
};

# define PRIME_TABLE_SIZE (sizeof(PRIME_TABLE)/PRIME_TABLE[0])

static uint32_t
s_apply_hashcode(sataf_automaton self)
{
  apply *app = (apply *)self;
  int w = app->width;
  uint32_t result = 175447*w;
  apply_couple *c = app->C;
  
  for(; w--; c++)
    result = 175447*result+
      PRIME_TABLE[w%PRIME_TABLE_SIZE]*((uint32_t)c->R)+
      PRIME_TABLE[(w+1)%PRIME_TABLE_SIZE]*((uint32_t)c->A);

  return result;
}

#define s_apply_to_dot NULL

			/* --------------- */

static const apply_methods APPLY_METHODS = {
  {
    "SA-APPLY",
    sizeof(struct apply_st),
    s_apply_destroy,
    s_apply_get_alphabet_size,
    s_apply_to_string,
    s_apply_succ,
    s_apply_is_final,
    s_apply_equals,
    s_apply_hashcode,
    s_apply_to_dot
  },
  s_couple_succ
};

			/* --------------- */

static const apply_methods APPLYINV_METHODS = {
  {
    "SA-APPLYINV",
    sizeof(struct apply_st),
    s_apply_destroy,
    s_apply_get_alphabet_size,
    s_apply_to_string,
    s_apply_succ,
    s_apply_is_final,
    s_apply_equals,
    s_apply_hashcode,
    s_apply_to_dot
  },
  s_couple_inv_succ
};

			/* --------------- */

static sataf_automaton
s_apply_create(int width, apply_couple *C, sataf_automaton_methods methods)
{
  apply *res = (apply *)
    sataf_automaton_create(sizeof(apply),methods);
  
  res->width = width;
  res->C = C;
#ifdef APPLY_SHOW_WIDTH
  if( width > 2 )
    ccl_warning("width=%d\n",width);
#endif

  return (sataf_automaton)res;
}

			/* --------------- */

sataf_automaton
apply_automaton_create(sataf_msa R, sataf_msa A)
{
  apply *res;
  apply_couple *C;

  if( sataf_msa_is_zero(A) || sataf_msa_is_zero(R) )
    return sataf_automaton_create_zero(2);

  C = ccl_new(apply_couple);
  C->R = sataf_msa_add_reference(R);
  C->A = sataf_msa_add_reference(A);

  return s_apply_create(1,C,(sataf_automaton_methods)&APPLY_METHODS);
}

			/* --------------- */

sataf_automaton
apply_automaton_create_inverse(sataf_msa R, sataf_msa A)
{
  apply *res;
  apply_couple *C;

  if( sataf_msa_is_zero(A) || sataf_msa_is_zero(R) )
    return sataf_automaton_create_zero(2);

  C = ccl_new(apply_couple);
  C->R = sataf_msa_add_reference(R);
  C->A = sataf_msa_add_reference(A);

  return s_apply_create(1,C,(sataf_automaton_methods)&APPLYINV_METHODS);
}
