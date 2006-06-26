/* $Id: linear-constraint-automaton.c,v 1.2 2006/01/04 08:27:23 point Exp $ */
#include "p-prestaf-formula.h"
#include "linear-constraint-automaton.h"

typedef struct lca_st {
  struct sataf_automaton_st super;
  int                        type;
  prestaf_linear_term         axb;
  prestaf_factor           factor;
  int                       value;
} *lca;

			/* --------------- */

static void
s_lca_destroy(sataf_automaton self);

static uint32_t
s_lca_get_alphabet_size(sataf_automaton self);

#define s_lca_to_string NULL

static sataf_automaton
s_lca_succ(sataf_automaton self, uint32_t letter);

static int
s_lca_is_final(sataf_automaton self);

static int
s_lca_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_lca_hashcode(sataf_automaton self);

# define s_lca_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st LCA_METHODS = {
  "SA-LCA",
  sizeof(struct lca_st),
  s_lca_destroy,
  s_lca_get_alphabet_size,
  s_lca_to_string,
  s_lca_succ,
  s_lca_is_final,
  s_lca_is_equal_to,
  s_lca_hashcode,
  s_lca_to_dot
};

			/* --------------- */

static sataf_automaton
s_new_lca(int type, prestaf_linear_term axb, prestaf_factor factor, int value)
{
  lca R = (lca)sataf_automaton_create(sizeof(struct lca_st),&LCA_METHODS);
  
  R->type   = type;
  R->axb    = prestaf_linear_term_add_reference(axb);
  R->factor = factor;
  R->value  = value;

  return (sataf_automaton)R;
}

			/* --------------- */

sataf_automaton
linear_constraint_automaton_create(int                 type,
				   prestaf_linear_term  axb)
{
  return s_new_lca(type,axb,axb->factors,axb->b);
}

			/* --------------- */

static void
s_lca_destroy(sataf_automaton self)
{
  lca a = (lca)self;

  prestaf_linear_term_del_reference(a->axb);
}

			/* --------------- */

static uint32_t
s_lca_get_alphabet_size(sataf_automaton self)
{
  return 2;
}

			/* --------------- */

static sataf_automaton
s_lca_succ(sataf_automaton self, uint32_t letter)
{
  lca a = (lca)self;
  int32_t value = a->value;
  prestaf_factor factor = a->factor;
  sataf_automaton R;

  if( letter == 1 )
    value += factor->a;

  if( factor->next != NULL )
    R = s_new_lca(a->type,a->axb,factor->next,value);
  else if( (value % 2) == 0 )
    R = s_new_lca(a->type,a->axb,a->axb->factors,value/2);
  else if( a->type == LCA_EQ )
    R = sataf_automaton_create_zero(2);
  else 
    R = s_new_lca(LCA_GEQ,a->axb,a->axb->factors,(value-1)/2);

  return R;
}

			/* --------------- */

static int
s_lca_is_final(sataf_automaton self)
{
  int R;
  lca a = (lca)self;

  if( a->type == LCA_EQ )       R = (a->value == 0);
  else if( a->type == LCA_GT )  R = (a->value > 0);
  else                          R = (a->value >= 0); 

  return R;
}

			/* --------------- */

static int
s_lca_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  lca a1 = (lca)self;
  lca a2 = (lca)other;

  return a1->type   == a2->type
    &&   a1->axb    == a2->axb
    &&   a1->factor == a2->factor
    &&   a1->value  == a2->value;
}
			/* --------------- */


static uint32_t
s_lca_hashcode(sataf_automaton self)
{
  lca a = (lca)self;

  return 11*a->type
    +    13*(uint32_t)a->axb
    +    71*(uint32_t)a->factor
    +    97*a->value;
}

