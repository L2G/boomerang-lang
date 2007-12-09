/* $Id: add-variable-automaton.c,v 1.2 2006/01/04 08:27:46 point Exp $ */
#include "add-variable-automaton.h"

typedef struct addvar_st {
  struct sataf_automaton_st super;
  uint32_t  i;
  uint32_t  n;
  sataf_msa msa;
} *addvar;

			/* --------------- */

static void
s_addvar_destroy(sataf_automaton self);

static uint32_t
s_addvar_get_alphabet_size(sataf_automaton self);

#define s_addvar_to_string NULL

static sataf_automaton
s_addvar_succ(sataf_automaton self, uint32_t letter);

static int
s_addvar_is_final(sataf_automaton self);

static int
s_addvar_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_addvar_hashcode(sataf_automaton self);

# define s_addvar_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st ADDVAR_METHODS = {
  "SA-ADDVAR",
  sizeof(struct addvar_st),
  s_addvar_destroy,
  s_addvar_get_alphabet_size,
  s_addvar_to_string,
  s_addvar_succ,
  s_addvar_is_final,
  s_addvar_is_equal_to,
  s_addvar_hashcode,
  s_addvar_to_dot
};

			/* --------------- */

static sataf_automaton 
s_new_addvar(sataf_msa msa, uint32_t i, uint32_t n)
{
  addvar R = (addvar)sataf_automaton_create(sizeof(struct addvar_st),
					    &ADDVAR_METHODS);
      
  R->msa = sataf_msa_add_reference(msa);
  R->i   = i;
  R->n   = n;

  return (sataf_automaton)R;
}

			/* --------------- */


sataf_automaton
add_variable_automaton_create(sataf_msa msa, uint32_t i, uint32_t n)
{
  sataf_automaton R;
  static uint8_t  is_final[] = { 0, 1 };
  static uint32_t succ[] = { 1, 1, 1, 1 };

  if( sataf_msa_is_zero(msa) )
    R = sataf_msa_to_automaton(msa);
  else if( sataf_msa_is_one(msa) )
    R = sataf_automaton_create_from_arrays(2,2,0,is_final,succ);
  else
    {
      R = s_new_addvar(msa,i,n);
    }

  return R;
}

			/* --------------- */

static void
s_addvar_destroy(sataf_automaton self)
{
  addvar a = (addvar)self;

  sataf_msa_del_reference(a->msa);
}

			/* --------------- */

static uint32_t
s_addvar_get_alphabet_size(sataf_automaton self)
{
  addvar a = (addvar)self;

  return sataf_msa_get_alphabet_size(a->msa);
}

			/* --------------- */

static sataf_automaton
s_addvar_succ(sataf_automaton self, uint32_t letter)
{
  sataf_automaton R;
  addvar          a = (addvar)self;

  if( a->i == 0 ) R = s_new_addvar(a->msa,a->n,a->n);
  else            
    {
      sataf_msa succ = sataf_msa_succ(a->msa,letter);
      R = (sataf_automaton)s_new_addvar(succ,a->i-1,a->n);
      sataf_msa_del_reference(succ);
    }

  return R;
}

			/* --------------- */

static int
s_addvar_is_final(sataf_automaton self)
{
  addvar a = (addvar)self;

  return sataf_msa_is_final(a->msa);
}

			/* --------------- */

static int
s_addvar_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  addvar a1 = (addvar)self;
  addvar a2 = (addvar)other;

  return a1->i == a2->i && a1->n == a2->n && a1->msa == a2->msa;
}

			/* --------------- */

static uint32_t
s_addvar_hashcode(sataf_automaton self)
{
  addvar a = (addvar)self;

  return 13*a->i+37*a->n+91*((uint32_t)a->msa);
}
