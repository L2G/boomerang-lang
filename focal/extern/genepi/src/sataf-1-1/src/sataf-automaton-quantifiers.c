/* $Id: sataf-automaton-quantifiers.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <ccl-assert.h>
#include "sataf.h"

# define DUP(_a) sataf_automaton_add_reference(_a)
# define DEL(_a) sataf_automaton_del_reference(_a)

			/* --------------- */

sataf_automaton
sataf_automaton_create_forall(sataf_automaton a)
{
  uint32_t         i;
  uint32_t       len = sataf_automaton_get_alphabet_size(a);
  sataf_automaton tmp2;
  sataf_automaton    R = sataf_automaton_succ(a,0);
  sataf_automaton tmp1;

  for(i = 1; i < len; i++)
    {
      tmp1 = sataf_automaton_succ(a,i);
      tmp2 = sataf_automaton_create_and(R,tmp1);
      DEL(tmp1);
      DEL(R);
      R = tmp2;
    }

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_exists(sataf_automaton a)
{
  uint32_t         i;
  uint32_t       len = sataf_automaton_get_alphabet_size(a);
  sataf_automaton tmp2;
  sataf_automaton    R = sataf_automaton_succ(a,0);
  sataf_automaton tmp1;

  for(i = 1; i < len; i++)
    {
      tmp1 = sataf_automaton_succ(a,i);
      tmp2 = sataf_automaton_create_or(R,tmp1);
      DEL(tmp1);
      DEL(R);
      R = tmp2;
    }

  return R;
}

			/* --------------- */

