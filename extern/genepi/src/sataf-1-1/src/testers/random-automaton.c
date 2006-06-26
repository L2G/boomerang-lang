/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.2 $
 * @date    : $Date: 2006/01/04 08:16:34 $
 */
#include "rnd.h"
#include "exit-automaton.h"
#include "random-automaton.h"



typedef struct rnd_automaton_st {
  struct sataf_automaton_st super;
  exit_automaton               ea;
  uint32_t                    s;
} RND_AUTOMATON;


			/* --------------- */

static void 
s_rnd_destroy(sataf_automaton self);

static uint32_t
s_rnd_get_alphabet_size(sataf_automaton self);

#define s_rnd_to_string NULL

static sataf_automaton
s_rnd_succ(sataf_automaton self, uint32_t letter);

static int
s_rnd_is_final(sataf_automaton self);

static int
s_rnd_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_rnd_hashcode(sataf_automaton self);

# define s_rnd_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st RND_METHODS = {
  "SA-RND",
  sizeof(RND_AUTOMATON),
  s_rnd_destroy,
  s_rnd_get_alphabet_size,
  s_rnd_to_string,
  s_rnd_succ,
  s_rnd_is_final,
  s_rnd_is_equal_to,
  s_rnd_hashcode,
  s_rnd_to_dot
};

			/* --------------- */

static RND_AUTOMATON *
s_create_rnd_automaton(exit_automaton ea, uint32_t s);

static exit_automaton
s_generate_random_ea(uint32_t size, uint32_t alphabet_size);

			/* --------------- */

sataf_automaton
random_automaton_create(uint32_t size, uint32_t alphabet_size)
{
  RND_AUTOMATON *R = 
    s_create_rnd_automaton(s_generate_random_ea(size,alphabet_size),0);

  return (sataf_automaton)R;
}

			/* --------------- */

exit_automaton
random_exit_automaton_create(uint32_t size, uint32_t alphabet_size)
{
  return s_generate_random_ea(size,alphabet_size);
}

			/* --------------- */

static void 
s_rnd_destroy(sataf_automaton self)
{
  RND_AUTOMATON *ra = (RND_AUTOMATON *)self;

  exit_automaton_del_reference(ra->ea);
}

			/* --------------- */

static uint32_t
s_rnd_get_alphabet_size(sataf_automaton self)
{
  RND_AUTOMATON *ra = (RND_AUTOMATON *)self;

  return ra->ea->alphabet_size;
}

			/* --------------- */

static sataf_automaton
s_rnd_succ(sataf_automaton self, uint32_t letter)
{
  RND_AUTOMATON *ra = (RND_AUTOMATON *)self;
  uint32_t   succ = 
    exit_automaton_decode_succ_state(exit_automaton_successor(ra->ea,
							      ra->s,letter));

  ra->ea = exit_automaton_add_reference(ra->ea);
  return (sataf_automaton)s_create_rnd_automaton(ra->ea,succ);
}

			/* --------------- */

static int
s_rnd_is_final(sataf_automaton self)
{
  RND_AUTOMATON *ra = (RND_AUTOMATON *)self;

  return ra->ea->is_final[ra->s];
}

			/* --------------- */

static int
s_rnd_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  RND_AUTOMATON *ra       = (RND_AUTOMATON *)self;
  RND_AUTOMATON *ra_other = (RND_AUTOMATON *)other;

  return exit_automaton_equals(ra->ea,ra_other->ea)
    &&   ra->s == ra_other->s;
}

			/* --------------- */

static uint32_t
s_rnd_hashcode(sataf_automaton self)
{
  RND_AUTOMATON *ra = (RND_AUTOMATON *)self;

  return exit_automaton_hashcode(ra->ea)+19*ra->s;
}

			/* --------------- */

static RND_AUTOMATON *
s_create_rnd_automaton(exit_automaton ea, uint32_t s)
{
  RND_AUTOMATON *result = (RND_AUTOMATON *)
    sataf_automaton_create(sizeof(RND_AUTOMATON),&RND_METHODS);

  result->ea = ea;
  result->s  = s;

  return result;
}

			/* --------------- */

static void
s_randomize_array(uint32_t *a, uint32_t len)
{
  int i;
  uint32_t t;

  if( len == 0 )
    return;
  i = rnd_modulo(len);
  t    = a[i];
  a[i] = a[0];
  a[0] = t;
  s_randomize_array(a+1,len-1);
}

			/* --------------- */

static uint32_t *
s_rnd_path(uint32_t size)
{
  uint32_t  i;
  uint32_t *R = ccl_new_array(uint32_t,size);
  uint32_t *path = ccl_new_array(uint32_t,size);

  for(i = 0; i < size; i++) 
    R[i] = i;
    
  s_randomize_array(R,size);

  for(i = 0; i < size-1; i++) 
    path[R[i]] = R[i+1];

  path[R[i]] = R[0];
  ccl_delete(R);

  return path;
}

			/* --------------- */

static exit_automaton
s_generate_random_ea(uint32_t size, uint32_t alphabet_size)
{
  uint32_t      s;
  uint32_t      a;
  exit_automaton ea = exit_automaton_create(size,0,alphabet_size);
  uint32_t  *path = s_rnd_path(size);
  
  for(s = 0; s < size; s++)
    {
      uint32_t path_letter = rnd_modulo(alphabet_size);
      ea->is_final[s] = rnd_probability() < 0.5;
      for(a = 0; a < alphabet_size; a++)
	{
	  uint32_t succ;

	  if( a == path_letter ) succ = path[s];
	  else                   succ = rnd_modulo(size);

	  exit_automaton_set_successor(ea,s,a,succ,0);
	}
    }
  ccl_delete(path);

  return ea;
}
