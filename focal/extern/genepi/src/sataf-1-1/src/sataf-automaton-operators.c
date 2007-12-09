/* $Id: sataf-automaton-operators.c,v 1.3 2006/03/09 16:34:36 point Exp $ */
#include <ccl-list.h>
#include <ccl-assert.h>
#include "sataf.h"

			/* --------------- */

typedef struct boolean_operator_automaton_st {
  struct sataf_automaton_st super;
  sataf_automaton *operands;
} *boolean_operator_automaton;

typedef struct boolean_operator_methods_st {
  struct sataf_automaton_methods_st super;
  uint32_t width;
  uint32_t refcount;
} *boolean_operator_methods;

			/* --------------- */

static sataf_automaton
s_create_boolean_automaton(ccl_list args, boolean_operator_methods m);

static sataf_automaton
s_create_ternary(sataf_automaton arg1, 
		 sataf_automaton arg2,
		 sataf_automaton arg3, 
		 boolean_operator_methods m);

static sataf_automaton
s_create_binary(sataf_automaton arg1, 
		sataf_automaton arg2,
		boolean_operator_methods m);

static sataf_automaton
s_create_unary(sataf_automaton arg1, 
	       boolean_operator_methods m);

			/* --------------- */

static void
s_boolean_operator_destroy(sataf_automaton self);

# define s_boolean_operator_to_string NULL

static uint32_t
s_boolean_operator_get_alphabet_size(sataf_automaton self);

static sataf_automaton
s_boolean_operator_succ(sataf_automaton self, uint32_t letter);

static int
s_boolean_operator_is_final_OR(sataf_automaton self);

static int
s_boolean_operator_is_final_AND(sataf_automaton self);

static int
s_boolean_operator_is_final_IMPLY(sataf_automaton self);

static int
s_boolean_operator_is_final_EQUIV(sataf_automaton self);

static int
s_boolean_operator_is_final_NOT(sataf_automaton self);

static int
s_boolean_operator_is_final_ITE(sataf_automaton self);

static int
s_boolean_operator_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_boolean_operator_hashcode(sataf_automaton self);

# define s_boolean_operator_to_dot NULL

# define DUP(_a) sataf_automaton_add_reference(_a)
# define DEL(_a) sataf_automaton_del_reference(_a)

			/* --------------- */

# define DECL_OP_METHODS(ID) \
static struct boolean_operator_methods_st ID ## _METHODS = { \
  { \
    "SA-BOOLEAN-OP", \
    sizeof(struct boolean_operator_automaton_st), \
    s_boolean_operator_destroy, \
    s_boolean_operator_get_alphabet_size, \
    s_boolean_operator_to_string, \
    s_boolean_operator_succ, \
    s_boolean_operator_is_final_ ## ID, \
    s_boolean_operator_is_equal_to, \
    s_boolean_operator_hashcode, \
    s_boolean_operator_to_dot \
  }, \
  0, \
  0 \
}

			/* --------------- */

DECL_OP_METHODS(OR);
DECL_OP_METHODS(AND);
DECL_OP_METHODS(IMPLY);
DECL_OP_METHODS(EQUIV);
DECL_OP_METHODS(NOT);
DECL_OP_METHODS(ITE);

			/* --------------- */

sataf_automaton
sataf_automaton_create_ite(sataf_automaton i, 
			   sataf_automaton t, 
			   sataf_automaton e)
{
  sataf_automaton   R;
  sataf_automaton tmp;

  ccl_pre( sataf_automaton_get_alphabet_size(i) == 
	   sataf_automaton_get_alphabet_size(t));
  ccl_pre( sataf_automaton_get_alphabet_size(t) == 
	   sataf_automaton_get_alphabet_size(e));

  if( i == SATAF_ONE )       R = DUP(t);
  else if( i == SATAF_ZERO ) R = DUP(e);
  else if( t == SATAF_ONE )  R = sataf_automaton_create_or(i,e);
  else if( t == SATAF_ZERO ) 
    {
      tmp = sataf_automaton_create_not(i);
      R = sataf_automaton_create_and(tmp,e);
      DEL(tmp);
    }
  else if( e == SATAF_ZERO ) R = sataf_automaton_create_and(i,t);
  else if( e == SATAF_ONE )
    {
      tmp = sataf_automaton_create_not(i);
      R = sataf_automaton_create_or(tmp,t);
      DEL(tmp);
    }
  else
    {
      R = s_create_ternary(i,t,e,&ITE_METHODS);
    }
    
  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_not(sataf_automaton a)
{
  sataf_automaton R;

  ccl_pre( a != NULL );

  if( a == SATAF_ONE )       R = DUP(SATAF_ZERO);
  else if( a == SATAF_ZERO ) R = DUP(SATAF_ONE);
  else                       R = s_create_unary(a,&NOT_METHODS);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_or(sataf_automaton a1, 
			  sataf_automaton a2)
{
  ccl_list     args = ccl_list_create();
  sataf_automaton R;

  ccl_pre( sataf_automaton_get_alphabet_size(a1) == 
	   sataf_automaton_get_alphabet_size(a2));

  ccl_list_add(args,a1);
  ccl_list_add(args,a2);
  R = sataf_automaton_create_multi_or(args);
  ccl_list_delete(args);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_multi_or(ccl_list a)
{
  return s_create_boolean_automaton(a,&OR_METHODS);
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_and(sataf_automaton a1, 
			   sataf_automaton a2)
{
  ccl_list     args = ccl_list_create();
  sataf_automaton R;

  ccl_pre( sataf_automaton_get_alphabet_size(a1) == 
	   sataf_automaton_get_alphabet_size(a2));

  ccl_list_add(args,a1);
  ccl_list_add(args,a2);
  R = sataf_automaton_create_multi_and(args);
  ccl_list_delete(args);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_multi_and(ccl_list a)
{
  return s_create_boolean_automaton(a,&AND_METHODS);
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_imply(sataf_automaton a1, 
			     sataf_automaton a2)
{
  sataf_automaton R;

  ccl_pre( sataf_automaton_get_alphabet_size(a1) == 
	   sataf_automaton_get_alphabet_size(a2));

  if( a1 == SATAF_ZERO )      R = DUP(SATAF_ONE);
  else if( a2 == SATAF_ZERO ) R = sataf_automaton_create_not(a1);
  else if( a1 == SATAF_ONE )  R = DUP(a2);
  else if( a2 == SATAF_ONE )  R = DUP(SATAF_ONE);
  else                        R = s_create_binary(a1,a2,&IMPLY_METHODS);

  return R;
}

			/* --------------- */

sataf_automaton
sataf_automaton_create_equiv(sataf_automaton a1, 
		    sataf_automaton a2)
{
  sataf_automaton R;

  ccl_pre( sataf_automaton_get_alphabet_size(a1) == 
	   sataf_automaton_get_alphabet_size(a2));

  if( a1 == SATAF_ZERO )      R = sataf_automaton_create_not(a2);
  else if( a1 == SATAF_ONE )  R = DUP(a2);
  else if( a2 == SATAF_ZERO ) R = sataf_automaton_create_not(a1);
  else if( a2 == SATAF_ONE )  R = DUP(a1);
  else                        R = s_create_binary(a1,a2,&EQUIV_METHODS);

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_boolean_automaton(ccl_list args, boolean_operator_methods m)
{
  int                        i;
  ccl_pair                   p;
  boolean_operator_automaton R;
  boolean_operator_methods   M = ccl_new(struct boolean_operator_methods_st);

  *M = *m;
  M->refcount = 1;
  M->width    = ccl_list_get_size(args);

  R = (boolean_operator_automaton)
    sataf_automaton_create(sizeof(struct boolean_operator_automaton_st),
			   (sataf_automaton_methods)M);
  R->operands = ccl_new_array(sataf_automaton,M->width);

  for(i = 0, p = FIRST(args); p; p = CDR(p), i++)
    R->operands[i] = sataf_automaton_add_reference(CAR(p));

  return (sataf_automaton)R;
}

			/* --------------- */

static sataf_automaton
s_create_ternary(sataf_automaton arg1, 
		 sataf_automaton arg2,
		 sataf_automaton arg3, 
		 boolean_operator_methods m)
{
  ccl_list     args = ccl_list_create();
  sataf_automaton R;

  ccl_list_add(args,arg1);
  if( arg2 != NULL )
    {
      ccl_list_add(args,arg2);
      if( arg3 != NULL )
	ccl_list_add(args,arg3);;
    }
  R =  s_create_boolean_automaton(args,m);
  ccl_list_delete(args);

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_binary(sataf_automaton arg1, 
		sataf_automaton arg2,
		 boolean_operator_methods m)
{
  return s_create_ternary(arg1,arg2,NULL,m);
}

			/* --------------- */


static sataf_automaton
s_create_unary(sataf_automaton arg1, 
	       boolean_operator_methods m)
{
  return s_create_binary(arg1,NULL,m);
}

			/* --------------- */

static void
s_boolean_operator_destroy(sataf_automaton self)
{
  uint32_t                   i;
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  boolean_operator_methods     m = (boolean_operator_methods)self->methods;

  for(i = 0; i < m->width; i++)
    DEL(boa->operands[i]);
  ccl_delete(boa->operands);

  if( --m->refcount == 0 )
    ccl_delete(m);
}

			/* --------------- */

static uint32_t
s_boolean_operator_get_alphabet_size(sataf_automaton self)
{
  boolean_operator_automaton boa = (boolean_operator_automaton)self;

  return sataf_automaton_get_alphabet_size(boa->operands[0]);
}

			/* --------------- */

static sataf_automaton
s_boolean_operator_succ(sataf_automaton self, uint32_t letter)
{
  uint32_t                   i;
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  boolean_operator_methods     m = (boolean_operator_methods)self->methods;
  boolean_operator_automaton   R = (boolean_operator_automaton)
    sataf_automaton_create(sizeof(struct boolean_operator_automaton_st),
			   self->methods);

  R->operands = ccl_new_array(sataf_automaton,m->width);
  m->refcount++;

  for(i = 0; i < m->width; i++)
    R->operands[i] = sataf_automaton_succ(boa->operands[i],letter);

  return (sataf_automaton)R;
}

			/* --------------- */

static int
s_boolean_operator_is_final_OR(sataf_automaton self)
{
  int                          r;
  uint32_t                   i;
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  boolean_operator_methods     m = (boolean_operator_methods)self->methods;

  r = sataf_automaton_is_final(boa->operands[0]);
  for(i = 1; !r && i < m->width; i++)
    r = sataf_automaton_is_final(boa->operands[i]);

  return r;
}

			/* --------------- */

static int
s_boolean_operator_is_final_AND(sataf_automaton self)
{
  int                          r;
  uint32_t                   i;
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  boolean_operator_methods     m = (boolean_operator_methods)self->methods;

  r = sataf_automaton_is_final(boa->operands[0]);
  for(i = 1; r && i < m->width; i++)
    r = sataf_automaton_is_final(boa->operands[i]);

  return r;
}

			/* --------------- */

static int
s_boolean_operator_is_final_NOT(sataf_automaton self)
{
  boolean_operator_automaton boa = (boolean_operator_automaton)self;

  ccl_pre( ((boolean_operator_methods)self->methods)->width == 1 );

  return ! sataf_automaton_is_final(boa->operands[0]);
}

			/* --------------- */

static int
s_boolean_operator_is_final_IMPLY(sataf_automaton self)
{
  boolean_operator_automaton boa = (boolean_operator_automaton)self;

  ccl_pre( ((boolean_operator_methods)self->methods)->width == 2 );

  return ! sataf_automaton_is_final(boa->operands[0])
    ||     sataf_automaton_is_final(boa->operands[1]);
}

			/* --------------- */

static int
s_boolean_operator_is_final_EQUIV(sataf_automaton self)
{
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  int                         v0 = sataf_automaton_is_final(boa->operands[0]);
  int                         v1 = sataf_automaton_is_final(boa->operands[1]);

  ccl_pre( ((boolean_operator_methods)self->methods)->width == 2 );

  return (v0&&v1) || (!v0&&!v1);
}

			/* --------------- */

static int
s_boolean_operator_is_final_ITE(sataf_automaton self)
{
  boolean_operator_automaton boa = (boolean_operator_automaton)self;

  ccl_pre( ((boolean_operator_methods)self->methods)->width == 3 );

  if( sataf_automaton_is_final(boa->operands[0]) )
    return sataf_automaton_is_final(boa->operands[1]);
  return sataf_automaton_is_final(boa->operands[2]);
}

			/* --------------- */

static int
s_boolean_operator_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  uint32_t                    i;
  boolean_operator_automaton boa1 = (boolean_operator_automaton)self;
  boolean_operator_automaton boa2 = (boolean_operator_automaton)other;
  boolean_operator_methods     m1 = (boolean_operator_methods)self->methods;
  boolean_operator_methods     m2 = (boolean_operator_methods)self->methods;
  
  if( m1->super.is_final != m2->super.is_final )
    return 0;

  if( m1->width != m2->width )
    return 0;

  for(i = 0; i < m1->width; i++)
    {
      if( ! sataf_automaton_equals(boa1->operands[i],boa2->operands[i]) )
	return 0;
    }

  return 1;
}

			/* --------------- */


static uint32_t
s_boolean_operator_hashcode(sataf_automaton self)
{
  uint32_t                   i;
  boolean_operator_automaton boa = (boolean_operator_automaton)self;
  boolean_operator_methods     m = (boolean_operator_methods)self->methods;
  uint32_t                   r = m->width;

  for(i = 0; i < m->width; i++)
    r += 19*r+sataf_automaton_hashcode(boa->operands[i]);

  return r;
}

			/* --------------- */

