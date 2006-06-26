/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.3 $
 * @date    : $Date: 2006/01/25 16:14:12 $
 */
#include "sataf-msa-p.h"
#include "sataf-msa.h"

# define DUP(a)  sataf_msa_add_reference(a)
# define DEL(a)  sataf_msa_del_reference(a)
# define ONE(a)  sataf_msa_is_one(a)
# define ZERO(a) sataf_msa_is_zero(a)
# define SZ(a)   ((a)->A->automaton->alphabet_size)

			/* --------------- */


typedef struct boolean_msa_operator_automaton_st {
  struct sataf_automaton_st super;
  sataf_msa              operands[3];
} *boolean_msa_operator_automaton;

			/* --------------- */

static sataf_automaton
s_create_boolean_automaton(sataf_msa *args, sataf_automaton_methods m);

static sataf_automaton
s_create_ternary(sataf_msa arg1, 
		 sataf_msa arg2,
		 sataf_msa arg3, 
		 sataf_automaton_methods m);

static sataf_automaton
s_create_binary(sataf_msa arg1, 
		sataf_msa arg2,
		sataf_automaton_methods m);

static sataf_automaton
s_create_unary(sataf_msa arg1, 
	       sataf_automaton_methods m);

			/* --------------- */

static void
s_boolean_msa_operator_destroy(sataf_automaton self);

static uint32_t
s_boolean_msa_operator_get_alphabet_size(sataf_automaton self);

#define s_boolean_msa_operator_to_string NULL

static sataf_automaton
s_boolean_msa_operator_succ(sataf_automaton self, uint32_t letter);

static int
s_boolean_msa_operator_is_final_OR(sataf_automaton self);

static int
s_boolean_msa_operator_is_final_AND(sataf_automaton self);

static int
s_boolean_msa_operator_is_final_IMPLY(sataf_automaton self);

static int
s_boolean_msa_operator_is_final_EQUIV(sataf_automaton self);

static int
s_boolean_msa_operator_is_final_NOT(sataf_automaton self);

static int
s_boolean_msa_operator_is_final_ITE(sataf_automaton self);

static int
s_boolean_msa_operator_is_equal_to(sataf_automaton self, 
				   sataf_automaton other);

static uint32_t
s_boolean_msa_operator_hashcode(sataf_automaton self);

# define s_boolean_msa_operator_to_dot NULL

			/* --------------- */

# define DECL_OP_METHODS(ID) \
static struct sataf_automaton_methods_st ID ## _METHODS = { \
    "SA-BOOLEAN-OP", \
    sizeof(struct boolean_msa_operator_automaton_st), \
    s_boolean_msa_operator_destroy, \
    s_boolean_msa_operator_get_alphabet_size, \
    s_boolean_msa_operator_to_string, \
    s_boolean_msa_operator_succ, \
    s_boolean_msa_operator_is_final_ ## ID, \
    s_boolean_msa_operator_is_equal_to, \
    s_boolean_msa_operator_hashcode, \
    s_boolean_msa_operator_to_dot \
}

			/* --------------- */

DECL_OP_METHODS(OR);
DECL_OP_METHODS(AND);
DECL_OP_METHODS(IMPLY);
DECL_OP_METHODS(EQUIV);
DECL_OP_METHODS(NOT);
DECL_OP_METHODS(ITE);

			/* --------------- */

sataf_msa
sataf_msa_ite(sataf_msa i, sataf_msa t, sataf_msa e)
{
  sataf_msa tmp;
  sataf_msa R;

  if( ONE(i) )       R = DUP(t);
  else if( ZERO(i) ) R = DUP(e);
  else if( ONE(t) )  R = sataf_msa_or(i,e);
  else if( ZERO(t) )
    {
      tmp = sataf_msa_not(i);
      R = sataf_msa_and(tmp,e);
      DEL(tmp);
    }
  else if( ZERO(e) ) R = sataf_msa_and(i,t);
  else if( ONE(e) )
    {
      tmp = sataf_msa_not(i);
      R = sataf_msa_or(tmp,t);
      DEL(tmp);
    }
  else if( (R = sataf_msa_cache_get(i,t,e,&ITE_METHODS)) == NULL )
    {
      sataf_automaton ite = s_create_ternary(i,t,e,&ITE_METHODS);
      
      R = sataf_msa_compute(ite);

      sataf_automaton_del_reference(ite);

      sataf_msa_cache_put(i,t,e,&ITE_METHODS,R);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_not(sataf_msa a)
{
  sataf_msa R;

  if( ONE(a) )       R = sataf_msa_zero(SZ(a));
  else if( ZERO(a) ) R = sataf_msa_one(SZ(a));
  else if( (R = sataf_msa_cache_get(a,NULL,NULL,&NOT_METHODS)) == NULL )
    {
      sataf_automaton aut = s_create_unary(a,&NOT_METHODS);

      R = sataf_msa_compute(aut);

      sataf_automaton_del_reference(aut);
      sataf_msa_cache_put(a,NULL,NULL,&NOT_METHODS,R);
    }

  return R;
} 

			/* --------------- */


static sataf_msa
s_apply_binary(sataf_msa a1, sataf_msa a2, sataf_automaton_methods m)
{
  sataf_msa R = sataf_msa_cache_get(a1,a2,NULL,m);
  
  if( R == NULL )
    {
      sataf_automaton sop = s_create_binary(a1,a2,m);

      R = sataf_msa_compute(sop);
      sataf_automaton_del_reference(sop);
      sataf_msa_cache_put(a1,a2,NULL,m,R);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_or(sataf_msa a1, sataf_msa a2)
{
  sataf_msa R;

  if( ZERO(a1) )      R = DUP(a2);
  else if( ZERO(a2) ) R = DUP(a1);
  else if( ONE(a1) )  R = sataf_msa_one(SZ(a1));
  else if( ONE(a2) )  R = sataf_msa_one(SZ(a1));
  else if( a1 == a2 ) R = DUP(a1);
  else 
    {
      if( a2 < a1 ) { sataf_msa tmp = a1; a1 = a2; a2 = tmp; }
      R = s_apply_binary(a1,a2,&OR_METHODS);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_and(sataf_msa a1, sataf_msa a2)
{
  sataf_msa R;

  if( ZERO(a1) )      R = sataf_msa_zero(SZ(a1));
  else if( ZERO(a2) ) R = sataf_msa_zero(SZ(a1));
  else if( ONE(a1) )  R = DUP(a2);
  else if( ONE(a2) )  R = DUP(a1);
  else if( a1 == a2 ) R = DUP(a1);
  else 
    {
      if( a2 < a1 ) { sataf_msa tmp = a1; a1 = a2; a2 = tmp; }
      R = s_apply_binary(a1,a2,&AND_METHODS);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_imply(sataf_msa a1, sataf_msa a2)
{
  sataf_msa R;

  if( ZERO(a1) )      R = sataf_msa_one(SZ(a1));
  else if( ZERO(a2) ) R = sataf_msa_not(a1);
  else if( ONE(a1) )  R = DUP(a2);
  else if( ONE(a2) )  R = sataf_msa_one(SZ(a1));
  else if( a1 == a2 ) R = sataf_msa_one(SZ(a1));
  else R = s_apply_binary(a1,a2,&IMPLY_METHODS);

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_equiv(sataf_msa a1, sataf_msa a2)
{
  sataf_msa R;

  if( ZERO(a1) )      R = sataf_msa_not(a2);
  else if( ONE(a1) )  R = DUP(a2);
  else if( ZERO(a2) ) R = sataf_msa_not(a1);
  else if( ONE(a2) )  R = DUP(a1);
  else if( a1 == a2 ) R = sataf_msa_one(SZ(a1));
  else 
    {
      if( a2 < a1 ) { sataf_msa tmp = a1; a1 = a2; a2 = tmp; }
      R = s_apply_binary(a1,a2,&EQUIV_METHODS);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_forall(sataf_msa a)
{
  sataf_msa R = sataf_msa_cache_get(a,NULL,NULL,(void *)0x2);

  if( R == NULL )
    {
      uint32_t   i;
      uint32_t len = sataf_msa_get_alphabet_size(a);
      sataf_msa tmp2;
      sataf_msa tmp1;
      
      sataf_msa R = sataf_msa_succ(a,0);
      for(i = 1; i < len; i++)
	{
	  tmp1 = sataf_msa_succ(a,i);
	  tmp2 = sataf_msa_and(R,tmp1);
	  sataf_msa_del_reference(tmp1);
	  sataf_msa_del_reference(R);
	  R = tmp2;
	}

      sataf_msa_cache_put(a,NULL,NULL,(void *)0x2,R);
    }

  return R;
}

			/* --------------- */

sataf_msa
sataf_msa_exists(sataf_msa a)
{
  sataf_msa R = sataf_msa_cache_get(a,NULL,NULL,(void *)0x1);

  if( R == NULL )
    {
      uint32_t   i;
      uint32_t len = sataf_msa_get_alphabet_size(a);
      sataf_msa tmp2;
      sataf_msa tmp1;
      
      R = sataf_msa_succ(a,0);
      for(i = 1; i < len; i++)
	{
	  tmp1 = sataf_msa_succ(a,i);
	  tmp2 = sataf_msa_or(R,tmp1);
	  sataf_msa_del_reference(tmp1);
	  sataf_msa_del_reference(R);
	  R = tmp2;
	}
      sataf_msa_cache_put(a,NULL,NULL,(void *)0x1,R);
    }

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_boolean_automaton(sataf_msa *args, sataf_automaton_methods m)
{
  boolean_msa_operator_automaton R;
  
  R = (boolean_msa_operator_automaton)
    sataf_automaton_create(sizeof(struct boolean_msa_operator_automaton_st),m);


  R->operands[0] = sataf_msa_add_reference(args[0]);
  if( args[1] )
    {
      R->operands[1] = sataf_msa_add_reference(args[1]);
      if( args[2] )
	R->operands[2] = sataf_msa_add_reference(args[2]);
    }

  return (sataf_automaton)R;
}

			/* --------------- */

static sataf_automaton
s_create_ternary(sataf_msa arg1, sataf_msa arg2, sataf_msa arg3, 
		 sataf_automaton_methods m)
{
  sataf_msa    args[3];
  sataf_automaton R;

  args[0] = arg1;
  args[1] = arg2;
  args[2] = arg3;
  R =  s_create_boolean_automaton(args,m);

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_binary(sataf_msa arg1, sataf_msa arg2, sataf_automaton_methods m)
{
  return s_create_ternary(arg1,arg2,NULL,m);
}

			/* --------------- */


static sataf_automaton
s_create_unary(sataf_msa arg1, sataf_automaton_methods m)
{
  return s_create_binary(arg1,NULL,m);
}

			/* --------------- */

static void
s_boolean_msa_operator_destroy(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  if( boa->operands[0] != NULL )
    DEL(boa->operands[0]);

  if( boa->operands[1] != NULL )
    DEL(boa->operands[1]);

  if( boa->operands[2] != NULL )
    DEL(boa->operands[2]);
}

			/* --------------- */


static uint32_t
s_boolean_msa_operator_get_alphabet_size(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  return boa->operands[0]->A->automaton->alphabet_size;
}

			/* --------------- */

static sataf_automaton
s_boolean_msa_operator_succ(sataf_automaton self, uint32_t letter)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;
  boolean_msa_operator_automaton R = (boolean_msa_operator_automaton)
    sataf_automaton_create(sizeof(struct boolean_msa_operator_automaton_st),
			   self->methods);

  

  R->operands[0] = sataf_msa_succ(boa->operands[0],letter);
  if( boa->operands[1] )
    {
      R->operands[1] = sataf_msa_succ(boa->operands[1],letter);
      if( boa->operands[2] )
	R->operands[2] = sataf_msa_succ(boa->operands[2],letter);
    }
  
  return (sataf_automaton)R;
}

			/* --------------- */

static int
s_boolean_msa_operator_is_final_OR(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  return MSA_IS_FINAL(boa->operands[0])
    ||   MSA_IS_FINAL(boa->operands[1]);
}

			/* --------------- */

static int
s_boolean_msa_operator_is_final_AND(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  return MSA_IS_FINAL(boa->operands[0])
    &&   MSA_IS_FINAL(boa->operands[1]);
}


			/* --------------- */

static int
s_boolean_msa_operator_is_final_NOT(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  return ! MSA_IS_FINAL(boa->operands[0]);
}

			/* --------------- */

static int
s_boolean_msa_operator_is_final_IMPLY(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  return ! MSA_IS_FINAL(boa->operands[0])
    ||     MSA_IS_FINAL(boa->operands[1]);
}

			/* --------------- */

static int
s_boolean_msa_operator_is_final_EQUIV(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;
  int                         v0 = MSA_IS_FINAL(boa->operands[0]);
  int                         v1 = MSA_IS_FINAL(boa->operands[1]);

  return (v0&&v1) || (!v0&&!v1);
}

			/* --------------- */

static int
s_boolean_msa_operator_is_final_ITE(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;

  if( MSA_IS_FINAL(boa->operands[0]) )
    return MSA_IS_FINAL(boa->operands[1]);
  return MSA_IS_FINAL(boa->operands[2]);
}

			/* --------------- */

static int
s_boolean_msa_operator_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  boolean_msa_operator_automaton boa1 = (boolean_msa_operator_automaton)self;
  boolean_msa_operator_automaton boa2 = (boolean_msa_operator_automaton)other;
  
  return self->methods->is_final == other->methods->is_final 
    &&   boa1->operands[0] == boa2->operands[0]
    &&   boa1->operands[1] == boa2->operands[1]
    &&   boa1->operands[2] == boa2->operands[2];
}

			/* --------------- */

static uint32_t
s_boolean_msa_operator_hashcode(sataf_automaton self)
{
  boolean_msa_operator_automaton boa = (boolean_msa_operator_automaton)self;
  return (uint32_t)(self->methods->is_final)
    +   21911*(uint32_t)(boa->operands[0])
    +   2729*(uint32_t)(boa->operands[1])
    +   163*(uint32_t)(boa->operands[2]);
}
