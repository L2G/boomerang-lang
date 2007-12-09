/* $Id: sataf-tester.c,v 1.3 2006/03/09 16:34:36 point Exp $ */
#include <stdio.h>
#include <ccl-init.h>
#include <sataf.h>
#include <sataf-msa.h>
#include <exit-automaton.h>
#include "random-automaton.h"
#include "rnd.h"

#if 1
# define MIN_S_SIZE 1
# define MAX_S_SIZE 20
# define MIN_A_SIZE 1
# define MAX_A_SIZE 4
# define NB_TEST    2
#else
# define MIN_S_SIZE 2
# define MAX_S_SIZE 2
# define MIN_A_SIZE 2
# define MAX_A_SIZE 2
# define NB_TEST    2
#endif

#define SEED 1106429888
#ifndef SEED
# define SEED 1234567891
#endif

# define ASZ(_a) (sizeof(_a)/sizeof((_a)[0]))

static void s_init(void);
static void s_terminate(void);

static int s_test(uint32_t size, uint32_t alphabet_size);
static sataf_msa
s_msa_power(sataf_msa msa, int N);

int main(int argc, char **argv)
{
  s_init();
  ccl_try(exception)
    {
      uint32_t i,j,k;
      int ok = 1;

      {
	sataf_msa eps = sataf_msa_epsilon(2);
	sataf_msa eps2 = sataf_msa_power(eps,2);
	sataf_msa epseps = sataf_msa_concat(eps,eps);
	ccl_assert( epseps == eps );
	ccl_assert( eps == eps2 );
	sataf_msa_del_reference(epseps);
	sataf_msa_del_reference(eps2);
	sataf_msa_del_reference(eps);
      }

      {
	uint8_t final[] = { 0, 0, 0, 0, 1, 0 };
	uint32_t succ[] = { 1, 2, 3, 4, 5, 3 };
	sataf_automaton a = 
	  sataf_automaton_create_from_arrays(6,1,0,final,succ);
	sataf_msa A = sataf_msa_compute(a);

	sataf_msa_del_reference(A);
	sataf_automaton_del_reference(a);
      }

      {
	uint8_t final[] = { 1, 0 };
	uint32_t succ[] = { 0, 1, 0, 0 };
	sataf_automaton a = 
	  sataf_automaton_create_from_arrays(2,2,0,final,succ);
	sataf_msa A = sataf_msa_compute(a);
	sataf_msa AA = sataf_msa_concat(A,A);
	sataf_msa A2 = sataf_msa_power(A,2);

	ccl_assert(AA == A2 );

	sataf_msa_del_reference(A);
	sataf_msa_del_reference(AA);
	sataf_msa_del_reference(A2);
	sataf_automaton_del_reference(a);
      }

      {
	uint8_t final[] = { 0, 1, 0 };
	uint32_t succ[] = { 1, 2, 0 };
	sataf_automaton a = 
	  sataf_automaton_create_from_arrays(3,1,0,final,succ);
	sataf_msa A = sataf_msa_compute(a);
	sataf_msa AA = sataf_msa_concat(A,A);
	sataf_msa A2 = sataf_msa_power(A,2);

	ccl_assert( AA == A2 );

	sataf_msa_del_reference(A);
	sataf_msa_del_reference(AA);
	sataf_msa_del_reference(A2);
	sataf_automaton_del_reference(a);
      }

      {
	uint8_t final[] = { 0, 0 };
	uint32_t succ[] = { 
	  exit_automaton_encode_succ_as_exit_state(1),
	  exit_automaton_encode_succ_as_local_state(1),
	  exit_automaton_encode_succ_as_local_state(0),
	  exit_automaton_encode_succ_as_exit_state(0)
	};

	exit_automaton ea = 
	  exit_automaton_create_with_arrays(2,2,2,final,succ);
	uint32_t h[2];
	exit_automaton min = exit_automaton_minimize(ea,h);
	exit_automaton_del_reference(min);
	exit_automaton_del_reference(ea);
      }

      {
	uint8_t is_final[] = { 0, 0, 0, 0, 1 };
	uint32_t succ[] = { 2, 1, 0, 3, 3, 4, 4, 2, 4, 4 };
	sataf_automaton A = 
	  sataf_automaton_create_from_arrays(ASZ(is_final),2,0,is_final,succ);
	sataf_msa R = sataf_msa_compute(A);
	sataf_msa_del_reference(R);
	sataf_automaton_del_reference(A);
      }

      rnd_set_seed(SEED);

      for(i = MIN_S_SIZE; ok && i <= MAX_S_SIZE ; i++)
	{
	  for(j = MIN_A_SIZE; ok && j <= MAX_A_SIZE ; j++)
	    {
	      ccl_warning("TEST #S=%d #A=%d SEED=%d\n",i,j,
		      rnd_get_seed());
	      for(k=0; ok && k<NB_TEST; k++)
		ok = s_test(i,j);	      
	    }
	}
    }
  ccl_catch
    {
      ccl_exception_print();
    }
  ccl_end_try;
  s_terminate();

  return 1;
}

			/* --------------- */

static void
s_listener(ccl_log_type type, const char *msg, void *data)
{
  if( type == CCL_LOG_DISPLAY )
    fprintf(stdout,"%s",msg);
  else
    fprintf(stderr,"%s",msg);
}

			/* --------------- */

static void s_init(void)
{
  ccl_init(0);
  sataf_init();
  ccl_log_add_listener(s_listener,NULL);
}

			/* --------------- */

static void s_terminate(void)
{
  sataf_terminate();
  ccl_terminate();
}

			/* --------------- */
static int Z = 0;

static int s_apply_t0(int *t, int size, int alphabet_size,
		      int (*test)(int,int,int))
{
  return test((*t)++,size,alphabet_size);
}

			/* --------------- */

static int s_apply_t1(int *t, sataf_msa *T, int Tlen, 
		      int (*test)(int,sataf_msa))
{
  int result = 1;
  int i;

  for(i = 0; result && i < Tlen; i++)
    result = result && test((*t)++,T[i]);

  return result;
}

			/* --------------- */

static int s_apply_t2(int *t, sataf_msa *T, int Tlen, 
		      int (*test)(int,sataf_msa,sataf_msa))
{
  int result = 1;
  int i,j;

  for(i = 0; result && i < Tlen; i++)
    for(j = 0; result && j < Tlen; j++)
      result = result && test((*t)++,T[i],T[j]);

  return result;
}

			/* --------------- */

static int s_apply_t3(int *t, sataf_msa *T, int Tlen, 
		      int (*test)(int,sataf_msa,sataf_msa,sataf_msa))
{
  int result = 1;
  int i,j,k;

  for(i = 0; result && i < Tlen; i++)
    for(j = 0; result && j < Tlen; j++)
      for(k = 0; result && k < Tlen; k++)
      result = result && test((*t)++,T[i],T[j],T[k]);

  return result;
}

			/* --------------- */

static int s_t01(int t, sataf_msa SA)
{
  int result;
  sataf_automaton  saA = sataf_msa_to_automaton(SA);
  sataf_msa       SsaA = sataf_msa_compute(saA);

  if( ! (result = ( SsaA == SA )) )
    ccl_warning("ERR %d.%d: SA <=/=> SSA\n",Z,t);
  sataf_automaton_del_reference(saA);
  sataf_msa_del_reference(SsaA);    

  return result;
}

static int s_t02(int t, sataf_msa SA)
{
  int result;
  sataf_msa       notA = sataf_msa_not(SA);
  sataf_msa A_and_notA = sataf_msa_and(SA,notA);
    

  if( ! (result = sataf_msa_is_zero(A_and_notA))  )
    {
      sataf_msa_display_as_dot(SA,1,0);
      sataf_msa_display_as_dot(notA,1,0);
      sataf_msa_display_as_dot(A_and_notA,1,0);
      ccl_warning("ERR %d.%d: a&&!a != 0\n",Z,t);
    }
  
  sataf_msa_del_reference(A_and_notA);
  sataf_msa_del_reference(notA);
    
  return result;
}

			/* --------------- */

static int s_t03(int t, sataf_msa SA)
{
  int result;
  sataf_msa A_and_A = sataf_msa_and(SA,SA);
    
  if( !( result = (A_and_A == SA)) )
    ccl_warning("ERR %d.%d: (a&a) != a\n",Z,t);

  sataf_msa_del_reference(A_and_A);
    
  return result;
}

			/* --------------- */

static int s_t04(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa A_and_B = sataf_msa_and(SA,SB);
  sataf_msa B_and_A = sataf_msa_and(SB,SA);

  if( ! (result=(A_and_B == B_and_A)) )
    ccl_warning("ERR %d.%d: (a&b) != (b&a)\n",Z,t);

  sataf_msa_del_reference(A_and_B);
  sataf_msa_del_reference(B_and_A);

  return result;
}

			/* --------------- */

static int s_t05(int t, sataf_msa SA, sataf_msa SB, sataf_msa SC)
{
  int result;
  sataf_msa       A_and_B1 = sataf_msa_and(SA,SB);
  sataf_msa A_and_B1_and_C = sataf_msa_and(A_and_B1,SC);
  sataf_msa       B_and_C1 = sataf_msa_and(SB,SC);
  sataf_msa A_and_B_and_C1 = sataf_msa_and(SA,B_and_C1);

  if( ! (result=(A_and_B1_and_C == A_and_B_and_C1)) )
    ccl_warning("ERR %d.%d: (a&b)&c != a&(b&c)\n",Z,t);
  sataf_msa_del_reference(A_and_B1);
  sataf_msa_del_reference(A_and_B1_and_C);
  sataf_msa_del_reference(B_and_C1);
  sataf_msa_del_reference(A_and_B_and_C1);
  
  return result;
}

			/* --------------- */

static int s_t06(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa          A_and_B = sataf_msa_and(SA,SB);
  sataf_msa             notA = sataf_msa_not(SA);
  sataf_msa A_and_B_and_notA = sataf_msa_and(A_and_B,notA);

  if( ! (result=sataf_msa_is_zero(A_and_B_and_notA)) )
    ccl_warning("ERR %d.%d: (a&b)&!a != 0\n",Z,t);
    
  sataf_msa_del_reference(A_and_B);
  sataf_msa_del_reference(notA);
  sataf_msa_del_reference(A_and_B_and_notA);

  return result;
}

			/* --------------- */

static int s_t07(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa          A_and_B = sataf_msa_and(SA,SB);
  sataf_msa A_and_B_and_A = sataf_msa_and(A_and_B,SA);

  if( ! (result=(A_and_B == A_and_B_and_A)) )
    ccl_warning("ERR %d.%d: (a&b)&a != a&b\n",Z,t);
  sataf_msa_del_reference(A_and_B);
  sataf_msa_del_reference(A_and_B_and_A);

  return result;
}

			/* --------------- */

static int s_t08(int t, sataf_msa SA, sataf_msa SB, sataf_msa SC)
{
  int result;
  sataf_msa          B_or_C = sataf_msa_or(SB,SC);
  sataf_msa    A_and_B_or_C = sataf_msa_and(SA,B_or_C);
  sataf_msa         A_and_B = sataf_msa_and(SA,SB);
  sataf_msa         A_and_C = sataf_msa_and(SA,SC);
  sataf_msa A_and_B_or_A_and_C = sataf_msa_or(A_and_B,A_and_C);
  
  if( ! (result=(A_and_B_or_C == A_and_B_or_A_and_C)) )
    ccl_warning("ERR %d.%d: a&(b|c) != a&b|a&c\n",Z,t);
  sataf_msa_del_reference(B_or_C);
  sataf_msa_del_reference(A_and_B_or_C);
  sataf_msa_del_reference(A_and_B);
  sataf_msa_del_reference(A_and_C);
  sataf_msa_del_reference(A_and_B_or_A_and_C);
  
  return result;
}

			/* --------------- */

static int s_t09(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa A_equiv_B = sataf_msa_equiv(SA,SB);
  sataf_msa A_imply_B = sataf_msa_imply(SA,SB);
  sataf_msa B_imply_A = sataf_msa_imply(SB,SA);
  sataf_msa A_imply_B_and_B_imply_A = sataf_msa_and(A_imply_B,B_imply_A);
  
  if( !( (result=(A_equiv_B == A_imply_B_and_B_imply_A))))
    ccl_warning("ERR %d.%d: a<=>b != a=>b & b=>a\n",Z,t);
  
  sataf_msa_del_reference(A_equiv_B);
  sataf_msa_del_reference(A_imply_B);
  sataf_msa_del_reference(B_imply_A);
  sataf_msa_del_reference(A_imply_B_and_B_imply_A);
  
  return result;
}


			/* --------------- */

static int s_t10(int t, sataf_msa SA, sataf_msa SB, sataf_msa SC)
{
  int result;
  sataf_msa B_and_C = sataf_msa_and(SB,SC);
  sataf_msa A_or_B_and_C = sataf_msa_or(SA,B_and_C);
  sataf_msa A_or_B = sataf_msa_or(SA,SB);
  sataf_msa A_or_C = sataf_msa_or(SA,SC);
  sataf_msa A_or_B_and_A_or_C = sataf_msa_and(A_or_B,A_or_C);
  
  if( ! (result=(A_or_B_and_C == A_or_B_and_A_or_C)) )
    ccl_warning("ERR %d.%d: a || (b && c) <=/=> (a || b) && ( a || c)\n",
	    Z,t);
  
  sataf_msa_del_reference(B_and_C);
  sataf_msa_del_reference(A_or_B_and_C);
  sataf_msa_del_reference(A_or_B);
  sataf_msa_del_reference(A_or_C);
  sataf_msa_del_reference(A_or_B_and_A_or_C);
  
  return result;
}


			/* --------------- */

static int s_t11(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa A_or_B = sataf_msa_or(SA,SB);
  sataf_msa notA   = sataf_msa_not(SA);
  sataf_msa notB   = sataf_msa_not(SB);
  sataf_msa notA_and_notB = sataf_msa_and(notA,notB);
  sataf_msa not_notA_and_notB = sataf_msa_not(notA_and_notB);
  
  if(!( (result=(A_or_B == not_notA_and_notB))))
    {
      sataf_msa_display_as_dot(SA,1,0);
      sataf_msa_display_as_dot(SB,1,0);
      sataf_msa_display_as_dot(A_or_B,1,0);
      sataf_msa_display_as_dot(not_notA_and_notB,1,0);
      ccl_warning("ERR %d.%d: a || b != !(!a && !b)\n",Z,t);
    }

  sataf_msa_del_reference(A_or_B);
  sataf_msa_del_reference(notA);
  sataf_msa_del_reference(notB);
  sataf_msa_del_reference(notA_and_notB);
  sataf_msa_del_reference(not_notA_and_notB);
  
  return result;
}


			/* --------------- */

static int s_t12(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa A_and_B = sataf_msa_or(SA,SB);
  sataf_msa notA   = sataf_msa_not(SA);
  sataf_msa notB   = sataf_msa_not(SB);
  sataf_msa notA_or_notB = sataf_msa_and(notA,notB);
  sataf_msa not_notA_or_notB = sataf_msa_not(notA_or_notB);
  
  if(!( (result=(A_and_B == not_notA_or_notB))))
    {
      sataf_msa_display_as_dot(SA,1,0);
      sataf_msa_display_as_dot(SB,1,0);
      sataf_msa_display_as_dot(A_and_B,1,0);
      sataf_msa_display_as_dot(not_notA_or_notB,1,0);
      ccl_warning("ERR %d.%d: a && b != !(!a || !b)\n",Z,t);
    }

  sataf_msa_del_reference(A_and_B);
  sataf_msa_del_reference(notA);
  sataf_msa_del_reference(notB);
  sataf_msa_del_reference(notA_or_notB);
  sataf_msa_del_reference(not_notA_or_notB);
  
  return result;
}


			/* --------------- */

static int s_t13(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa A_imply_B = sataf_msa_imply(SA,SB);
  sataf_msa      notA = sataf_msa_not(SA);
  sataf_msa notA_or_B = sataf_msa_or(notA,SB);
  
  if(!( (result=(A_imply_B == notA_or_B))))
    ccl_warning("ERR %d.%d: a => b != !a || b\n",Z,t);

  sataf_msa_del_reference(A_imply_B);
  sataf_msa_del_reference(notA);
  sataf_msa_del_reference(notA_or_B);
  
  return result;
}


			/* --------------- */

static int s_t14(int t, sataf_msa SA, sataf_msa SB)
{
  int result;
  sataf_msa       A_imply_B = sataf_msa_imply(SA,SB);
  sataf_msa            notA = sataf_msa_not(SA);
  sataf_msa            notB = sataf_msa_not(SB);
  sataf_msa notB_imply_notA = sataf_msa_imply(notB,notA);
  
  if(!( (result=(A_imply_B == notB_imply_notA))))
    ccl_warning("ERR %d.%d: a => b != !b => !a\n",Z,t);

  sataf_msa_del_reference(A_imply_B);
  sataf_msa_del_reference(notA);
  sataf_msa_del_reference(notB);
  sataf_msa_del_reference(notB_imply_notA);
  
  return result;
}


			/* --------------- */

static int s_t15(int t, int size, int alphabet_size)
{
  int result;
  sataf_msa  zero1 = sataf_msa_zero(alphabet_size);
  sataf_msa  zero2 = sataf_msa_zero(alphabet_size);
  
  if(!( (result=(zero1 == zero2))))
    ccl_warning("ERR %d.%d: 0 != 0\n",Z,t);

  sataf_msa_del_reference(zero1);
  sataf_msa_del_reference(zero2);

  return result;
}

static int s_t16(int t, int size, int alphabet_size)
{
  int result;
  sataf_msa  one1 = sataf_msa_one(alphabet_size);
  sataf_msa  one2 = sataf_msa_one(alphabet_size);
  
  if(!( (result=(one1 == one2))))
    ccl_warning("ERR %d.%d: 0 != 0\n",Z,t);

  sataf_msa_del_reference(one1);
  sataf_msa_del_reference(one2);

  return result;
}

static int s_t17(int t, int size, int alphabet_size)
{
  int result;
  sataf_msa   one = sataf_msa_one(alphabet_size);
  sataf_msa  zero = sataf_msa_zero(alphabet_size);
  
  if(!( (result=(one != zero))))
    ccl_warning("ERR %d.%d: 0 == 1\n",Z,t);

  sataf_msa_del_reference(one);
  sataf_msa_del_reference(zero);

  return result;
}

			/* --------------- */

static int
s_t18(int t, sataf_msa A)
{
  int result = 1;
  int asize = sataf_msa_get_alphabet_size(A);
  sataf_msa eps = sataf_msa_epsilon(asize);
  sataf_msa A_c_eps = sataf_msa_concat(A,eps);
  sataf_msa eps_c_A = sataf_msa_concat(eps,A);


  if( eps_c_A != A )
    {
      result = 0;
      ccl_warning("ERR %d.%d: epsilon.a != a\n",Z,t);
    }

  if( A_c_eps != A)
    {
      result = 0;
      ccl_warning("ERR %d.%d: a.epsilon != a\n",Z,t);
    }
  sataf_msa_del_reference(eps_c_A);
  sataf_msa_del_reference(A_c_eps);
  sataf_msa_del_reference(eps);
  
  return result;
}

			/* --------------- */

static int s_t19(int t, sataf_msa A)
{
  int result = 1;
  int asize = sataf_msa_get_alphabet_size(A);
  sataf_msa empty = sataf_msa_empty(asize);
  sataf_msa empty_c_A = sataf_msa_concat(empty,A);
  sataf_msa A_c_empty = sataf_msa_concat(A,empty);


  if( ! sataf_msa_is_zero(empty_c_A) )
    {
      result = 0;
      ccl_warning("ERR %d.%d: {}.a != {}\n",Z,t);
    }

  if( ! sataf_msa_is_zero(A_c_empty) )
    {
      result = 0;
      ccl_warning("ERR %d.%d: a.{} != {}\n",Z,t);
    }

  sataf_msa_del_reference(empty_c_A);
  sataf_msa_del_reference(A_c_empty);
  sataf_msa_del_reference(empty);
  
  return result;
}

			/* --------------- */

static sataf_msa
s_msa_power(sataf_msa msa, int N)
{
  sataf_msa R;

  if( N == 0 ) 
    R = sataf_msa_epsilon(sataf_msa_get_alphabet_size(msa));
  else if( N == 1 )
    R = sataf_msa_add_reference(msa);
  else 
    {
      sataf_msa tmp1 = s_msa_power(msa,N>>1);
      sataf_msa tmp2 = sataf_msa_concat(tmp1,tmp1);

      sataf_msa_del_reference(tmp1);
      
      if( (N%2) == 0 )
	R = sataf_msa_add_reference(tmp2);
      else
	R = sataf_msa_concat(tmp2,msa);

      sataf_msa_del_reference(tmp2);
    }

  return R;
}

			/* --------------- */

static int s_t20(int t, sataf_msa A)
{
  int result = 1;
  sataf_msa p1 = s_msa_power(A,3);
  sataf_msa p2 = sataf_msa_power(A,3);
      
  result = (p1 == p2);

  if( ! result )
    ccl_warning("ERR %d.%d: wrong power(a,3)\n",Z,t);

  sataf_msa_del_reference(p1);
  sataf_msa_del_reference(p2);

  return result;
}

			/* --------------- */

static int s_t21(int t, sataf_msa A)
{
  int i, N = 3;
  int result = 1;
  sataf_msa astar = sataf_msa_star(A);

  for(i = 0; result && i <= N; i++)
    {
      sataf_msa pow_a_i = sataf_msa_power(A,i);

      result = sataf_msa_is_zero(A) || sataf_msa_is_included_in(pow_a_i,astar);
      
      if( ! result )
	ccl_warning("ERR %d.%d: power(a,%d) ! included in a*\n",Z,t,i);
      
      sataf_msa_del_reference(pow_a_i);
    }

  sataf_msa_del_reference(astar);

  return result;
}

			/* --------------- */

static int s_test(uint32_t size, uint32_t alphabet_size)
{
  sataf_automaton  A, B, C;
  sataf_msa SA, SB, SC;
  int res;
  int i, t = 0;
  sataf_msa T[3];

  Z++;

  C  = random_automaton_create(size,alphabet_size);
  A  = random_automaton_create(size,alphabet_size);
  B  = random_automaton_create(size,alphabet_size);

  T[0] = SA = sataf_msa_compute(A);
  T[1] = SB = sataf_msa_compute(B);
  T[2] = SC = sataf_msa_compute(C);

  for(i = 0; i < 1; i++)
    {
  res = s_apply_t0(&t,size,alphabet_size,s_t15); /* 0 == 0 */
  res = res && s_apply_t0(&t,size,alphabet_size,s_t16); /* 1 == 1 */
  res = res && s_apply_t0(&t,size,alphabet_size,s_t17); /* 1 != 0 */
    
  res = res && s_apply_t1(&t,T,3,s_t01); /* SA <=> SSA */
  res = res && s_apply_t1(&t,T,3,s_t02); /* (a && ! a) == 0 */
  res = res && s_apply_t1(&t,T,3,s_t03); /* (a && a) == a */

  res = res && s_apply_t1(&t,T,3,s_t18); /* epsilon.a == a && a.epsilon == a */
  res = res && s_apply_t1(&t,T,3,s_t19); /* {}.a == a && a.{} == a */
  res = res && s_apply_t1(&t,T,3,s_t20); /* power(a,10) */
  res = res && s_apply_t1(&t,T,3,s_t21); /* power(a,i) inc a* */

  res = res && s_apply_t2(&t,T,3,s_t04); /* (a && b) == (b && a) */
  res = res && s_apply_t2(&t,T,3,s_t06); /* (a && b) && !a == 0 */
  res = res && s_apply_t2(&t,T,3,s_t07); /* (a && b) && a == a && b */
  res = res && s_apply_t2(&t,T,3,s_t09); /* (a <=> b) == (a=>b && b=>a) */
  res = res && s_apply_t2(&t,T,3,s_t11); /* (a || b) == !(!a && !b) */
  res = res && s_apply_t2(&t,T,3,s_t12); /* (a && b) == !(!a || !b) */
  res = res && s_apply_t2(&t,T,3,s_t13); /* (a => b) == !a || b */
  res = res && s_apply_t2(&t,T,3,s_t14); /* (a => b) == !b => !a */

  res = res && s_apply_t3(&t,T,3,s_t05); /* (a && b) && c == a && (b && c) */
  res = res && s_apply_t3(&t,T,3,s_t08); /* (a && (b || c)) = (a && b) || (a && c) */
  res = res && s_apply_t3(&t,T,3,s_t10); /* (a || (b && c)) = (a || b) && (a || c) */
    }

  sataf_automaton_del_reference(A);
  sataf_automaton_del_reference(B);
  sataf_automaton_del_reference(C);
  sataf_msa_del_reference(SA);
  sataf_msa_del_reference(SB);
  sataf_msa_del_reference(SC);

  return res;
}
