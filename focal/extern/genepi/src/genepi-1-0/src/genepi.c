/**
 * GENEPI is the GENEric Presburger programming Interface.
 *
 * Copyright (C) 2006 Jerome Leroux (coordinator), Sebastien Bardin, 
 * Gerald Point and LaBRI, CNRS UMR 5800, Universite Bordeaux 1, ENSEIRB.
 *
 * GENEPI is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2, or (at your option) any later version.
 *
 * GENEPI  is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * GENEPI; see the file COPYING.  If not, write to the Free Software 
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* $Id: genepi.c,v 1.1 2006/03/17 11:11:33 point Exp $ */
#include <time.h>
#include <stdlib.h>
#include <assert.h>
#include "genepi.h"

			/* --------------- */
enum {
  T_LINEAR_EQUALITY  = 0,
  T_UNION,
  T_INTERSECTION,
  T_COMPLEMENT,
  T_PROJECT,
  T_INVPROJECT,
  T_APPLY,
  T_APPLYINV,
  T_IS_EMPTY,
  T_IS_FULL,
  T_IS_INCLUDED_IN,
  T_TOP,
  T_BOT,
  T_EQUAL,
  T_DS_SIZE,
  T_TOTAL_TIME,
  NB_TIMERS
};

typedef struct timer_st {
  const char *name;
  clock_t chronometer;
  unsigned long number_of_calls;
  unsigned long cumulated_time;
} timer;


static timer TIMERS[NB_TIMERS];
static const genepi_engine *GENEPI_ENGINE = NULL;

# define START_TIMER(_t) \
  do { (_t)->number_of_calls++; (_t)->chronometer = clock(); } while(0)

# define END_TIMER(_t) \
  do { (_t)->cumulated_time += clock()-(_t)->chronometer; } while(0)

# define TIMED_STMT(_stmt) \
  do { START_TIMER(T); _stmt; END_TIMER(T); } while(0) 

# define INIT_TIMER(_timer) \
  do { \
    TIMERS[T_ ## _timer].name = # _timer; \
    TIMERS[T_ ## _timer].chronometer = 0; \
    TIMERS[T_ ## _timer].number_of_calls = 0; \
    TIMERS[T_ ## _timer].cumulated_time = 0; \
 } while(0)

# define DECLARE_TIMER(_t)   static timer *T = &TIMERS[T_ ## _t]

			/* --------------- */

static genepi_set *
s_generic_apply(genepi_set *R, genepi_set *A);

static genepi_set *
s_generic_applyinv(genepi_set *R, genepi_set *A);

			/* --------------- */

void
genepi_set_init(void)
{
  if( GENEPI_ENGINE->init )
    GENEPI_ENGINE->init();

  INIT_TIMER(LINEAR_EQUALITY);
  INIT_TIMER(UNION);
  INIT_TIMER(INTERSECTION);
  INIT_TIMER(COMPLEMENT);
  INIT_TIMER(PROJECT);
  INIT_TIMER(INVPROJECT);
  INIT_TIMER(APPLY);
  INIT_TIMER(APPLYINV);
  INIT_TIMER(IS_EMPTY);
  INIT_TIMER(IS_FULL);
  INIT_TIMER(IS_INCLUDED_IN);
  INIT_TIMER(TOP);
  INIT_TIMER(BOT);
  INIT_TIMER(EQUAL);
  INIT_TIMER(DS_SIZE);
  INIT_TIMER(TOTAL_TIME);

  START_TIMER(&TIMERS[T_TOTAL_TIME]);
}

			/* --------------- */

static int
s_cmp_timers(const void *t1, const void *t2)
{
  timer *T1 = (timer *)t1;
  timer *T2 = (timer *)t2;

  if( T1->cumulated_time < T2->cumulated_time ) return 1;
  if( T1->cumulated_time > T2->cumulated_time ) return -1;
  return T2->number_of_calls-T1->number_of_calls;
}

			/* --------------- */

void
genepi_set_terminate(int flags)
{
  END_TIMER(&TIMERS[T_TOTAL_TIME]);

  if( GENEPI_ENGINE->terminate )
    GENEPI_ENGINE->terminate(GENEPI_SHOW_STATISTICS&flags);

  if( (GENEPI_SHOW_TIMERS&flags) != 0 )
    {
      int i;

      qsort(TIMERS,NB_TIMERS,sizeof(timer),s_cmp_timers);
      printf("\n");
      printf("ENGINE = %s\n",GENEPI_ENGINE->name);
      printf("%15s %10s %10s %13s\n","Operation","# calls", "s", "ms/c");
      printf("----------------------------------------------------------------------\n");
      for(i = 0; i < NB_TIMERS; i++)
	{
	  if( TIMERS[i].number_of_calls )
	    {
	      double s = (double)TIMERS[i].cumulated_time/CLOCKS_PER_SEC;
	      double uspc = 1000.0*s/TIMERS[i].number_of_calls;

	      printf("%15s %10ld % 10.3f % 13.3f\n",
		     TIMERS[i].name,
		     TIMERS[i].number_of_calls,
		     s,
		     uspc);		
	    }
	  else
	    {
	      printf("%15s is never called\n",TIMERS[i].name);
	    }
	}
      printf("----------------------------------------------------------------------\n");
      printf("\n");
    }
}

			/* --------------- */

genepi_set *
genepi_set_add_reference(genepi_set *X)
{
  if( GENEPI_ENGINE->add_reference )
    return GENEPI_ENGINE->add_reference(X);
  return X;
}

			/* --------------- */

void
genepi_set_del_reference(genepi_set *X)
{
  if( GENEPI_ENGINE->del_reference )
    GENEPI_ENGINE->del_reference(X);
}

			/* --------------- */

genepi_set *
genepi_set_linear_equality(int *alpha, int alpha_size, int cst)
{
  DECLARE_TIMER(LINEAR_EQUALITY);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->linear_equality(alpha,alpha_size,cst));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_union(genepi_set *X1, genepi_set *X2)
{
  DECLARE_TIMER(UNION);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->set_union(X1,X2));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_intersection(genepi_set *X1, genepi_set *X2)
{
  DECLARE_TIMER(INTERSECTION);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->set_intersection(X1,X2));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_complement(genepi_set *X)
{
  DECLARE_TIMER(COMPLEMENT);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->set_complement(X));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_project(genepi_set *X, int *sel, int selsize)
{
  DECLARE_TIMER(PROJECT);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->project(X,sel,selsize));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_invproject(genepi_set *X, int *sel, int selsize)
{
  DECLARE_TIMER(INVPROJECT);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->invproject(X,sel,selsize));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_apply(genepi_set *R, genepi_set *A)
{
  DECLARE_TIMER(APPLY);
  genepi_set *result;

  if( GENEPI_ENGINE->apply == NULL )
    TIMED_STMT(result = s_generic_apply(R,A));
  else
    TIMED_STMT(result = GENEPI_ENGINE->apply(R,A));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_applyinv(genepi_set *R, genepi_set *A)
{
  DECLARE_TIMER(APPLYINV);
  genepi_set *result;

  if( GENEPI_ENGINE->apply == NULL )
    TIMED_STMT(result = s_generic_applyinv(R,A));
  else
    TIMED_STMT(result = GENEPI_ENGINE->applyinv(R,A));

  return result;
}

			/* --------------- */

int
genepi_set_is_empty(genepi_set *X)
{
  DECLARE_TIMER(IS_EMPTY);
  int result;

  TIMED_STMT(result = GENEPI_ENGINE->is_empty(X));

  return result;
}

			/* --------------- */

int 
genepi_set_is_full(genepi_set *X)
{
  DECLARE_TIMER(IS_FULL);
  int result;

  TIMED_STMT(result = GENEPI_ENGINE->is_full(X));

  return result;
}

			/* --------------- */

int
genepi_set_is_included_in(genepi_set *X1, genepi_set *X2)
{
  DECLARE_TIMER(IS_INCLUDED_IN);
  int result;

  TIMED_STMT(result = GENEPI_ENGINE->is_included_in(X1,X2));

  return result;
}

			/* --------------- */

int 
genepi_set_is_finite(genepi_set *X)
{
  return GENEPI_ENGINE->is_finite(X);
}

			/* --------------- */

void
genepi_set_get_solutions(genepi_set *X, int ***psolutionss, int *psize, int max)
{
  if( GENEPI_ENGINE->get_solutions != NULL )
    GENEPI_ENGINE->get_solutions(X,psolutionss,psize,max);
}

			/* --------------- */

void
genepi_set_display_all_solutions(genepi_set *X, FILE *output)
{
  if( GENEPI_ENGINE->display_all_solutions != NULL )
  GENEPI_ENGINE->display_all_solutions(X,output);
}

			/* --------------- */

void
genepi_set_display_data_structure(genepi_set *X, FILE *output)
{
  if( GENEPI_ENGINE->display_data_structure != NULL )
    GENEPI_ENGINE->display_data_structure(X,output);
}

			/* --------------- */

int
genepi_set_get_width(genepi_set *X)
{
  return GENEPI_ENGINE->get_width(X);
}

			/* --------------- */

int
genepi_set_get_data_structure_size(genepi_set *X)
{
  DECLARE_TIMER(DS_SIZE);
  int result;

  TIMED_STMT(result = GENEPI_ENGINE->get_data_structure_size(X));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_top(int width)
{
  DECLARE_TIMER(TOP);
  genepi_set *result;

  TIMED_STMT(result = GENEPI_ENGINE->top(width));

  return result;
}

			/* --------------- */

genepi_set *
genepi_set_bot(int width)
{
  DECLARE_TIMER(BOT);
  genepi_set *result;


  TIMED_STMT(result = GENEPI_ENGINE->bot(width));
  
  return result;
}

			/* --------------- */

int
genepi_set_equal(genepi_set *X1, genepi_set *X2)
{
  DECLARE_TIMER(EQUAL);
  int result;

  TIMED_STMT(result = GENEPI_ENGINE->equal(X1,X2));

  return result;
}


		/* --------------- */

static genepi_set *
s_x_is_even(void)
{
  int coefs[2] = { 1, -2 };
  genepi_set *a = genepi_set_linear_equality(coefs,2,0);
  int sel[2] = { 0, 1 };
  genepi_set *R = genepi_set_project(a,sel,2);

  genepi_set_del_reference(a);

  return R;
}

			/* --------------- */

static genepi_set *
s_x_is_even_without_0(void)
{
  int coefs[2] = { 1, -2 };
  genepi_set *a = genepi_set_linear_equality(coefs,2,2);
  int sel[2] = { 0, 1 };
  genepi_set *R = genepi_set_project(a,sel,2);

  genepi_set_del_reference(a);

  return R;
}

			/* --------------- */

/* xp-x = 2 */
static genepi_set *
s_x_p_equals_x_plus_2(void)
{
  int coefs[2] = { 1, -1 };

  return genepi_set_linear_equality(coefs,2,2);
}

			/* --------------- */

/* xp-x = -n */
static genepi_set *
s_x_p_equals_x_minus_n(int n)
{
  int coefs[2] = { 1, -1 };

  return genepi_set_linear_equality(coefs,2,-n);
}

			/* --------------- */

static genepi_set *
s_x_equals_n(int n)
{
  int coefs[1] = { 1 };

  return genepi_set_linear_equality(coefs,1,n);
}

			/* --------------- */

static int
s_check_apply_vs_gen_apply(genepi_set *R, genepi_set *A, genepi_set *Res)
{
  genepi_set *R0 = s_generic_apply(R,A);
  genepi_set *R1 = genepi_set_apply(R,A);
  int result = genepi_set_equal(R0,Res) ;
  result = result && genepi_set_equal(R0,R1);
  genepi_set_del_reference(R1);
  genepi_set_del_reference(R0);

  return result;
}

			/* --------------- */

int
genepi_autotest(void)
{
  int result = 1;
  genepi_set *x_eq_2 = s_x_equals_n(2);
  genepi_set *x_eq_0 = s_x_equals_n(0);
  genepi_set *xp_eq_x_plus_2 = s_x_p_equals_x_plus_2();
  genepi_set *x_is_even = s_x_is_even();
  genepi_set *x_is_even_without_0 = s_x_is_even_without_0();
  genepi_set *x_in_nat = genepi_set_top(1);
  genepi_set *xp_eq_x_minus_1 = s_x_p_equals_x_minus_n(1);
  genepi_set *x_neq_0 = genepi_set_complement(x_eq_0);
  genepi_set *xp_eq_x_minus_4 = s_x_p_equals_x_minus_n(4);

  result = result && s_check_apply_vs_gen_apply(xp_eq_x_plus_2,x_eq_0,x_eq_2);
  result = result && s_check_apply_vs_gen_apply(xp_eq_x_plus_2,x_is_even,
						x_is_even_without_0);
  result = result && s_check_apply_vs_gen_apply(xp_eq_x_minus_1,x_in_nat,
						x_in_nat);

  result = result && s_check_apply_vs_gen_apply(xp_eq_x_minus_1,x_neq_0,
						x_in_nat);
  result = result && s_check_apply_vs_gen_apply(xp_eq_x_minus_4,x_in_nat,
						x_in_nat);

  genepi_set_del_reference(x_is_even_without_0);
  genepi_set_del_reference(x_is_even);
  genepi_set_del_reference(xp_eq_x_plus_2);
  genepi_set_del_reference(x_eq_0);
  genepi_set_del_reference(x_neq_0);
  genepi_set_del_reference(x_eq_2);
  genepi_set_del_reference(x_in_nat);
  genepi_set_del_reference(xp_eq_x_minus_1);
  genepi_set_del_reference(xp_eq_x_minus_4);

  return result;
}

			/* --------------- */

void
genepi_set_engine(const genepi_engine *engine)
{
  GENEPI_ENGINE = engine;
}

			/* --------------- */

const genepi_engine *
genepi_get_engine(void)
{
  return GENEPI_ENGINE;
}

			/* --------------- */

static genepi_set *
s_generic_apply(genepi_set *R, genepi_set *A)
{
  int i;
  genepi_set *X0, *X1, *X2;
  int m = genepi_set_get_width(A);
  int *selection = (int *)calloc(sizeof(int),2*m);

  for(i = 0; i < m; i++)
    {
      selection[2*i] = 1;
      selection[2*i+1] = 0;
    }
  
  X0 = genepi_set_invproject(A,selection,2*m);
  X1 = genepi_set_intersection(R,X0);
  genepi_set_del_reference(X0);

  for(i = 0; i < m; i++)
    {
      selection[2*i] = 0;
      selection[2*i+1] = 1;
    }

  X2 = genepi_set_project(X1,selection,2*m);
  genepi_set_del_reference(X1);
  free(selection);

  return X2;
}

			/* --------------- */

static genepi_set *
s_generic_applyinv(genepi_set *R, genepi_set *A)
{
  int i;
  genepi_set *X0, *X1, *X2;
  int m = genepi_set_get_width(A);
  int *selection = (int *)calloc(sizeof(int),2*m);

  for(i = 0; i < m; i++)
    {
      selection[2*i] = 0;
      selection[2*i+1] = 1;
    }

  X0 = genepi_set_invproject(A,selection,2*m);
  X1 = genepi_set_intersection(R,X0);
  genepi_set_del_reference(X0);

  for(i = 0; i < m; i++)
    {
      selection[2*i] = 1;
      selection[2*i+1] = 0;
    }
  X2 = genepi_set_project(X1,selection,2*m);
  genepi_set_del_reference(X1);

  free(selection);

  return X2; 
}
