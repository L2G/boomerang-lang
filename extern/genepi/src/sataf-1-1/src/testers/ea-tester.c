/* $Id: ea-tester.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <time.h>
#ifndef WIN32
# include <sys/times.h>
#endif /* WIN32 */
#include <stdio.h>
#include <ccl-assert.h>
#include <ccl-init.h>
#include <sataf.h>
#include <sataf-msa.h>
#include "random-automaton.h"
#include "rnd.h"

# define MIN_S_SIZE 1000
# define MAX_S_SIZE 100000
# define S_STEP 100

# define MIN_A_SIZE 100
# define MAX_A_SIZE 300
# define A_STEP     100

# define NB_TEST    1

static void s_init(void);
static void s_terminate(void);

static void
s_test(uint32_t size, uint32_t alphabet_size, clock_t *m, clock_t *mr);
static void
s_test2(uint32_t size, uint32_t alphabet_size, clock_t *m, clock_t *mr);

			/* --------------- */

int main(int argc, char **argv)
{
  s_init();
  ccl_try(exception)
    {
      FILE *out = fopen("result.tab","w");
      uint32_t i,j,k;
      
      for(i = MIN_S_SIZE; i <= MAX_S_SIZE ; i+=S_STEP)
	{
	  for(j = MIN_A_SIZE; j <= MAX_A_SIZE ; j+=A_STEP)
	    {
	      clock_t mean     = 0;
	      clock_t mean_ref = 0;
	      
	      for(k=0; k < NB_TEST; k++)
		{
		  clock_t m, mr;
		  s_test2(i,j,&m,&mr);
		  mean += m;
		  mean_ref += mr;
		}

	      mean /= NB_TEST;
	      mean_ref /= NB_TEST;

	      fprintf(out,"%d %d %ld\n",i,j,mean/1000);
	      fflush(out);
	      if(0) fprintf(stderr,"%d %d %ld\n",i,j,mean_ref/1000);
	    }
	  fprintf(out,"\n");
	}
      fflush(out);
      fclose(out);
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

static void s_terminate(void)
{
  sataf_terminate();
  ccl_terminate();
}

			/* --------------- */

extern exit_automaton
exit_automaton_ref_minimize(exit_automaton ea, uint32_t *ph);

static void
s_test(uint32_t size, uint32_t alphabet_size, clock_t *m, clock_t *mr)
{
  uint32_t *h;
  exit_automaton ea, min_ea;
  clock_t start;
  
  ea = random_exit_automaton_create(size,alphabet_size);
  h = ccl_new_array(uint32_t,size);
  start = clock();
  min_ea = exit_automaton_minimize(ea,h);
  *m = clock()-start;

  if( 0 ) {
    uint32_t *h2= ccl_new_array(uint32_t,size);
    exit_automaton min_ea2 = exit_automaton_ref_minimize(ea,h2);
    start = clock();

    *mr = clock()-start;
    ccl_assert( min_ea2->nb_local_states == min_ea->nb_local_states );
    ccl_delete(h2);
    exit_automaton_del_reference(min_ea2);
  }
  ccl_delete(h);
  exit_automaton_del_reference(ea);
  exit_automaton_del_reference(min_ea);
}

			/* --------------- */

static void
s_test2(uint32_t size, uint32_t alphabet_size, clock_t *m, clock_t *mr)
{
  uint32_t i, a;
  uint32_t *h;
  exit_automaton ea, min_ea;
  clock_t start;
  uint32_t l = rnd_modulo(alphabet_size);
  
  ea = exit_automaton_create(size,0,alphabet_size);
  ea->is_final[size-1] = 1;
  for(i = 0; i < size; i++)
    {
      for(a = 0; a < alphabet_size; a++)
	{
	  if( a == l ) 
	    {
	      if( i+1 == size ) exit_automaton_set_successor(ea,i,a,i,0);
	      else              exit_automaton_set_successor(ea,i,a,i+1,0);
	    }
	  else		
	    exit_automaton_set_successor(ea,i,a,size-1,0);
	}
    }
  start = clock();
  h = ccl_new_array(uint32_t,size);
  min_ea = exit_automaton_minimize(ea,h);
  *m = clock()-start;

  if( 0 ) {
    uint32_t *h2= ccl_new_array(uint32_t,size);
    exit_automaton min_ea2 = exit_automaton_ref_minimize(ea,h2);
    start = clock();
    *mr = clock()-start;
    ccl_assert( min_ea2->nb_local_states == min_ea->nb_local_states );
    ccl_delete(h2);
    exit_automaton_del_reference(min_ea2);
  }
  ccl_delete(h);
  exit_automaton_del_reference(ea);
  exit_automaton_del_reference(min_ea);
}
