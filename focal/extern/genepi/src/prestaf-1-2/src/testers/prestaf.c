/* $Id: prestaf.c,v 1.2 2006/01/04 08:21:47 point Exp $ */
#include <stdio.h>
#include <ccl-init.h>
#include <sataf.h>
#include "prestaf-parser.h"
#include "prestaf-interp.h"

			/* --------------- */

static void s_init(void);
static void s_terminate(void);

static void s_test(void);
static void s_usage(const char *cmd);
static void s_show_vars(ccl_list l);
static void s_parse_args(int argc, char **argv);

			/* --------------- */

static char   *filename = NULL;
static uint32_t flags = 0;

#define F_FLAG(i) (1<<(i))
#define F_PRETEST      F_FLAG(0)
#define F_DISP_FORMULA F_FLAG(1)
#define F_DOT          F_FLAG(2)
#define F_VARS         F_FLAG(3)
#define F_FORMULA_STAT F_FLAG(4)
#define F_SOLUTIONS    F_FLAG(5)
#define F_HELP         F_FLAG(6)

			/* --------------- */

int main(int argc, char **argv)
{
  int result = 0;

  s_init();


  ccl_try(exception)
    {
      ccl_pair p;
      ccl_parse_tree T;
      ccl_list formulas;

      s_parse_args(argc,argv);
      if( flags & F_HELP )
	{
	  s_usage(argv[0]);
	}
      else
	{

	  if( flags & F_PRETEST ) 
	    s_test();

	  if( filename != NULL )
	    {
	      T = prestaf_load_file(filename);
	      formulas = prestaf_interp(T);
	  
	      ccl_parse_tree_delete_container(T);      
	  
	      for(p = FIRST(formulas); p; p = CDR(p))
		{
		  prestaf_formula F = (prestaf_formula)CAR(p);
		  prestaf_predicate P;
		  
		  if( flags & F_DISP_FORMULA ) 
		    {
		      prestaf_formula_display(F);
		      ccl_display("\n");
		    }
		  
		  P = prestaf_formula_solutions(F);
		  if( flags & F_VARS )  
		    s_show_vars(prestaf_predicate_get_variables(P));
		  
		  if( flags & F_DOT )
		    {
		      sataf_msa rel = prestaf_predicate_get_relation(P);
		      sataf_msa_display_as_olddot(rel,CCL_LOG_DISPLAY,NULL);
		      sataf_msa_del_reference(rel);
		    }
		  
		  if( flags & F_SOLUTIONS )
		    {
		      if( ! prestaf_predicate_display_vectors(P) )
			ccl_warning("infinite solution set.\n");
		    }
		  if( flags & F_FORMULA_STAT )
		    prestaf_formula_statistics(F,CCL_LOG_DISPLAY);
	      
		  prestaf_predicate_del_reference(P);
		}
	      for(p = FIRST(formulas); p; p = CDR(p))
		prestaf_formula_del_reference(CAR(p));
	      ccl_list_delete(formulas);
	    }
	}
    }
  ccl_catch
    {
      ccl_exception_print();
    }
  ccl_end_try;


  s_terminate();

  return result;
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

static void
s_init(void)
{
  ccl_init(0);
  sataf_init();
  ccl_log_add_listener(s_listener,NULL);
}

			/* --------------- */

static void
s_terminate(void)
{
  sataf_terminate();
  ccl_terminate();
}

			/* --------------- */

static void
s_test(void)
{
  uint32_t a1_init   = 1;
  uint32_t a2_init   = 2;
  uint8_t a1_is_final[] = { 1, 0, 0, 1, 1 };
  uint8_t a2_is_final[] = { 0, 1, 0, 0, 0 };
  uint32_t a1_succ[] = { 4, 4, 2, 3, 1, 1, 4, 1, 3, 0 };  
  uint32_t a2_succ[] = { 4, 4, 2, 2, 3, 1, 2, 4, 0, 3 };

  sataf_automaton A2 = sataf_automaton_create_from_arrays(5,2,a1_init,
							  a1_is_final,a1_succ);
  sataf_automaton A1 = sataf_automaton_create_from_arrays(5,2,a2_init,
							  a2_is_final,a2_succ);

  {
    sataf_msa a1 = sataf_msa_compute(A1);
    sataf_msa a2 = sataf_msa_compute(A2);
    sataf_msa notA2 = sataf_msa_not(a2);
    sataf_msa A1capnotA2 = sataf_msa_and(a1,notA2);

    sataf_msa_display_as_dot(A1capnotA2,1,NULL);
    
    sataf_msa_del_reference(notA2);
    sataf_msa_del_reference(A1capnotA2);
    sataf_msa_del_reference(a1);
    sataf_msa_del_reference(a2);
  }
  sataf_automaton_del_reference(A1);
  sataf_automaton_del_reference(A2);
}

			/* --------------- */

static void
s_show_vars(ccl_list l)
{
  ccl_pair p;

  for(p = FIRST(l); p; p = CDR(p))
    {
      ccl_log(CCL_LOG_DISPLAY,"%s ",CAR(p));
    }
  ccl_log(CCL_LOG_DISPLAY,"\n");
}

			/* --------------- */

static void 
s_usage(const char *cmd)
{
  ccl_log(CCL_LOG_ERROR,"USAGE:%s [options] input-filename\nwith options:\n",
	  cmd);
  ccl_log(CCL_LOG_ERROR,"-p : run a pre-test\n");
  ccl_log(CCL_LOG_ERROR,"-f : display formulas\n");
  ccl_log(CCL_LOG_ERROR,"-d : display solutions in dot file format\n");
  ccl_log(CCL_LOG_ERROR,"-v : display the list of variables for each "
	  "formula\n");
  ccl_log(CCL_LOG_ERROR,"-s : display solutions\n");
  ccl_log(CCL_LOG_ERROR,"-m : display stats about formulas\n");
  ccl_log(CCL_LOG_ERROR,"-h : this help message\n");
}

			/* --------------- */

static void
s_parse_args(int argc, char **argv)
{
  int i = 0;

  for(i = 1; i < argc; i++)
    {
      if( argv[i][0] == '-' )
	{
	  char *s;

	  for(s = argv[i]+1; *s; s++)
	    {
	      switch( *s ) {
	      case 'h': flags |= F_HELP; break;
	      case 'p': flags |= F_PRETEST; break;
	      case 'f': flags |= F_DISP_FORMULA; break;
	      case 'd': flags |= F_DOT; break;
	      case 'v': flags |= F_VARS; break;
	      case 's': flags |= F_SOLUTIONS; break;
	      case 'm': flags |= F_FORMULA_STAT; break;
	      default : ccl_throw(exception,"unknown option\n");
	      };
	    }
	}
      else
	filename = argv[i];
    }
  if( filename == NULL && ! (flags&(F_HELP|F_PRETEST)) )
    ccl_throw(exception,"no input file\n");
}

			/* --------------- */

