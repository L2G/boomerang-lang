/**
 * FAST Enhanced Release, an accelerated symbolic model-checker. 
 * Copyright (C) 2005-2006 Jerome Leroux (coordinator), Sebastien Bardin, 
 * Gerald Point and LaBRI, CNRS UMR 5800, Universite Bordeaux 1, ENSEIRB.
 *
 * FAST is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2, or (at your option) any later version.
 *
 * FAST  is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 *
 * FAST; see the file COPYING.  If not, write to the Free Software Foundation, 
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <stdio.h>
#include <ccl-assert.h>
#include <ccl-hash.h>
#include <ccl-init.h>
#include <ccl-log.h>
#include <sataf.h>
#include <sataf-msa-p.h>
#include <add-variable-automaton.h>
#include <quantifier-automaton.h>
#include <apply-automaton.h>
#include "prestaf2fast-cache.h"
#include "prestaf2fast.h"

#define USE_GENERIC_APPLY
#define CACHE_SIZE_INDEX 10

			/* --------------- */

struct genepi_set_st {
  int refcount;
  int width;
  sataf_msa msa;
};

			/* --------------- */

static genepi_set *
s_new_set(int width);
static void
s_log_listener(ccl_log_type type, const char *msg, void *data);
static sataf_automaton
s_crt_linear_equality(int *a, int asize, int factor_index, int value);
static sataf_automaton
s_crt_add_variable_set(sataf_msa msa, int n, int pos, int *sel, int selsize);
static int **
s_get_solutions(sataf_msa msa, int n, int *psize, int max);

			/* --------------- */

static int SIZE_FOR_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361,
   2729, 5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641,
   2807303, 5614657, 11229331
};

			/* --------------- */

static int use_logproc = 0;
static int init_counter = 0;


			/* --------------- */

static void
prestaf_genepi_set_init(void);
static void
prestaf_genepi_set_terminate(int show_statistics);
static genepi_set *
prestaf_genepi_set_add_reference(genepi_set *X);
static void
prestaf_genepi_set_del_reference(genepi_set *X);
static genepi_set *
prestaf_genepi_set_linear_equality(int *alpha, int alpha_size, int c);
static genepi_set *
prestaf_genepi_set_union(genepi_set *X1, genepi_set *X2);
static genepi_set *
prestaf_genepi_set_intersection(genepi_set *X1, genepi_set *X2);
static genepi_set *
prestaf_genepi_set_complement(genepi_set *X);
static genepi_set *
prestaf_genepi_set_project(genepi_set *X, int *sel, int selsize);
static genepi_set *
prestaf_genepi_set_invproject(genepi_set *X, int *sel, int selsize);
#ifdef USE_GENERIC_APPLY
# define prestaf_genepi_set_apply NULL
# define prestaf_genepi_set_applyinv NULL
#else
static genepi_set *
prestaf_genepi_set_apply(genepi_set *R, genepi_set *A);
static genepi_set *
prestaf_genepi_set_applyinv(genepi_set *R, genepi_set *A);
#endif
static int
prestaf_genepi_set_is_empty(genepi_set *X);
static int
prestaf_genepi_set_is_full(genepi_set *X);
static int
prestaf_genepi_set_is_included_in(genepi_set *X1, genepi_set *X2);
static int
prestaf_genepi_set_is_finite(genepi_set *X);
static void
prestaf_genepi_set_get_solutions(genepi_set *X, int ***psolutions, int *psize, 
			       int max);
static void
prestaf_genepi_set_display_all_solutions(genepi_set *X, FILE *output);
static void
prestaf_genepi_set_display_data_structure(genepi_set *X, FILE *output);
static int
prestaf_genepi_set_get_width(genepi_set *X);
static int
prestaf_genepi_set_get_data_structure_size(genepi_set *X);
static genepi_set *
prestaf_genepi_set_top(int n);
static genepi_set *
prestaf_genepi_set_bot(int n);
static int
prestaf_genepi_set_equal(genepi_set *X1, genepi_set *X2);

			/* --------------- */

static genepi_engine PRESTAF2FAST = {
  "prestaf",
  prestaf_genepi_set_init,
  prestaf_genepi_set_terminate,
  prestaf_genepi_set_add_reference,
  prestaf_genepi_set_del_reference,
  prestaf_genepi_set_linear_equality,
  prestaf_genepi_set_union,
  prestaf_genepi_set_intersection,
  prestaf_genepi_set_complement,
  prestaf_genepi_set_project,
  prestaf_genepi_set_invproject,
  prestaf_genepi_set_apply,
  prestaf_genepi_set_applyinv,
  prestaf_genepi_set_is_empty,
  prestaf_genepi_set_is_full,
  prestaf_genepi_set_is_included_in,
  prestaf_genepi_set_is_finite,
  prestaf_genepi_set_get_solutions,
  prestaf_genepi_set_display_all_solutions,
  prestaf_genepi_set_display_data_structure,
  prestaf_genepi_set_get_width,
  prestaf_genepi_set_get_data_structure_size,
  prestaf_genepi_set_top,
  prestaf_genepi_set_bot,
  prestaf_genepi_set_equal
};

			/* --------------- */

static void
prestaf_genepi_set_init(void)
{
  if( init_counter++ == 0 )
    {
      ccl_init(0);

      use_logproc = !ccl_log_has_listener();
      
      if( use_logproc )
	ccl_log_add_listener(s_log_listener,NULL);  
      
      sataf_init();

      genepi_set_cache_init(SIZE_FOR_TABLE[CACHE_SIZE_INDEX]);
    }
}

			/* --------------- */

static void
prestaf_genepi_set_terminate(int show_statistics)
{
  if( --init_counter == 0 )
    {
      if( show_statistics )
	genepi_set_cache_log_statistics(CCL_LOG_DISPLAY);
      genepi_set_cache_terminate();
      if( show_statistics )
	sataf_log_statistics(CCL_LOG_DISPLAY);
      sataf_terminate();
      if( use_logproc )
	ccl_log_remove_listener(s_log_listener);
      ccl_terminate();
    }
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_add_reference(genepi_set *X)
{
  ccl_pre( X != NULL );

  X->refcount++;

  return X;
}

			/* --------------- */

static void
prestaf_genepi_set_del_reference(genepi_set *X)
{
  ccl_pre( X != NULL );
  
  if( --X->refcount == 0 )
    {
      sataf_msa_del_reference(X->msa);
      ccl_delete(X);
    }
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_linear_equality(int *alpha, int alpha_size, int c)
{
  sataf_msa R;
  genepi_set *result;

  if( (R = genepi_set_cache_get_linear_equality(alpha_size,alpha,c)) == NULL )
    {
      sataf_automaton saut;
      int *aux = ccl_new_array(int,alpha_size+1);
      
      ccl_memcpy(aux+1,alpha,alpha_size*sizeof(int));
      saut = s_crt_linear_equality(aux+1,alpha_size,0,-c);
      R = sataf_msa_compute(saut);
      genepi_set_cache_put_linear_equality(alpha_size,aux+1,c,R);
      sataf_automaton_del_reference(saut);
    }
  result = s_new_set(alpha_size);
  result->msa = R;

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_union(genepi_set *X1, genepi_set *X2)
{
  genepi_set *result = s_new_set(X1->width);

  ccl_pre( prestaf_genepi_set_get_width(X1) == prestaf_genepi_set_get_width(X2) );

  result->msa = sataf_msa_or(X1->msa,X2->msa);

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_intersection(genepi_set *X1, genepi_set *X2)
{
  genepi_set *result = s_new_set(X1->width);

  ccl_pre( prestaf_genepi_set_get_width(X1) == prestaf_genepi_set_get_width(X2) );

  result->msa = sataf_msa_and(X1->msa,X2->msa);

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_complement(genepi_set *X)
{
  genepi_set *result = s_new_set(X->width);

  ccl_pre( X != NULL );

  result->msa = sataf_msa_not(X->msa);

  return result;
}

			/* --------------- */

static sataf_msa
s_compute_exists(sataf_msa msa, uint32_t i, uint32_t n)
{
  uint32_t e = ((i<<16)|(0x0000FFFF&n))+1;
  sataf_msa R = sataf_msa_cache_get(msa,NULL,NULL,(void *)e);

  ccl_assert( i < 0xFFFF );
  ccl_assert( n < 0xFFFF );

  ccl_assert( e > 2 );

  if( R == NULL )
    {
      sataf_automaton aux = quantifier_automaton_create(0,i,n,msa);
      R = sataf_msa_compute_with_transformer(aux,SSA_EXISTS);
      sataf_automaton_del_reference(aux);
      sataf_msa_cache_put(msa,NULL,NULL,(void *)e,R);
    }

  return R;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_project(genepi_set *X, int *sel, int selsize)
{
  int i, nb_T;
  genepi_set *result;

  for(i = 0, nb_T = 0; i < X->width; i++)
    {
      if( sel[i] )
	nb_T++;
    }

  if( nb_T == 0 )
    return prestaf_genepi_set_add_reference(X);
  else if( nb_T == X->width )
    {
      if( sataf_msa_is_zero(X->msa) )
	return prestaf_genepi_set_bot(0);
      else
	return prestaf_genepi_set_top(1);
    }
  
  result = s_new_set(X->width-nb_T);
  result->msa = genepi_set_cache_get_project(X->msa,X->width,selsize,sel);

  if( result->msa == NULL )
    {
      int w = X->width;
      int *newsel = ccl_new_array(int,X->width);
      sataf_msa msa = sataf_msa_add_reference(X->msa);

      for(i = 0; i < X->width; i++)
	newsel[i] = sel[i];
      
      while( w > 0 )
	{
	  sataf_msa aux;
	  for(i = 0; i < w && ! newsel[i]; i++)
	    /* do nothing */;
	  
	  if( i == w )
	    break;
	  aux = s_compute_exists(msa,i,w);
	  sataf_msa_del_reference(msa);
	  msa = aux;
	  
	  for(i++; i < w; i++)
	    newsel[i-1] = newsel[i];
	  
	  w--;
	}
      ccl_delete(newsel);
	  
      result->msa = msa;
      genepi_set_cache_put_project(X->msa,X->width,selsize,sel,result->msa);
    }

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_invproject(genepi_set *X, int *sel, int selsize)
{
  genepi_set *result = s_new_set(selsize);
  result->msa = genepi_set_cache_get_invproject(X->msa,X->width,selsize,sel);

  if( result->msa == NULL )
    {
      sataf_automaton aux = 
	s_crt_add_variable_set(X->msa,X->width,0,sel,selsize);

      ccl_pre( X->width <= selsize );
      
      result->msa = sataf_msa_compute(aux);
      sataf_automaton_del_reference(aux);

      genepi_set_cache_put_invproject(X->msa,X->width,selsize,sel,result->msa);
    }

  return result;
}

			/* --------------- */
#ifndef USE_GENERIC_APPLY
static genepi_set *
prestaf_genepi_set_apply(genepi_set *R, genepi_set *A)
{
  genepi_set *result = s_new_set(A->width);

  ccl_pre( R != NULL && A != NULL && R->width == 2*A->width );


  result->msa = genepi_set_cache_get_apply(R->msa,A->msa);
  if( result->msa == NULL )
    {
      sataf_automaton aux = apply_automaton_create(R->msa,A->msa);

      result->msa = sataf_msa_compute_with_transformer(aux,SSA_EXISTS);
      
      sataf_automaton_del_reference(aux);
      genepi_set_cache_put_apply(R->msa,A->msa,result->msa);
    }

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_applyinv(genepi_set *R, genepi_set *A)
{
  genepi_set *result = s_new_set(A->width);

  ccl_pre( R != NULL && A != NULL && R->width == 2*A->width );

  result->msa = genepi_set_cache_get_applyinv(R->msa,A->msa);
  if( result->msa == NULL )
    {
      sataf_automaton aux = apply_automaton_create_inverse(R->msa,A->msa);

      result->msa = sataf_msa_compute_with_transformer(aux,SSA_EXISTS);
      sataf_automaton_del_reference(aux);
      genepi_set_cache_put_applyinv(R->msa,A->msa,result->msa);
    }

  return result;
}
#endif
			/* --------------- */

static int
prestaf_genepi_set_is_empty(genepi_set *X)
{
  ccl_pre( X != NULL );

  return sataf_msa_is_zero(X->msa);
}

			/* --------------- */

static int
prestaf_genepi_set_is_full(genepi_set *X)
{
  ccl_pre( X != NULL );

  return sataf_msa_is_one(X->msa);
}

			/* --------------- */

static int
prestaf_genepi_set_is_included_in(genepi_set *X1, genepi_set *X2)
{
  ccl_pre( prestaf_genepi_set_get_width(X1) == prestaf_genepi_set_get_width(X2) );
  
  return sataf_msa_is_included_in(X1->msa,X2->msa);
}


			/* --------------- */

static int
prestaf_genepi_set_is_finite(genepi_set *X)
{
  ccl_warning(" 'genepi_set_is_finite' not yet implemented \n");
  return 0;
}

			/* --------------- */

static void
prestaf_genepi_set_get_solutions(genepi_set *X, int ***psolutions, int *psize, 
			       int max)
{
  ccl_pre( X != NULL );

  *psolutions = s_get_solutions(X->msa,X->width,psize,max);
}


			/* --------------- */

static void
prestaf_genepi_set_display_all_solutions(genepi_set *X, FILE *output)
{
  ccl_warning("'genepi_set_display_all_solutions' not yet implemented \n");
}

			/* --------------- */

static void
s_log_to_file(ccl_log_type type, const char  *msg, void *data)
{
  fprintf((FILE *)data,"%s",msg);
}

			/* --------------- */

static void
prestaf_genepi_set_display_data_structure(genepi_set *X, FILE *output)
{  
  ccl_pre( X != NULL ); ccl_pre( output != NULL ); 

  ccl_log_redirect(CCL_LOG_DISPLAY,s_log_to_file,output);
  sataf_msa_display_as_dot(X->msa,CCL_LOG_DISPLAY,NULL);
  ccl_log_restore(CCL_LOG_DISPLAY);
}

			/* --------------- */

static int
prestaf_genepi_set_get_width(genepi_set *X)
{
  ccl_pre( X != NULL );

  return X->width;
}

			/* --------------- */

static int
prestaf_genepi_set_get_data_structure_size(genepi_set *X)
{
  sataf_msa_info info;

  ccl_pre( X != NULL );
  
  sataf_msa_get_info(X->msa,&info);

  return info.nb_states;
}


			/* --------------- */

static genepi_set *
prestaf_genepi_set_top(int n)
{
  genepi_set *result = s_new_set(n);
  result->msa = sataf_msa_one(2);

  return result;
}

			/* --------------- */

static genepi_set *
prestaf_genepi_set_bot(int n)
{
  genepi_set *result = s_new_set(n);
  result->msa = sataf_msa_zero(2);

  return result;
}

			/* --------------- */

static int
prestaf_genepi_set_equal(genepi_set *X1, genepi_set *X2)
{
  ccl_pre( prestaf_genepi_set_get_width(X1) == prestaf_genepi_set_get_width(X2) );

  return X1->msa == X2->msa;
}

			/* --------------- */

static void
s_log_listener(ccl_log_type type, const char *msg, void *data)
{
  if( type == CCL_LOG_DISPLAY )
    fprintf(stdout,"%s",msg);
  else
    fprintf(stderr,"%s",msg);
}

			/* --------------- */

static genepi_set *
s_new_set(int width)
{
  genepi_set *result = ccl_new(genepi_set);

  ccl_pre( width >= 0 );

  result->refcount = 1;
  result->width = width;
  result->msa = NULL;

  return result;
}

			/* --------------- */

typedef struct lca_st {
  struct sataf_automaton_st super;
  int width;
  int *alpha;
  int factor_index;
  int value;
} *lca;

			/* --------------- */

static void
s_lca_destroy(sataf_automaton self);

static uint32_t
s_lca_get_alphabet_size(sataf_automaton self);

static char *
s_lca_to_string(sataf_automaton self);

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
  "FAST-LCA",
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
s_crt_linear_equality(int *a, int asize, int factor_index, int value)
{
  lca R = (lca)sataf_automaton_create(sizeof(struct lca_st),&LCA_METHODS);

  R->width = asize;
  R->alpha = a;  
  a[-1]++;
  R->factor_index = factor_index;
  R->value = value;

  return (sataf_automaton)R;
}

			/* --------------- */

static void
s_lca_destroy(sataf_automaton self)
{
  lca a = (lca)self;
  
  if( --a->alpha[-1] == 0 )
    ccl_delete(&(a->alpha[-1]));
}

			/* --------------- */

static uint32_t
s_lca_get_alphabet_size(sataf_automaton self)
{
  return 2;
}

			/* --------------- */

static char *
s_lca_to_string(sataf_automaton self)
{
  int i;
  lca a = (lca)self;
  char *result = NULL;
  char **s = &result;
  int *coeff = a->alpha;

  for(i = 0; i < a->width; i++)
    {
      if( coeff[i] > 0 )
	{
	  char *sign;
	  if( i == 0 ) sign = "";
	  else         sign = "+";

	  if( coeff[i] == 1 ) 
	    ccl_string_format_append(s,"%sx%d",sign,i);
	  else  
	    ccl_string_format_append(s,"%s%d*x%d",sign,coeff[i],i);
	}
      else if( coeff[i] < 0 )
	{
	  if( coeff[i] == -1 ) 	  
	    ccl_string_format_append(s,"-x%d",i);
	  else 
	    ccl_string_format_append(s,"%d*x%d",coeff[i],i);
	}
    }
  ccl_string_format_append(s," = %d",a->value);

  return result;
}

			/* --------------- */

static sataf_automaton
s_lca_succ(sataf_automaton self, uint32_t letter)
{
  lca a = (lca)self;
  int32_t value = a->value;
  int factor_index = a->factor_index;
  sataf_automaton R;

  if( letter == 1 )
    value += a->alpha[factor_index];

  if( factor_index+1 < a->width )
    R = s_crt_linear_equality(a->alpha,a->width,factor_index+1,value);
  else if( (value % 2) == 0 )
    R = s_crt_linear_equality(a->alpha,a->width,0,value/2);
  else 
    R = sataf_automaton_create_zero(2);

  return R;
}

			/* --------------- */

static int
s_lca_is_final(sataf_automaton self)
{
  lca a = (lca)self;

  return (a->value == 0);
}

			/* --------------- */

static int
s_lca_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  lca a1 = (lca)self;
  lca a2 = (lca)other;

  return a1->width == a2->width 
    &&   a1->value  == a2->value 
    &&   a1->factor_index  == a2->factor_index
    &&   memcmp(a1->alpha,a2->alpha,sizeof(int)*a1->width) == 0;
}
			/* --------------- */


static uint32_t
s_lca_hashcode(sataf_automaton self)
{
  lca a = (lca)self;
  uint32_t i = a->width;
  uint32_t result = 1361*a->width+350899*(uint32_t)a->factor_index
    + 5614657*a->value;

  while( i-- )
    result = 21911*result+a->alpha[i]*SIZE_FOR_TABLE[i%a->width];

  return result;
}

			/* --------------- */


typedef struct display_stack_st {
  sataf_msa rel;
  int    letter;
} display_stack;

typedef struct prefix_tree_st prefix_tree;
struct prefix_tree_st {
  uint32_t val;
  prefix_tree *right;
  prefix_tree *down;
};

			/* --------------- */

static int
s_is_in_stack(display_stack *S, int top, sataf_msa node)
{
  while( top >= 0 )
    if( S[top--].rel == node )
      return 1;
  return 0;
}

			/* --------------- */

static prefix_tree *
s_add_assignment(int width, uint32_t *values, prefix_tree *T)
{
  int i;
  prefix_tree *result = T;
  prefix_tree **pT = &result;

  for(i = 0; i < width; i++, values++)
    {
      while( *pT != NULL && (*pT)->val < *values )
	  pT = &((*pT)->down);	
      
      if( *pT == NULL || (*pT)->val > *values )
	{
	  prefix_tree *t = ccl_new(prefix_tree);
	  
	  t->val   = *values;
	  t->right = NULL;
	  t->down  = *pT;

	  *pT = t;
	}
      pT = &((*pT)->right);
    }

  return result;
}

			/* --------------- */

struct dlist {
  int val;
  struct dlist *next;
};

			/* --------------- */

static void
s_assign_prefix_tree_rec(int var, int width, prefix_tree *T, struct dlist *dl,
			 ccl_list solutions, int max)
{
  if( var == width )
    {
      int i = width-1;
      int *sol = ccl_new_array(int,width);

      while( dl != NULL )
	{
	  sol[i--] = dl->val;
	  dl = dl->next;
	}
      ccl_list_add(solutions,sol);
    }
  else
    {
      struct dlist data;
      
      data.next = dl;

      while( T != NULL )
	{
	  data.val = T->val;
	  s_assign_prefix_tree_rec(var+1,width,T->right,&data,solutions,max);
	  if( ccl_list_get_size(solutions) == max )
	    return;
	  T = T->down;
	}
    }
}

			/* --------------- */

static int **
s_get_assignments(int width, int max, prefix_tree *T, int *psize)
{
  int i;
  ccl_pair p;
  int **result;
  ccl_list solutions = ccl_list_create();

  s_assign_prefix_tree_rec(0,width,T,NULL,solutions,max);
  *psize = ccl_list_get_size(solutions);
  result = ccl_new_array(int *,*psize);
  for(i = 0, p = FIRST(solutions); i < *psize; i++, p = CDR(p))
    result[i] = CAR(p);
  ccl_list_delete(solutions);
  
  return result;
}

			/* --------------- */

static int
s_compute_range_tree(int width, sataf_msa R, prefix_tree **pT)
{
  int result = 1;
  int nb_bits = 0;
  int var = 0;
  int nb_vars = width;
  int stack_size = 33*nb_vars;
  uint32_t *values = ccl_new_array(uint32_t,nb_vars);
  int top = 0;
  display_stack *stack = ccl_new_array(display_stack,stack_size);
  
  stack[top].rel = sataf_msa_add_reference(R);
  stack[top].letter = 0;

  while( result && top >= 0 )
    {
      sataf_msa node = stack[top].rel;
      uint32_t letter = stack[top].letter;

      if( letter <= 1 )
	{
	  sataf_msa succ;

	  if( sataf_msa_is_final(node) && letter == 0 )
	    *pT = s_add_assignment(width,values,*pT);

	  succ = sataf_msa_succ(node,letter);
	      
	  if( s_is_in_stack(stack,top,succ) )
	    {
	      if( succ != node ) result = 0;
	      else
		{
		  sataf_msa succ2;

		  ccl_assert( letter == 0 );

		  succ2 = sataf_msa_succ(node,1);
		  
		  if( s_is_in_stack(stack,top,succ2) )
		    {
		      if((succ2 != node || sataf_msa_is_final(node) ) )
			result = 0;
		      else 
			{
			  ccl_assert( sataf_msa_is_zero(node) );
			  sataf_msa_del_reference(node);
			  top--;
			  var--;
			  if( var < 0 )
			    {
			      var = nb_vars-1;
			      nb_bits--;
			    }
			}
		    }
		  else if( sataf_msa_is_zero(succ2) )
		    {
		      sataf_msa_del_reference(node);
		      top--;
		      var--;
		      if( var < 0 )
			{
			  var = nb_vars-1;
			  nb_bits--;
			}
		    }
		  else
		    {
		      result = 0;
		    }

		  sataf_msa_del_reference(succ2);
		}
	    }
	  else /* succ isn't in stack */
	    {
	      if( letter == 1 )
		values[var] += (1<<nb_bits);

	      stack[top].letter++;

	      top++;

	      stack[top].rel = sataf_msa_add_reference(succ);
	      stack[top].letter = 0;
	      var++;

	      if( var == nb_vars )
		{
		  var = 0;
		  nb_bits++;
		}
	    }

	  sataf_msa_del_reference(succ);
	}
      else /* letter > 1 */
	{
	  values[var] -= (1 << nb_bits);
	  sataf_msa_del_reference(node);
	  top--;
	  var--;
	  if( var < 0 )
	    {
	      var = nb_vars-1;
	      nb_bits--;
	    }
	}
    }

  while( top >= 0 )
    sataf_msa_del_reference(stack[top--].rel);

  ccl_assert( top == - 1 );

  ccl_delete(values);
  ccl_delete(stack);

  return result;
}

			/* --------------- */

static void
s_delete_tree(prefix_tree *T)
{
  if( T == NULL ) return;
  s_delete_tree(T->right);
  s_delete_tree(T->down);
  ccl_delete(T);
}

			/* --------------- */
static int **
s_get_solutions(sataf_msa msa, int n, int *psize, int max)
{
  prefix_tree *T = NULL;
  int **result = NULL;

  s_compute_range_tree(n,msa,&T);
  result = s_get_assignments(n,max,T,psize);
      
  if( T != NULL )
    s_delete_tree(T);

  return result;
}

			/* --------------- */

typedef struct avs_st {
  struct sataf_automaton_st super;
  sataf_msa msa;
  int width;
  int *selection;
  int selsize;
  int pos;
} *avs;

			/* --------------- */

# define s_avs_to_dot NULL

			/* --------------- */

static void
s_avs_destroy(sataf_automaton self)
{
  avs a = (avs)self;

  sataf_msa_del_reference(a->msa);  
}

			/* --------------- */

static uint32_t
s_avs_get_alphabet_size(sataf_automaton self)
{
  return 2;
}

			/* --------------- */

# define s_avs_to_string NULL

static sataf_automaton
s_avs_succ(sataf_automaton self, uint32_t letter)
{
  avs a = (avs)self;
  sataf_msa succ;
  sataf_automaton R;
  int newpos = (a->pos+1)%a->selsize;

  if( a->selection[a->pos] )
    succ = sataf_msa_add_reference(a->msa);
  else
    succ = sataf_msa_succ(a->msa,letter);
  R = s_crt_add_variable_set(succ,a->width,newpos,a->selection,a->selsize);
  sataf_msa_del_reference(succ);

  return R;
}

			/* --------------- */

static int
s_avs_is_final(sataf_automaton self)
{
  avs a = (avs)self;

  return sataf_msa_is_final(a->msa);
}

			/* --------------- */

static int
s_avs_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  avs a1 = (avs)self;
  avs a2 = (avs)other;

  return a1->msa == a2->msa
    &&   a1->selection  == a1->selection
    &&   a1->selsize  == a2->selsize
    &&   a1->pos  == a2->pos;
}
			/* --------------- */


static uint32_t
s_avs_hashcode(sataf_automaton self)
{
  avs a = (avs)self;

  return 13*(uint32_t)a->selection
    +    71*(uint32_t)a->selsize
    +    97*a->pos
    +    (uint32_t)a->msa;
}

			/* --------------- */

static const struct sataf_automaton_methods_st AVS_METHODS = {
  "FAST-AVS",
  sizeof(struct avs_st),
  s_avs_destroy,
  s_avs_get_alphabet_size,
  s_avs_to_string,
  s_avs_succ,
  s_avs_is_final,
  s_avs_is_equal_to,
  s_avs_hashcode,
  s_avs_to_dot
};

			/* --------------- */

static sataf_automaton
s_crt_add_variable_set(sataf_msa msa, int n, int pos, int *sel, int selsize)
{
  sataf_automaton R;

  if( sataf_msa_is_one(msa) )
    R = sataf_automaton_create_one(2);
  else if( sataf_msa_is_zero(msa) )
    R = sataf_automaton_create_zero(2);
  else
    {
      avs A = (avs)sataf_automaton_create(sizeof(struct avs_st),&AVS_METHODS);

      A->msa = sataf_msa_add_reference(msa);
      A->width = n;
      A->selection = sel;
      A->selsize = selsize;
      A->pos = pos;

      R = (sataf_automaton)A;  
    }

  return R;
}

			/* --------------- */

genepi_engine *
genepi_plugin_init(void)
{
  return &PRESTAF2FAST;
}
