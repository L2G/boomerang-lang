/* $Id: prestaf-interp.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <ccl-hash.h>
#include "prestaf-interp.h"

static void
s_interp_statement(ccl_parse_tree pt, ccl_hash symbols, ccl_list result);

			/* --------------- */

static void
s_delete_formula(void *ptr);

			/* --------------- */

ccl_list
prestaf_interp(ccl_parse_tree tree)
{
  ccl_list  result = ccl_list_create();
  ccl_hash symbols = ccl_hash_create(NULL,NULL,NULL,s_delete_formula);

  tree = tree->child;
  while( tree != NULL )
    {
      s_interp_statement(tree,symbols,result);
      tree = tree->next;
    }

  ccl_hash_delete(symbols);

  return result;
}

			/* --------------- */

static prestaf_linear_term
s_interp_linear_term(ccl_parse_tree pt, ccl_hash symbols)
{
  int                  a;
  ccl_ustring              x;
  prestaf_linear_term t1 = NULL;
  prestaf_linear_term t2 = NULL;
  prestaf_linear_term  R = NULL;

  switch( pt->node_type ) {
  case PRESTAF_PLUS  :
  case PRESTAF_MINUS :
    t2 = s_interp_linear_term(pt->child->next,symbols);
  case PRESTAF_NEG :
    t1 = s_interp_linear_term(pt->child,symbols);
    if( pt->node_type == PRESTAF_NEG )
      R = prestaf_linear_term_neg(t1);
    else if( pt->node_type == PRESTAF_PLUS ) 
      R = prestaf_linear_term_plus(t1,t2);
    else
      R = prestaf_linear_term_minus(t1,t2);
    break;

  case PRESTAF_FACTOR :
    R = prestaf_linear_term_create(1,
				   &pt->child->value.int_value,
				   &pt->child->next->value.id_value,
				   0);
    break;

  case PRESTAF_IDENT :
    a = 1;
    x = pt->value.id_value;
    R = prestaf_linear_term_create(1,&a,&x,0);
    break;

  case PRESTAF_INTEGER :
    R = prestaf_linear_term_create(0,NULL,NULL,pt->value.int_value);
    break;
  };

  if( t1 != NULL )
    prestaf_linear_term_del_reference(t1);
  if( t2 != NULL )
    prestaf_linear_term_del_reference(t2);

  return R;
}

			/* --------------- */

static prestaf_formula
s_interp_linear_equation(ccl_parse_tree pt, ccl_hash symbols)
{
  prestaf_formula      R = NULL;
  prestaf_linear_term t1 = s_interp_linear_term(pt->child,symbols);
  prestaf_linear_term t2 = s_interp_linear_term(pt->child->next,symbols);

  switch( pt->node_type ) {
  case PRESTAF_EQ  : R = prestaf_formula_create_eq(t1,t2);  break;
  case PRESTAF_NEQ : R = prestaf_formula_create_neq(t1,t2); break;
  case PRESTAF_LT  : R = prestaf_formula_create_lt(t1,t2);  break;
  case PRESTAF_LEQ : R = prestaf_formula_create_leq(t1,t2); break;
  case PRESTAF_GT  : R = prestaf_formula_create_gt(t1,t2);  break;
  case PRESTAF_GEQ : R = prestaf_formula_create_geq(t1,t2); break;
  };

  if( t1 != NULL )
    prestaf_linear_term_del_reference(t1);
  if( t2 != NULL )
    prestaf_linear_term_del_reference(t2);

  ccl_assert( R != NULL );

  return R;
}

			/* --------------- */

static prestaf_formula
s_interp_in_formula(ccl_parse_tree pt, ccl_hash symbols)
{
  prestaf_formula        R = NULL;  
  prestaf_linear_term    t = s_interp_linear_term(pt->child,symbols);
  prestaf_linear_term tmin = s_interp_linear_term(pt->child->next,symbols);
  prestaf_linear_term tmax = s_interp_linear_term(pt->child->next->next,
						  symbols);

  {
    prestaf_formula tmp1 = prestaf_formula_create_leq(tmin,t);
    prestaf_formula tmp2 = prestaf_formula_create_leq(t,tmax);
    
    R = prestaf_formula_create_and(tmp1,tmp2);

    prestaf_formula_del_reference(tmp1);
    prestaf_formula_del_reference(tmp2);
  }

  if( t != NULL )
    prestaf_linear_term_del_reference(t);
  if( tmax != NULL )
    prestaf_linear_term_del_reference(tmin);
  if( tmax != NULL )
    prestaf_linear_term_del_reference(tmax);

  ccl_assert( R != NULL );

  return R;
}

			/* --------------- */

static prestaf_formula
s_interp_formula(ccl_parse_tree pt, ccl_hash symbols)
{
  ccl_parse_tree        t;
  prestaf_formula f1 = NULL;
  prestaf_formula f2 = NULL;
  prestaf_formula  R = NULL;
  ccl_list      vars;

  switch( pt->node_type ) {
  case PRESTAF_IDENT : 
    if( ccl_hash_find(symbols,pt->value.id_value) )
      {
	R = ccl_hash_get(symbols);
	R = prestaf_formula_add_reference(R);
      }
    else
      {
	ccl_log(CCL_LOG_ERROR,"undefined symbol '%s'\n",pt->value.id_value);
	abort();
      }
    break;
  case PRESTAF_OR    : 
  case PRESTAF_AND   :
  case PRESTAF_IMPLY :
  case PRESTAF_EQUIV :
    f2 = s_interp_formula(pt->child->next,symbols);
  case PRESTAF_NOT :
    f1 = s_interp_formula(pt->child,symbols);
    if( pt->node_type == PRESTAF_NOT )
      R = prestaf_formula_create_not(f1);
    else if( pt->node_type == PRESTAF_OR )
      R = prestaf_formula_create_or(f1,f2);
    else if( pt->node_type == PRESTAF_AND )
      R = prestaf_formula_create_and(f1,f2);
    else if( pt->node_type == PRESTAF_IMPLY ) 
      R = prestaf_formula_create_imply(f1,f2);
    else       
      R = prestaf_formula_create_equiv(f1,f2);
    break;

  case PRESTAF_FORALL: 
  case PRESTAF_EXISTS:
    vars = ccl_list_create();
    f1 = s_interp_formula(pt->child,symbols);
    for(t = pt->child->next; t; t = t->next)
      {
	if( ! ccl_list_has(vars,t->value.id_value) )
	  ccl_list_add(vars,t->value.id_value);
      }

    if( pt->node_type == PRESTAF_FORALL ) 
      R = prestaf_formula_create_forall(vars,f1);
    else
      R = prestaf_formula_create_exists(vars,f1);
    ccl_list_delete(vars);
    break;

  case PRESTAF_IN:
    R = s_interp_in_formula(pt,symbols);
    break;

  default:
    R = s_interp_linear_equation(pt,symbols);
    break;
  };


  if( f1 != NULL )
    prestaf_formula_del_reference(f1);
  if( f2 != NULL )
    prestaf_formula_del_reference(f2);

  return R;
}

			/* --------------- */

static void
s_interp_definition(ccl_parse_tree pt, ccl_hash symbols)
{
  prestaf_formula F;

  pt = pt->child;
  F = s_interp_formula(pt->next,symbols);

  if( ccl_hash_find(symbols,pt->value.id_value) )
    ccl_log(CCL_LOG_WARNING,"warning: symbol '%s' redefinition\n",
	   pt->value.id_value);
  ccl_hash_insert(symbols,F);
}

			/* --------------- */

static void
s_interp_statement(ccl_parse_tree pt, ccl_hash symbols, ccl_list result)
{
  if( pt->node_type == PRESTAF_DEFINITION )
    s_interp_definition(pt,symbols);
  else
    {
      prestaf_formula F = s_interp_formula(pt,symbols);
      ccl_list_add(result,F);
    }
}

			/* --------------- */

static void
s_delete_formula(void *ptr)
{
  prestaf_formula_del_reference(ptr);
}
