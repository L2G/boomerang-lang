/* $Id: prestaf-formula.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include <stdio.h>
#include "linear-constraint-automaton.h"
#include "quantifier-automaton.h"
#include "add-variable-automaton.h"
#include "p-prestaf-formula.h"
#include "prestaf-formula.h"


static prestaf_formula
s_create_formula(size_t size, prestaf_formula_type type)
{
  prestaf_formula R = ccl_calloc(size,1);

  R->refcount = 1;
  R->type = type;
  R->P = NULL;

  return R;
}

			/* --------------- */

static void
s_delete_formula(prestaf_formula F)
{
  ccl_pair  p;

  switch( F->type ) {
  case PF_AND: case PF_OR: case PF_NOT: case PF_EQUIV: case PF_IMPLY:
    for(p = FIRST(((prestaf_boolean_formula)F)->operands); p; p = CDR(p))      
      prestaf_formula_del_reference(CAR(p));
    ccl_list_delete(((prestaf_boolean_formula)F)->operands);
    break;

  case PF_FORALL: case PF_EXISTS:
    prestaf_formula_del_reference(((prestaf_quantifier)F)->F);
    break;

  default:
    prestaf_linear_term_del_reference(((prestaf_linear_constraint)F)->axb);
  };

  if( F->P != NULL )
    prestaf_predicate_del_reference(F->P);

  ccl_delete(F);  
}

			/* --------------- */

static void
s_get_variables(prestaf_formula f, ccl_list R)
{
  ccl_pair          p;
  prestaf_factor   pf;
  ccl_list        tmp;

  switch( f->type ) {
  case PF_NOT: case PF_AND: case PF_OR: case PF_EQUIV: case PF_IMPLY:
    for(p = FIRST(((prestaf_boolean_formula)f)->operands); p; p = CDR(p))      
      s_get_variables(CAR(p),R);
    break;

  case PF_FORALL: case PF_EXISTS:
    tmp = ccl_list_create();
    s_get_variables(((prestaf_quantifier)f)->F,tmp);

    for(p = FIRST(tmp); p; p = CDR(p))
      {
	if( CAR(p) == ((prestaf_quantifier)f)->qV )
	  continue;
	if( ! ccl_list_has(R,CAR(p)) )
	  ccl_list_insert(R,CAR(p),NULL);
      }
    ccl_list_delete(tmp);
    break;

  default:
    for(pf = ((prestaf_linear_constraint)f)->axb->factors; pf; pf = pf->next)
      {
	if( ! ccl_list_has(R,pf->x) )
	  ccl_list_insert(R,pf->x,NULL);
      }
  };
}

			/* --------------- */

ccl_list
prestaf_formula_get_variables(prestaf_formula f)
{
  ccl_list R;

  ccl_pre( f != NULL );

  if( f->P != NULL )
    R = ccl_list_dup(prestaf_predicate_get_variables(f->P));
  else
    {
      R = ccl_list_create();
      s_get_variables(f,R);
    }

  return R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_add_reference(prestaf_formula F)
{
  ccl_pre( F != NULL );

  F->refcount++;

  return F;
}

			/* --------------- */

void
prestaf_formula_del_reference(prestaf_formula F)
{
  ccl_pre( F != NULL && F->refcount > 0 );

  if( --F->refcount == 0 )
    s_delete_formula(F);
}

			/* --------------- */

static void
s_display_factor(prestaf_factor ft)
{
  if( ft->a == 1 )
    ccl_log(CCL_LOG_DISPLAY,"%s",ft->x);
  else if( ft->a == -1 )
    ccl_log(CCL_LOG_DISPLAY,"-%s",ft->x);
  else 
    ccl_log(CCL_LOG_DISPLAY,"%d*%s",ft->a,ft->x);
    
}

			/* --------------- */

void
prestaf_formula_display(prestaf_formula f)
{
  ccl_pair            p;
  const char       *fmt;
  prestaf_factor     pf;
  prestaf_linear_term t;

  ccl_pre( f != NULL );

  switch( f->type ) {
  case PF_AND : 
  case PF_OR :
  case PF_EQUIV :
  case PF_IMPLY :
    if( f->type == PF_AND )        fmt = " && ";
    else if( f->type == PF_OR )    fmt = " || ";
    else if( f->type == PF_IMPLY ) fmt = " => ";
    else                           fmt = " <=> ";
    
    for(p = FIRST(((prestaf_boolean_formula)f)->operands); p; p = CDR(p))
      {
	prestaf_formula_display(CAR(p));
	if( CDR(p) != NULL )
	  ccl_log(CCL_LOG_DISPLAY,fmt);
      }
    break;

  case PF_NOT :
    ccl_log(CCL_LOG_DISPLAY,"! (");    
    prestaf_formula_display(CAR(FIRST(((prestaf_boolean_formula)f)
				      ->operands)));
    ccl_log(CCL_LOG_DISPLAY,")");    
    break;

    break;

  case PF_FORALL :
  case PF_EXISTS :
    if( f->type == PF_FORALL ) ccl_log(CCL_LOG_DISPLAY,"forall ");
    else                       ccl_log(CCL_LOG_DISPLAY,"exists ");
    ccl_log(CCL_LOG_DISPLAY,"%s ",((prestaf_quantifier)f)->qV);

    ccl_log(CCL_LOG_DISPLAY,"(");
    prestaf_formula_display(((prestaf_quantifier)f)->F);
    ccl_log(CCL_LOG_DISPLAY,")");
    break;

  case PF_EQ : case PF_GT : case PF_GEQ : 
    t = ((prestaf_linear_constraint)f)->axb;
    
    for(pf = t->factors; pf; pf = pf->next)
      {
	if( pf == t->factors || pf->a < 0 )
	  s_display_factor(pf);
	else 
	  {
	    ccl_log(CCL_LOG_DISPLAY,"+");
	    s_display_factor(pf);
	  }
      }

    if( f->type == PF_EQ )       fmt = " == %d";
    else if( f->type == PF_GEQ ) fmt = " >= %d";
    else                         fmt = " > %d";
    ccl_log(CCL_LOG_DISPLAY,fmt,-t->b);
    break;
  };
}


			/* --------------- */

static void
s_compute_solutions(prestaf_formula f, int depth);

static void
s_compute_quantifier_solutions(prestaf_quantifier qf, int depth)
{
  s_compute_solutions(qf->F,depth);
  if( qf->super.type == PF_FORALL )
    qf->super.P = prestaf_predicate_forall(qf->qV,qf->F->P);
  else 
    qf->super.P = prestaf_predicate_exists(qf->qV,qf->F->P);
}

			/* --------------- */

static void
s_compute_linear_constraint_solutions(prestaf_linear_constraint f)
{
  int type;
  sataf_automaton saut;
  sataf_msa rel;
  ccl_list vars = prestaf_formula_get_variables((prestaf_formula)f);

  ccl_assert( f->super.P == NULL );
  
  switch(f->super.type) {
  case PF_EQ  : type = LCA_EQ; break;
  case PF_GT  : type = LCA_GT; break;
  default     : ccl_assert(f->super.type == PF_GEQ); type = LCA_GEQ; break;
  }

  saut = linear_constraint_automaton_create(type,f->axb);
  rel = sataf_msa_compute(saut);

  f->super.P = prestaf_predicate_create(vars,rel);

  sataf_msa_del_reference(rel);
  ccl_list_delete(vars);
  sataf_automaton_del_reference(saut);
}

			/* --------------- */

static void
s_compute_solutions(prestaf_formula f, int depth)
{
  ccl_pair                 p;
  prestaf_boolean_formula bf = (prestaf_boolean_formula)f;

  if( f->P != NULL )
    return;

  /*
  ccl_log(CCL_LOG_DISPLAY,"[%d] compute ",depth);
  prestaf_formula_display(f);
  ccl_log(CCL_LOG_DISPLAY,"\n");
  */

  switch( f->type ) {
  case PF_NOT : case PF_AND: case PF_OR: case PF_EQUIV: case PF_IMPLY: 
    for(p = FIRST(bf->operands); p; p = CDR(p))
      s_compute_solutions(CAR(p),depth+1);
    p = FIRST(bf->operands);

    if( f->type == PF_AND || f->type == PF_OR )
      {
	ccl_list args = ccl_list_create();
	for(; p; p = CDR(p))
	  ccl_list_add(args,((prestaf_formula)CAR(p))->P);
	if( f->type == PF_AND ) f->P = prestaf_predicate_multi_and(args);
	else                    f->P = prestaf_predicate_multi_or(args);
	ccl_list_delete(args);
      }
    else if( f->type == PF_NOT )
      f->P = prestaf_predicate_not(((prestaf_formula)CAR(p))->P);
    else if( f->type == PF_IMPLY ) 
      f->P = prestaf_predicate_imply(((prestaf_formula)CAR(p))->P,
				      ((prestaf_formula)CADR(p))->P);
    else 
      f->P = prestaf_predicate_equiv(((prestaf_formula)CAR(p))->P,
				      ((prestaf_formula)CADR(p))->P);
    break;
        
  case PF_FORALL: case PF_EXISTS:
    s_compute_quantifier_solutions((prestaf_quantifier)f,depth+1);
    break;
    
  default: /* linear constraint */
    s_compute_linear_constraint_solutions((prestaf_linear_constraint)f);
    break;
  };

  /*
  ccl_log(CCL_LOG_DISPLAY,"[%d] done\n",depth);
  */

  ccl_post( f->P != NULL );
}

			/* --------------- */

prestaf_predicate
prestaf_formula_solutions(prestaf_formula f)
{
  ccl_pre( f != NULL );

  s_compute_solutions(f,0);

  return prestaf_predicate_add_reference(f->P);
}

			/* --------------- */

static prestaf_formula
s_create_boolean_formula(prestaf_formula_type type,
			 prestaf_formula        f1, 
			 prestaf_formula        f2)
{
  prestaf_boolean_formula R = (prestaf_boolean_formula)
    s_create_formula(sizeof(struct prestaf_boolean_formula_st),type);

  R->operands = ccl_list_create();
  ccl_list_add(R->operands,prestaf_formula_add_reference(f1));
  if( f2 != NULL )
    ccl_list_add(R->operands,prestaf_formula_add_reference(f2));

  return (prestaf_formula)R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_or(prestaf_formula f1, prestaf_formula f2)
{
  ccl_pair                p;
  prestaf_boolean_formula R = (prestaf_boolean_formula)
    s_create_formula(sizeof(struct prestaf_boolean_formula_st),PF_OR);
  
  ccl_pre( f1 != NULL && f2 != NULL );

  R->operands = ccl_list_create();
  
  if( f1->type == PF_OR ) 
    {
      for(p = FIRST(((prestaf_boolean_formula)f1)->operands); p; p = CDR(p))
	prestaf_formula_add_reference(CAR(p));
      ccl_list_append(R->operands,((prestaf_boolean_formula)f1)->operands);
    }
  else 
    {
      ccl_list_add(R->operands,prestaf_formula_add_reference(f1));
    }

  if( f2->type == PF_OR ) 
    {
      for(p = FIRST(((prestaf_boolean_formula)f2)->operands); p; p = CDR(p))
	prestaf_formula_add_reference(CAR(p));
      ccl_list_append(R->operands,((prestaf_boolean_formula)f2)->operands);
    }
  else 
    {
      ccl_list_add(R->operands,prestaf_formula_add_reference(f2));
    }

  return (prestaf_formula)R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_and(prestaf_formula f1, prestaf_formula f2)
{
  ccl_pair                p;
  prestaf_boolean_formula R = (prestaf_boolean_formula)
    s_create_formula(sizeof(struct prestaf_boolean_formula_st),PF_AND);
  
  ccl_pre( f1 != NULL && f2 != NULL );

  R->operands = ccl_list_create();
  
  if( f1->type == PF_AND ) 
    {
      for(p = FIRST(((prestaf_boolean_formula)f1)->operands); p; p = CDR(p))
	prestaf_formula_add_reference(CAR(p));
      ccl_list_append(R->operands,((prestaf_boolean_formula)f1)->operands);
    }
  else 
    {
      ccl_list_add(R->operands,prestaf_formula_add_reference(f1));
    }

  if( f2->type == PF_AND ) 
    {
      for(p = FIRST(((prestaf_boolean_formula)f2)->operands); p; p = CDR(p))
	prestaf_formula_add_reference(CAR(p));
      ccl_list_append(R->operands,((prestaf_boolean_formula)f2)->operands);
    }
  else 
    {
      ccl_list_add(R->operands,prestaf_formula_add_reference(f2));
    }

  return (prestaf_formula)R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_equiv(prestaf_formula f1, prestaf_formula f2)
{
  return s_create_boolean_formula(PF_EQUIV,f1,f2);
}
			/* --------------- */

prestaf_formula
prestaf_formula_create_imply(prestaf_formula f1, prestaf_formula f2)
{
  return s_create_boolean_formula(PF_IMPLY,f1,f2);
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_not(prestaf_formula f)
{
  return s_create_boolean_formula(PF_NOT,f,NULL);
}

			/* --------------- */

static prestaf_formula
s_create_quantifier(prestaf_formula_type type,
		    ccl_ustring                qV,
		    prestaf_formula         F)
{
  prestaf_quantifier qF = (prestaf_quantifier)
    s_create_formula(sizeof(struct prestaf_quantifier_st),type);
  
  qF->qV = qV;
  qF->F  = prestaf_formula_add_reference(F);

  return (prestaf_formula)qF;
}

			/* --------------- */

static prestaf_formula
s_create_quantifier_formula_rec(prestaf_formula_type type,
				ccl_pair                p,
				prestaf_formula         F)
{
  prestaf_formula R;

  if( CDR(p) == NULL )
    R = s_create_quantifier(type,CAR(p),F);
  else
    {
      prestaf_formula tmp = s_create_quantifier_formula_rec(type,CDR(p),F);
      R = s_create_quantifier(type,CAR(p),tmp);
      prestaf_formula_del_reference(tmp);
    }

  return R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_forall(ccl_list variables, 
			      prestaf_formula  f)
{
  ccl_pre( variables != NULL && f != NULL ) ;
  ccl_pre( ccl_list_get_size(variables) > 0 );

  return s_create_quantifier_formula_rec(PF_FORALL,FIRST(variables),f);
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_exists(ccl_list variables, 
			      prestaf_formula  f)
{
  ccl_pre( ccl_list_get_size(variables) > 0 );

  return s_create_quantifier_formula_rec(PF_EXISTS,FIRST(variables),f);
}

			/* --------------- */

static prestaf_formula
s_create_linear_constraint(prestaf_formula_type type,
			   prestaf_linear_term    t1,
			   prestaf_linear_term    t2)
{
  prestaf_linear_constraint R = (prestaf_linear_constraint)
    s_create_formula(sizeof(struct prestaf_linear_constraint_st),type);
  
  ccl_pre( t1 != NULL && t2 != NULL );
  R->axb = prestaf_linear_term_minus(t1,t2);

  return (prestaf_formula)R;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_eq(prestaf_linear_term t1, prestaf_linear_term  t2)
{
  return s_create_linear_constraint(PF_EQ,t1,t2);
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_neq(prestaf_linear_term t1, prestaf_linear_term t2)
{
  prestaf_formula  f = prestaf_formula_create_eq(t1,t2);
  prestaf_formula nf = prestaf_formula_create_not(f);
  prestaf_formula_del_reference(f);

  return nf;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_lt(prestaf_linear_term t1, prestaf_linear_term t2)
{
  prestaf_formula  f = prestaf_formula_create_geq(t1,t2);
  prestaf_formula nf = prestaf_formula_create_not(f);
  prestaf_formula_del_reference(f);

  return nf;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_leq(prestaf_linear_term t1, prestaf_linear_term t2)
{
  prestaf_formula  f = prestaf_formula_create_gt(t1,t2);
  prestaf_formula nf = prestaf_formula_create_not(f);
  prestaf_formula_del_reference(f);

  return nf;
}

			/* --------------- */

prestaf_formula
prestaf_formula_create_gt(prestaf_linear_term t1, prestaf_linear_term t2)
{
  return s_create_linear_constraint(PF_GT,t1,t2);}

			/* --------------- */

prestaf_formula
prestaf_formula_create_geq(prestaf_linear_term t1, prestaf_linear_term t2)
{
  return s_create_linear_constraint(PF_GEQ,t1,t2);
}

			/* --------------- */

static prestaf_factor
s_new_factor(int a, ccl_ustring x)
{
  prestaf_factor R = ccl_new(struct prestaf_factor_st);

  R->next = NULL;
  R->a    = a;
  R->x    = x;

  return R;
}


prestaf_linear_term
prestaf_linear_term_create(uint32_t n, int *a, ccl_ustring *x, int b)
{
  uint32_t i;
  prestaf_factor *pf;
  prestaf_linear_term R = ccl_new(struct prestaf_linear_term_st);

  ccl_pre( ccl_imply(n >= 1,a != NULL && x != NULL) );

  R->refcount = 1;
  R->factors = NULL;
  R->b = b;
  R->nb_factors = n;

  for(i = 0, pf = &R->factors; i < n; i++, pf = &((*pf)->next))
    *pf = s_new_factor(a[i],x[i]);
  
  return R;
}

			/* --------------- */

static prestaf_factor
s_merge_factors(prestaf_factor f1, prestaf_factor f2, int (*operator)(int,int))
{
  int            a, a1, a2;
  prestaf_factor R;
  prestaf_factor nf1, nf2;
  ccl_ustring        x;

  a = a1 = a2 = 0;

  if( f1 == NULL && f2 == NULL )
    return NULL;

  if( f1 == NULL ) 
    { nf1 = f1;       nf2 = f2->next; a1 = 0;     a2 = f2->a; x = f2->x; }
  else if( f2 == NULL ) 
    { nf1 = f1->next; nf2 = f2;       a1 = f1->a; a2 = 0;     x = f1->x; }
  else if( f1->x < f2->x ) 
    { nf1 = f1->next; nf2 = f2;       a1 = f1->a; a2 = 0;     x = f1->x; }
  else if( f1->x > f2->x ) 
    { nf1 = f1;       nf2 = f2->next; a1 = 0;     a2 = f2->a; x = f2->x; }
  else 
    { nf1 = f1->next; nf2 = f2->next; a1 = f1->a; a2 = f2->a; x = f1->x; }

  if( (a = operator(a1,a2)) == 0 )
    R = s_merge_factors(nf1,nf2,operator);
  else
    {
      R       = s_new_factor(a,x);
      R->next = s_merge_factors(nf1,nf2,operator);
    }

  return R;
}

			/* --------------- */

static int
s_plus(int a1, int a2)
{
  return a1+a2;
}

			/* --------------- */

static uint32_t
s_count_factors(prestaf_linear_term t)
{
  uint32_t      r = 0;
  prestaf_factor pf;

  for(pf = t->factors; pf; pf = pf->next)
    r++;

  return r;
}

			/* --------------- */

prestaf_linear_term
prestaf_linear_term_plus(prestaf_linear_term t1, prestaf_linear_term t2)
{
  prestaf_linear_term R = ccl_new(struct prestaf_linear_term_st);

  ccl_pre( t1 != NULL && t2 != NULL );

  R->refcount = 1;
  R->factors = s_merge_factors(t1->factors,t2->factors,s_plus);
  R->b = t1->b+t2->b;
  R->nb_factors = s_count_factors(R);

  return R;
}

			/* --------------- */

static int
s_minus(int a1, int a2)
{
  return a1-a2;
}

			/* --------------- */

prestaf_linear_term
prestaf_linear_term_minus(prestaf_linear_term t1, prestaf_linear_term t2)
{
  prestaf_linear_term R = ccl_new(struct prestaf_linear_term_st);

  ccl_pre( t1 != NULL && t2 != NULL );

  R->refcount = 1;
  R->factors = s_merge_factors(t1->factors,t2->factors,s_minus);
  R->b = t1->b-t2->b;
  R->nb_factors = s_count_factors(R);

  return R;
}

			/* --------------- */

prestaf_linear_term
prestaf_linear_term_neg(prestaf_linear_term t)
{
  prestaf_factor           f;
  prestaf_factor         *pf;
  prestaf_linear_term result = ccl_new(struct prestaf_linear_term_st);

  ccl_pre( t != NULL );

  result->refcount = 1;
  result->b = -t->b;
  result->nb_factors = t->nb_factors;
  pf = &result->factors;
  for(f = t->factors; f; f = f->next, pf = &((*pf)->next))
    {
      prestaf_factor nf = ccl_new(struct prestaf_factor_st);
      
      nf->a = -f->a;
      nf->x = f->x;
      nf->next = NULL;
      *pf = nf;
    }    

  return result;
}

			/* --------------- */

prestaf_linear_term
prestaf_linear_term_add_reference(prestaf_linear_term t)
{
  ccl_pre( t != NULL );

  t->refcount++;

  return t;
}

			/* --------------- */

void
prestaf_linear_term_del_reference(prestaf_linear_term t)
{
  ccl_pre( t != NULL && t->refcount > 0 );

  if( --t->refcount ==  0 )
    {
      prestaf_factor f, next;

      for(f = t->factors; f; f = next)
	{
	  next = f->next;
	  ccl_delete(f);
	}
	  
      ccl_delete(t);
    }
}

			/* --------------- */

int
prestaf_linear_term_get_coef(prestaf_linear_term t, ccl_ustring x)
{
  prestaf_factor f;

  ccl_pre( t != NULL && x != NULL );

  for(f = t->factors; f; f = f->next)
    {
      if( f->x == x )
	return f->a;
    }

  return 0;
}

			/* --------------- */

int
prestaf_linear_term_get_ith_coef(prestaf_linear_term t, uint32_t i)
{
  prestaf_factor f = t->factors;

  ccl_pre( i < t->nb_factors );

  while( i-- && f != NULL )
    f = f->next;

  if( f != NULL )
    return f->a;
  return 0;
}

			/* --------------- */

void
prestaf_formula_statistics(prestaf_formula F, ccl_log_type log)
{
  sataf_msa rel;
  sataf_msa_info info;

  ccl_pre( F != NULL );

  rel = prestaf_predicate_get_relation(F->P);
  sataf_msa_get_info(rel,&info);
  sataf_msa_del_reference(rel);

  ccl_log(CCL_LOG_DISPLAY,
	  "# states                = %d\n"
	  "# shared automaton (SA) = %d\n"
	  "# ea                    = %d\n"
	  "# depth                 = %d\n"
	  "# refcount              = %d\n",
	  info.nb_states,
	  info.nb_shared_automata,
	  info.nb_exit_automata,
	  info.depth,
	  info.refcount);
}

			/* --------------- */



