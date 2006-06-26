/* $Id: prestaf-predicate.c,v 1.2 2005/10/04 15:04:28 point Exp $ */
#include "add-variable-automaton.h"
#include "quantifier-automaton.h"
#include "prestaf-predicate.h"

struct prestaf_predicate_st {
  uint32_t refcount;  /* This field counts the number of reference to this 
		       * structure. This counter is initially set to 1; when
		       * it falls to 0 the structure is deallocated.
		       * @see ::prestaf_predicate_add_reference and 
 		       * ::prestaf_predicate_del_reference.
		       */
  sataf_msa relation; /* The (msa-encoded) relation of the predicate. */
  ccl_list variables; /* Names labelling the columns of the relation. 
		       * Each name is a ::ccl_ustring.
		       */
};

static prestaf_predicate
s_alloc_rel(void)
{
  prestaf_predicate R = ccl_new(struct prestaf_predicate_st);

  R->refcount  = 1;

  return R;
}


prestaf_predicate
prestaf_predicate_create(ccl_list vars, sataf_msa rel)
{
  prestaf_predicate R = s_alloc_rel();

  ccl_pre( vars != NULL && rel != NULL );

  R->relation  = sataf_msa_add_reference(rel);
  R->variables = ccl_list_dup(vars);

  return R;
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_add_reference(prestaf_predicate P)
{
  ccl_pre( P != NULL );

  P->refcount++;

  return P;
}

			/* --------------- */

void
prestaf_predicate_del_reference(prestaf_predicate P)
{
  ccl_pre( P != NULL && P->refcount > 0 );

  if( --P->refcount == 0 )
    {
      sataf_msa_del_reference(P->relation);
      ccl_list_delete(P->variables);
      ccl_delete(P);
    }
}

			/* --------------- */

ccl_list
prestaf_predicate_get_variables(prestaf_predicate P)
{
  ccl_pre( P != NULL );

  return P->variables;
}

			/* --------------- */

sataf_msa
prestaf_predicate_get_relation(prestaf_predicate P)
{
  ccl_pre( P != NULL );

  return sataf_msa_add_reference(P->relation);
}

			/* --------------- */

void
prestaf_predicate_rename(prestaf_predicate P, ccl_list vars)
{
  ccl_pre( P != NULL && vars != NULL );

  ccl_list_delete(P->variables);
  P->variables = ccl_list_dup(vars);
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_not(prestaf_predicate P)
{
  prestaf_predicate result = s_alloc_rel();

  ccl_pre( P != NULL );

  result->variables = ccl_list_dup(P->variables);
  result->relation  = sataf_msa_not(P->relation);

  return result;
}

			/* --------------- */

typedef sataf_msa (*msa_binary_operator)(sataf_msa,sataf_msa);

static sataf_msa 
s_add_variable(sataf_msa S, uint32_t i, uint32_t n)
{
  sataf_automaton aut = add_variable_automaton_create(S,i,n);
  sataf_msa R = sataf_msa_compute(aut);
  sataf_automaton_del_reference(aut);

  return R;
}

static prestaf_predicate
s_apply_sataf_operator(prestaf_predicate P1, prestaf_predicate P2,
		       msa_binary_operator op)
{  
  prestaf_predicate P = s_alloc_rel();
  int i1 = 0;
  uint32_t n1 = ccl_list_get_size(P1->variables);
  int i2 = 0;
  uint32_t n2 = ccl_list_get_size(P2->variables);
  ccl_pair p1 = FIRST(P1->variables);
  ccl_pair p2 = FIRST(P2->variables);
  sataf_msa T1 = sataf_msa_add_reference(P1->relation);
  sataf_msa T2 = sataf_msa_add_reference(P2->relation);
  sataf_msa tmp;

  P->variables = ccl_list_create();

  for(; p1 != NULL && p2 != NULL; i1++, i2++)
    {
      if( CAR(p1) == CAR(p2) )
	{
	  ccl_list_insert(P->variables,CAR(p1),NULL);
	  p1 = CDR(p1); 
	  p2 = CDR(p2); 
	}
      else if( CAR(p1) < CAR(p2) )
	{
	  tmp = s_add_variable(T2,i2,n2);
	  sataf_msa_del_reference(T2);
	  T2 = tmp;
	  n2++; 
	  ccl_list_insert(P->variables,CAR(p1),NULL);
	  p1 = CDR(p1);
	}
      else 
	{
	  tmp = s_add_variable(T1,i1,n1);
	  sataf_msa_del_reference(T1);
	  T1 = tmp;
	  n1++;
	  ccl_list_insert(P->variables,CAR(p2),NULL);
	  p2 = CDR(p2);
	}
    }
  
  while( p1 != NULL )
    {
      tmp = s_add_variable(T2,i2,n2);
      sataf_msa_del_reference(T2);
      T2 = tmp;
      n2++;
      i2++;
      ccl_list_insert(P->variables,CAR(p1),NULL);
      p1 = CDR(p1);
    }

  while( p2 != NULL )
    {
      tmp = s_add_variable(T1,i1,n1);
      sataf_msa_del_reference(T1);
      T1 = tmp;
      n1++; i1++;
      ccl_list_insert(P->variables,CAR(p2),NULL);
      p2 = CDR(p2);
    }

  P->relation = op(T1,T2);

  sataf_msa_del_reference(T1);
  sataf_msa_del_reference(T2);

  return P;
}
			/* --------------- */


prestaf_predicate
prestaf_predicate_or(prestaf_predicate P1, prestaf_predicate P2)
{
  ccl_pre( P1 != NULL && P2 != NULL );

  return s_apply_sataf_operator(P1,P2,sataf_msa_or);
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_and(prestaf_predicate P1, prestaf_predicate P2)
{
  ccl_pre( P1 != NULL && P2 != NULL );

  return s_apply_sataf_operator(P1,P2,sataf_msa_and);
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_multi_or(ccl_list predicates)     
{
  ccl_pair p;
  prestaf_predicate tmp, R;

  ccl_pre( predicates != NULL && ccl_list_get_size(predicates) >= 1 );

  p = FIRST(predicates);
  R = prestaf_predicate_add_reference(CAR(p));

  for(p = CDR(p); p; p = CDR(p))
    {
      tmp = prestaf_predicate_or(R,CAR(p));
      prestaf_predicate_del_reference(R);
      R = tmp;
    }

  return R;
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_multi_and(ccl_list predicates)
{
  ccl_pair p;
  prestaf_predicate tmp, R;

  ccl_pre( predicates != NULL && ccl_list_get_size(predicates) >= 1 );

  p = FIRST(predicates);
  R = prestaf_predicate_add_reference(CAR(p));
  
  for(p = CDR(p); p; p = CDR(p))
    {
      tmp = prestaf_predicate_and(R,CAR(p));
      prestaf_predicate_del_reference(R);
      R = tmp;
    }
  
  return R;
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_imply(prestaf_predicate P1, prestaf_predicate P2)
{
  ccl_pre( P1 != NULL && P2 != NULL );

  return s_apply_sataf_operator(P1,P2,sataf_msa_imply);
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_equiv(prestaf_predicate P1, prestaf_predicate P2)
{
  ccl_pre( P1 != NULL && P2 != NULL );

  return s_apply_sataf_operator(P1,P2,sataf_msa_equiv);
}

			/* --------------- */

static prestaf_predicate
s_quantify(int forall, ccl_ustring v, prestaf_predicate P)
{
  prestaf_predicate R;
  int i = ccl_list_get_index(P->variables,v,NULL);
  
  if( i < 0 ) 
    {
      R = prestaf_predicate_create(P->variables,P->relation);
    }
  else 
    {
      int n = ccl_list_get_size(P->variables);
      sataf_automaton saut = quantifier_automaton_create(forall,i,n,
							 P->relation);
      R = s_alloc_rel();
      R->variables = ccl_list_dup(P->variables);
      ccl_list_remove(R->variables,v);
      if( forall ) 
	R->relation = sataf_msa_compute_with_transformer(saut,SSA_FORALL);
      else
	R->relation = sataf_msa_compute_with_transformer(saut,SSA_EXISTS);
      sataf_automaton_del_reference(saut);
    }

  return R;
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_exists(ccl_ustring v, prestaf_predicate P)
{
  ccl_pre( v != NULL && P != NULL );

  return s_quantify(0,v,P);
}

			/* --------------- */

prestaf_predicate
prestaf_predicate_forall(ccl_ustring v, prestaf_predicate P)
{
  ccl_pre( v != NULL && P != NULL );

  return s_quantify(1,v,P);
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
s_add_assignment(prestaf_predicate P, uint32_t *values, prefix_tree *T)
{
  ccl_pair p;
  prefix_tree *result = T;
  prefix_tree **pT = &result;

  for(p = FIRST(P->variables); p; p = CDR(p), values++)
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
  ccl_ustring         id;
  uint32_t     val;
  struct dlist *next;
};

static void
s_display_prefix_tree_rec(ccl_pair var, prefix_tree *T, struct dlist *dl)
{
  if( var == NULL )
    {
      char *fmt = "%s = %d, ";

      while( dl != NULL )
	{
	  if( dl->next == NULL )
	    fmt = "%s = %d";
	  ccl_log(CCL_LOG_DISPLAY,fmt,dl->id,dl->val);
	  dl = dl->next;
	}
      ccl_log(CCL_LOG_DISPLAY,"\n");
    }
  else
    {
      struct dlist data;
      
      data.id   = (ccl_ustring)CAR(var);
      data.next = dl;

      while( T != NULL )
	{
	  data.val = T->val;
	  s_display_prefix_tree_rec(CDR(var),T->right,&data);
	  T = T->down;
	}
    }
}

			/* --------------- */

static void
s_display_prefix_tree(prestaf_predicate R, prefix_tree *T)
{
  ccl_log(CCL_LOG_DISPLAY,"{\n");
  s_display_prefix_tree_rec(FIRST(R->variables),T,NULL);
  ccl_log(CCL_LOG_DISPLAY,"}\n");
}

			/* --------------- */

static int
s_compute_range_tree(prestaf_predicate P, prefix_tree **pT)
{
  int result = 1;
  int nb_bits = 0;
  int var = 0;
  int nb_vars = ccl_list_get_size(P->variables);
  int stack_size = 33*nb_vars;
  uint32_t *values = ccl_new_array(uint32_t,nb_vars);
  int top = 0;
  display_stack *stack = ccl_new_array(display_stack,stack_size);
  
  stack[top].rel = sataf_msa_add_reference(P->relation);
  stack[top].letter = 0;

  while( result && top >= 0 )
    {
      sataf_msa node = stack[top].rel;
      uint32_t letter = stack[top].letter;

      if( letter <= 1 )
	{
	  sataf_msa succ;

	  if( sataf_msa_is_final(node) && letter == 0 )
	    *pT = s_add_assignment(P,values,*pT);

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
		      if( succ2 != node || sataf_msa_is_final(node) ) 
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
int
prestaf_predicate_display_vectors(prestaf_predicate P)
{
  prefix_tree *T = NULL;
  int result = s_compute_range_tree(P,&T);

  if( result )
    s_display_prefix_tree(P,T);

  if( T != NULL )
    s_delete_tree(T);

  return result;
}
