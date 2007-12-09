/* $Id: ccl-parse-tree.c,v 1.5 2006/03/15 16:03:06 point Exp $ */
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-parse-tree.h"

ccl_parse_tree
ccl_parse_tree_create(int                       type, 
		      const char *type_string,
		     ccl_parse_tree_value_type vtype,
		     int                       line, 
		     const           char *filename, 
		     ccl_parse_tree            child, 
		     ccl_parse_tree             next, 
		     ccl_parse_tree        container)
{
  ccl_parse_tree result = ccl_new(struct ccl_parse_tree_st);
  
  result->node_type         = type;
  result->node_type_string = type_string;
  result->value_type        = vtype;
  result->line              = line;
  result->filename          = filename;
  result->child             = child;
  result->next              = next;
  result->next_in_container = container;

  return result;
}

			/* --------------- */

int
ccl_parse_tree_count_siblings(ccl_parse_tree t)
{
  int result = 0;

  while( t != NULL )
    {
      result++;
      t = t->next;
    }

  return result;
}

void
ccl_parse_tree_delete_node(ccl_parse_tree t)
{
  ccl_pre( t != NULL );

  if( t->value_type == CCL_PARSE_TREE_STRING && t->value.string_value != NULL )
    ccl_delete(t->value.string_value);
  ccl_delete(t);
}

			/* --------------- */

void
ccl_parse_tree_delete_tree(ccl_parse_tree t)
{
  ccl_pre( t != NULL );

  if( t->child != NULL )
    ccl_parse_tree_delete_tree(t->child);
  if( t->next != NULL )
    ccl_parse_tree_delete_tree(t->next);
  ccl_parse_tree_delete_node(t);
}

			/* --------------- */

void
ccl_parse_tree_delete_container(ccl_parse_tree t)
{
  ccl_parse_tree next;

  for(; t; t = next)
    {
      next = t->next_in_container;
      ccl_parse_tree_delete_node(t);
    }
}

			/* --------------- */

ccl_parse_tree
ccl_parse_tree_reverse_siblings(ccl_parse_tree t)
{
  ccl_parse_tree prev = NULL;

  while( t != NULL ) 
    {
      ccl_parse_tree next = t->next;
      
      t->next = prev;
      prev = t;
      t = next;
    }

  return prev;
}

			/* --------------- */

static ccl_parse_tree
s_tree_duplicate_rec(ccl_parse_tree t, ccl_parse_tree *cont, int is_root,
		     char *filename)
{
  ccl_parse_tree result;

  if( t == NULL )  result = NULL;
  else
    {      
      ccl_parse_tree child = s_tree_duplicate_rec(t->child,cont,0,filename);
      ccl_parse_tree next = 
	is_root?NULL:s_tree_duplicate_rec(t->next,cont,0,filename);

      result = ccl_parse_tree_create(t->node_type,t->node_type_string,
				     t->value_type,t->line,
				     filename, child, next,
				     (cont?*cont:NULL));
      if( cont != NULL )
	*cont = result;

      switch( t->value_type ) {
      case CCL_PARSE_TREE_INT :
	result->value.int_value = t->value.int_value;
	break;
      case CCL_PARSE_TREE_STRING :
	result->value.string_value = ccl_string_dup(t->value.string_value);
	break;
      case CCL_PARSE_TREE_IDENT :
	result->value.id_value = t->value.id_value;
	break;
      case CCL_PARSE_TREE_EMPTY :
	break;
      }
    }

  return result;
}

			/* --------------- */

ccl_parse_tree
ccl_parse_tree_duplicate(ccl_parse_tree t, ccl_parse_tree *cont)
{
  return s_tree_duplicate_rec(t,cont,1,ccl_string_make_unique(t->filename));
}
