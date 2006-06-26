/* $Id: ccl-parse-tree.h,v 1.3 2006/03/15 16:03:06 point Exp $ */
#ifndef __CCL_PARSE_TREE_H__
# define __CCL_PARSE_TREE_H__

# include <ccl-string.h>

typedef union ccl_parse_tree_value_union {
  int int_value;
  char *string_value;
  ccl_ustring id_value;
} ccl_parse_tree_value;

typedef enum ccl_parse_tree_value_type_enum {
  CCL_PARSE_TREE_INT,
  CCL_PARSE_TREE_STRING,
  CCL_PARSE_TREE_IDENT,
  CCL_PARSE_TREE_EMPTY
} ccl_parse_tree_value_type;

typedef struct ccl_parse_tree_st *ccl_parse_tree;

struct ccl_parse_tree_st {
  int                       node_type;
  const char *node_type_string;
  ccl_parse_tree_value_type value_type;
  ccl_parse_tree_value           value;
  int                            line;
  const char                *filename;
  ccl_parse_tree                 child;
  ccl_parse_tree                  next;
  ccl_parse_tree     next_in_container;
};

			/* --------------- */

CCL_EXTERN ccl_parse_tree
ccl_parse_tree_create(int                       type, 
		      const char *type_string,
		      ccl_parse_tree_value_type vtype,
		      int                       line, 
		     const           char *filename, 
		     ccl_parse_tree            child, 
		     ccl_parse_tree             next, 
		     ccl_parse_tree        container);

CCL_EXTERN int
ccl_parse_tree_count_siblings(ccl_parse_tree t);

CCL_EXTERN void
ccl_parse_tree_delete_node(ccl_parse_tree t);

CCL_EXTERN void
ccl_parse_tree_delete_tree(ccl_parse_tree t);

CCL_EXTERN void
ccl_parse_tree_delete_container(ccl_parse_tree t);

CCL_EXTERN ccl_parse_tree
ccl_parse_tree_reverse_siblings(ccl_parse_tree t);

CCL_EXTERN ccl_parse_tree
ccl_parse_tree_duplicate(ccl_parse_tree t, ccl_parse_tree *cont);

#endif /* __CCL_PARSE_TREE_H__ */
