/* $Id: prestaf-interp.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __PRESTAF_INTERP_H__
# define __PRESTAF_INTERP_H__

# include <ccl-parse-tree.h>
# include <prestaf-formula.h>

typedef enum prestaf_ptree_node_enum {
  PRESTAF_FORMULA = 0,

  PRESTAF_DEFINITION,

  PRESTAF_OR, PRESTAF_AND, PRESTAF_NOT,
  PRESTAF_EQUIV, PRESTAF_IMPLY, PRESTAF_NEQ,

  PRESTAF_IN,

  PRESTAF_FORALL,  PRESTAF_EXISTS,

  PRESTAF_LEQ,  PRESTAF_GEQ,   PRESTAF_LT, PRESTAF_GT, PRESTAF_EQ,
  PRESTAF_PLUS, PRESTAF_MINUS, PRESTAF_NEG,

  PRESTAF_FACTOR,  PRESTAF_IDENT, PRESTAF_INTEGER,
  PRESTAF_STATEMENT_LIST
} prestaf_ptree_node;

CCL_EXTERN ccl_list
prestaf_interp(ccl_parse_tree tree);

#endif /* __PRESTAF_INTERP_H__ */
