/* $Id: p-prestaf-formula.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __PRESTAF_FORMULA_P_H__
# define __PRESTAF_FORMULA_P_H__

# include <prestaf-formula.h>
# include <prestaf-predicate.h>

typedef enum prestaf_formula_type_enum {
  PF_AND, PF_OR, PF_NOT, PF_EQUIV, PF_IMPLY,
  PF_EQ, PF_GT, PF_GEQ,
  PF_FORALL, PF_EXISTS
} prestaf_formula_type;


typedef struct prestaf_boolean_formula_st   *prestaf_boolean_formula;
typedef struct prestaf_factor_st            *prestaf_factor;
typedef struct prestaf_linear_constraint_st *prestaf_linear_constraint;
typedef struct prestaf_quantifier_st        *prestaf_quantifier;


struct prestaf_formula_st {
  uint32_t refcount;
  prestaf_formula_type type;
  prestaf_predicate P;
};

struct prestaf_boolean_formula_st {
  struct prestaf_formula_st super;
  ccl_list operands;
};

struct prestaf_linear_constraint_st {
  struct prestaf_formula_st super;
  prestaf_linear_term axb;
};

struct prestaf_quantifier_st {
  struct prestaf_formula_st super;
  ccl_ustring qV;
  prestaf_formula  F;
};

struct prestaf_factor_st {
  prestaf_factor next;
  int a;
  ccl_ustring x;
};

struct prestaf_linear_term_st {
  uint32_t refcount;
  prestaf_factor factors;
  int b;
  uint32_t  nb_factors;
};

#endif /* __PRESTAF_FORMULA_P_H__ */
