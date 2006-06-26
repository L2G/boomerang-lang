/* $Id*/
/**
 * @file
 * @brief This module implements an encoding of Presburger formulae. It 
 * computes also for a given formula its set of solutions using Shared 
 * Automata.
 *
 * Presburger formulae are syntactically defined by:
 * 
 * F ::= T == T | T != T | T < T | T <= T | T > T | T >= T | 
 *       -F | F \/ F | F /\ F | F => F | F <=> F | E.x F | A.x F
 *
 * <i>E.</i> and <i>A.</i> operators are respectively existential and universal
 * quantification. <i>T </i> represents a linear term defined syntactically by:
 *
 * T ::= c | a.x | T+T | T-T | -T
 * 
 * where <i>c</i> and <i>a</i> are an integers and <i>x</i> is a variable taken
 * its value into natural numbers \f$\cal N\f$.
 */
#ifndef __PRESTAF_FORMULA_H__
# define __PRESTAF_FORMULA_H__

# include <sataf.h>
# include <prestaf-predicate.h>

/**
 * @brief Pointers to Presburger formula.
 *
 * This type defines pointers to structures encoding Presburger formulae. These
 * formula are syntactically defined by:
 * 
 * F ::= T == T | T != T | T < T | T <= T | T > T | T >= T | 
 *       -F | F \/ F | F /\ F | F => F | F <=> F | E.x F | A.x F
 *
 * <i>E.</i> and <i>A.</i> operators are respectively existential and universal
 * quantification. <i>T </i> represents a linear term defined syntactically by:
 *
 * T ::= c | a.x | T+T | T-T | -T
 * 
 * where <i>c</i> and <i>a</i> are an integers and <i>x</i> is a variable taken
 * its value into natural numbers \f$\cal N\f$.
 *
 * ::prestaf_formula uses a counter of references.
 *
 * @see ::prestaf_linear_term
 */
typedef struct prestaf_formula_st *prestaf_formula;

/**
 * @brief Pointers to linear terms.
 *
 * This type defines pointers to structures encoding linear terms of the form
 * <i>a<sub>0</sub>.x<sub>0</sub></i>+...+<i>a<sub>n</sub>.x<sub>n</sub></i>+
 * <i>b</i> where <i>a<sub>i</sub></i> and <i>b</i> are integers and 
 * <i>x<sub>i</sub></i> are variables.
 *
 * ::prestaf_linear_term uses a counter of references.
 */
typedef struct prestaf_linear_term_st *prestaf_linear_term;


/**
 * @brief Free variables of a formula.
 * 
 * This function returns a list containing all the variables appearing in the
 * given formula <i>f</i>.
 *
 * @param f the formula 
 * @pre f != NULL
 * @return The list of free variables appearing in the formula <i>f</i>. This 
 * list * has to be deleted by the client code.
 */
CCL_EXTERN ccl_list
prestaf_formula_get_variables(prestaf_formula f);

/**
 * @brief Increments the reference counter of a formula.
 *
 * @param f the formual to be referenced
 * @pre f != NULL toto
 * @return f
 */
CCL_EXTERN prestaf_formula
prestaf_formula_add_reference(prestaf_formula f);

/**
 * @brief Decrements the reference counter of a a formula.
 *
 * When the reference counter of the object falls to 0 then the memory
 * allocated for the formula is released.
 *
 * @param f the formula to be dereferenced
 * @pre f != NULL and there exists at least 1 reference to this formula.
 */
CCL_EXTERN void
prestaf_formula_del_reference(prestaf_formula f);

/**
 * @brief Textual display of a Presburger formula.
 * @param f the formula to be displayed
 * @pre f != NULL
 */
CCL_EXTERN void
prestaf_formula_display(prestaf_formula f);

/**
 * @brief Getting solutions of a Presbruger formula
 *
 * @pre f != NULL 
 * @return a ::prestaf_predicate encoding the solutions of the formula <i>f</i>
 */
CCL_EXTERN prestaf_predicate
prestaf_formula_solutions(prestaf_formula f);


/**
 * @brief Formulae disjunction
 * @param f1 first operand
 * @param f2 second operand
 * @pre f1 != NULL && f2 != NULL 
 * @return a formulat encoding f1 \/ f2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_or(prestaf_formula f1, prestaf_formula f2);

/**
 * @brief Formulae conjunction
 * @param f1 first operand
 * @param f2 second operand
 * @pre f1 != NULL && f2 != NULL 
 * @return a formulat encoding f1 /\ f2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_and(prestaf_formula f1, prestaf_formula f2);

/**
 * @brief Formulae equivalence
 * @param f1 first operand
 * @param f2 second operand
 * @pre f1 != NULL && f2 != NULL 
 * @return a formulat encoding f1 <=> f2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_equiv(prestaf_formula f1, prestaf_formula f2);

/**
 * @brief Formulae implication
 * @param f1 first operand
 * @param f2 second operand
 * @pre f1 != NULL && f2 != NULL 
 * @return a formulat encoding f1 => f2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_imply(prestaf_formula f1, prestaf_formula f2);

/**
 * @brief Formula negation
 * @param f first operand
 * @pre f != NULL
 * @return a formulat encoding -f
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_not(prestaf_formula f);

/**
 * @brief Universal quantification of a formula
 *
 * @param variables the list of quantified variables
 * @param f the formula to quantify
 * @pre variables != NULL && f != NULL
 * @pre ccl_list_get_size(variables) > 0
 * @return A formula encoding A.x0 ... A.xn f where x0 ... xn are variables
 *         listed in <i>variables</i>.
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_forall(ccl_list variables, prestaf_formula f);

/**
 * @brief Existential quantification of a formula
 *
 * @param variables the list of quantified variables
 * @param f the formula to quantify
 * @pre variables != NULL && f != NULL
 * @pre ccl_list_get_size(variables) > 0
 * @return A formula encoding E.x0 ... E.xn f where x0 ... xn are variables
 *         listed in <i>variables</i>.
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_exists(ccl_list variables, prestaf_formula f);

/**
 * @brief Constructor for equality (==) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 <= t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_eq(prestaf_linear_term t1, prestaf_linear_term  t2);

/**
 * @brief Constructor for disequality (!=) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 != t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_neq(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Constructor for strict inferiority (<) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 < t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_lt(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Constructor for large inferiority (<=) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 <= t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_leq(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Constructor for strict superiority (>) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 > t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_gt(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Constructor for weak superiority (>=) of linear terms
 *
 * @param t1 first operand 
 * @param t2 second operand 
 * @pre t1 != NULL && t2 != NULL
 * @return a ::prestaf_formula encoding t1 >= t2
 */
CCL_EXTERN prestaf_formula
prestaf_formula_create_geq(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Constructor for linear terms.
 *
 * This function allocates a structure encoding a linear term of the
 * form <i>a[0]*x[0]+...+a[n-1]*x[n-1]+b</i>.
 *
 * @param n the number of variables in this term.
 * @param a the coefficient of each sub-term.
 * @param x the names of variables.
 * @param b the constant of the term?
 *
 * @pre n >= 1 => (a != NULL && x != NULL)
 * @return A newly allocate linear term encoding 
 * <i>a[0]*x[0]+...+a[n-1]*x[n-1]+b</i>.
 */
CCL_EXTERN prestaf_linear_term
prestaf_linear_term_create(uint32_t n, int *a, ccl_ustring *x, int b);

/**
 * @brief Add two linear terms.
 *
 * This operation computes the sum of its two arguments. The set of variables 
 * of the two terms are merged before the operation.
 *
 * @param t1 the first linear term
 * @param t2 the second linear term
 * @pre t1 != NULL && t2 != NULL
 *
 * @return <i>t1 + t2</i>
 */
CCL_EXTERN prestaf_linear_term
prestaf_linear_term_plus(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Substract two linear terms.
 *
 * This operation computes the substraction of its two arguments. The set of 
 * variables of the two terms are merged before the operation.
 *
 * @param t1 the first linear term.
 * @param t2 the second linear term.
 * @pre t1 != NULL && t2 != NULL
 *
 * @return <i>t1 - t2</i>
 */
CCL_EXTERN prestaf_linear_term
prestaf_linear_term_minus(prestaf_linear_term t1, prestaf_linear_term t2);

/**
 * @brief Negate a linear term
 *
 * This operation negates its two arguments. 
 *
 * @param t the first linear term
 * @pre t != NULL 
 * @return <i> - t</i>
 */
CCL_EXTERN prestaf_linear_term
prestaf_linear_term_neg(prestaf_linear_term t);

/**
 * @brief Increments the reference counter of a linear term.
 *
 * @param t the term to be referenced
 * @pre t != NULL 
 * @return t
 */
CCL_EXTERN prestaf_linear_term
prestaf_linear_term_add_reference(prestaf_linear_term t);

/**
 * @brief Decrements the reference counter of a linear term.
 *
 * When the reference counter of the object falls to 0 then the memory
 * allocated for the term is released.
 *
 * @param t the term to be dereferenced
 * @pre t != NULL and there exists at least 1 reference to this term.
 */
CCL_EXTERN void
prestaf_linear_term_del_reference(prestaf_linear_term t);

/**
 * @brief Getting coefficient of a linear term
 *
 * This function returns the coefficient of the variable <i>x</i> in <i>t</i>.
 *
 * @param t the considered term.
 * @param x the variable for which one want to get the coefficent..
 * @pre t != NULL && x != NULL
 * @return the coefficient of <i>x</i> in <i>t</i>; if x doesn't appear in 
 * <i>t</i> the function return 0.
 */
CCL_EXTERN int
prestaf_linear_term_get_coef(prestaf_linear_term t, ccl_ustring x);

/**
 * @brief Getting coefficient of a linear term.
 *
 * This function returns the <i>i</i><sup>th</sup> coefficient in <i>t</i> (the
 * constant is not considered here). The subterms are ordered using the address
 * of the variable names.
 * 
 * @param t the considered term.
 * @param i the index for which one want to get the coefficent.
 * @pre t != NULL && i < the number of subterms
 * @return the <i>i</i><sup>th</sup> coefficient in <i>t</i>.
 */
CCL_EXTERN int
prestaf_linear_term_get_ith_coef(prestaf_linear_term t, uint32_t i);

/**
 * @brief Display informations about the ::sataf_msa encoding the solutions
 * of Presburger formula.
 * 
 * @param F a Presburger formula.
 * @param log the log-stream where informations are displayed.
 * @pre F != NULL
 */
CCL_EXTERN void
prestaf_formula_statistics(prestaf_formula F, ccl_log_type log);

#endif /* __PRESTAF_FORMULA_H__ */
