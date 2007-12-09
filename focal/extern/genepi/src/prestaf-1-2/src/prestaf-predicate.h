/* $Id: prestaf-predicate.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
/**
 * @file 
 * @brief  This module allows the manipulation of predicates for which 
 * variables take their value into \f${\cal N}\f$. The relation underlying a 
 * predicate is encoded using a ::sataf_msa.
 *
 * The module offers Boolean operations over these predicates  and a pretty
 * printer of the elements of the relation (in the finite case).  
 * When applying a binary operation on predicates (e.g prestaf_predicate_or()),
 * the operands are considered to be defined over the same set of variables; if
 * this is not the case they are extended to fit this requirement.
 */
#ifndef __PRESTAF_PREDICATE_H__
# define __PRESTAF_PREDICATE_H__

# include <prestaf-formula.h>

/**
 * @brief  Structure representing a predicate. The variables of the predicate
 * take their value into \f${\cal N}\f$.
 *
 * This structure associates a relation encoded using a marked-shared 
 * automaton (see ::sataf_msa) and a list of names. The width of the relation 
 * is the size of the list and the <i>i<sup>th</sup></i> column is labelled by 
 * the name with the index <i>i</i> in then list.
 */
typedef struct prestaf_predicate_st *prestaf_predicate;


/**
 * @brief Contructor of predicates. 
 *
 * This function associates a list of names and a relation encoded with a
 * ::sataf_msa automaton. 
 *
 * @param vars is a list of ::ccl_ustring. These names label the columns of 
 * the relation <i>rel</i>.
 * @param rel is the relation encoding the assignments satisfying the 
 * predicate.
 * @pre vars != NULL && rel != NULL
 * @return The newly allocated predicate.
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_create(ccl_list vars, sataf_msa rel);

/**
 * @brief Increments the counter of reference of <i>P</i>.
 *
 * @param P the referenced predicate
 * @pre P != NULL
 * @return the argument P
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_add_reference(prestaf_predicate P);

/**
 * @brief Decrements the counter of reference of <i>P</i>. 
 *
 * If the counter becomes 0 then the structure is deallocated.
 * @param P the dereferenced predicate
 * @pre P != NULL && the number of reference to P is > 0
 */
CCL_EXTERN void
prestaf_predicate_del_reference(prestaf_predicate P);

/**
 * @brief Variables of a predicate
 *
 * @pre P != NULL
 * @return The (internal) ordered list of variables of <i>P</i>
 */
CCL_EXTERN ccl_list
prestaf_predicate_get_variables(prestaf_predicate P);

/**
 * @brief Relation of a predicate
 *
 * @pre P != NULL
 * @return The ::sataf_msa encoding the set of vectors represented by <i>P</i>
 */
CCL_EXTERN sataf_msa
prestaf_predicate_get_relation(prestaf_predicate P);

/**
 * @brief Relabelling of the variables. 
 *
 * This function simply replaces the list of variables of the predicate 
 * <i>P</i> by the list <i>vars</i>.
 *
 * @param P the relabelled predicate.
 * @param vars the new labelling list.
 *
 * @pre P != NULL && vars != NULL 
 */
CCL_EXTERN void
prestaf_predicate_rename(prestaf_predicate P, ccl_list vars);

/**
 * @brief Compute the complement of a predictate <i>P</i>
 *
 * @param P the predicate to be complemented.
 * @pre P != NULL
 * @return The complement of <i>P</i>
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_not(prestaf_predicate P);

/**
 * @brief Binary disjunction.
 *
 * @param P1 first operand of the disjunction.
 * @param P2 second operand of the disjunction.
 * @pre P1 != NULL && P2 != NULL
 * @return A predicate representing the union of the two sets represented by 
 * <i>P1</i> and <i>P2</i>.
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_or(prestaf_predicate P1, prestaf_predicate P2);

/**
 * @brief Binary conjunction.
 *
 * @param P1 first operand of the conjunction.
 * @param P2 second operand of the conjunction.
 * @pre P1 != NULL && P2 != NULL
 * @return A predicate representing the intersection of the two sets 
 * represented by <i>P1</i> and <i>P2</i>.
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_and(prestaf_predicate P1, prestaf_predicate P2);

/**
 * @brief N-ary disjunction.
 *
 * @param predicates the list of predicates on which the disjunction is
 * applied.
 * @pre predicates != NULL && ccl_list_size(predicates) >= 1 
 * @return The disjunction of elements in <i>predicates</i>.
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_multi_or(ccl_list predicates);

/**
 * @brief N-ary conjunction.
 *
 * @param predicates the list of predicates on which the conjunction is
 * applied.
 * @pre predicates != NULL && ccl_list_size(predicates) >= 1 
 * @return The conjunction of elements in <i>predicates</i>.
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_multi_and(ccl_list predicates);

/**
 * @brief Logical implication.
 *
 * This operation computes \f$\overline{P1}\cup P2\f$
 * @param P1 first operand of the implication
 * @param P2 first operand of the implication
 * @pre P1 != NULL && P2 != NULL
 * @return The predicate encoding \f$\overline{P1}\cup P2\f$
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_imply(prestaf_predicate P1, prestaf_predicate P2);

/**
 * @brief Logical equality
 *
 * This operation computes \f$P1\cap P2 \cup \overline{P1}\cap \overline{P2}\f$
 * @param P1 first operand of the equality
 * @param P2 first operand of the equality
 * @pre P1 != NULL && P2 != NULL
 * @return The predicate encoding the equality
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_equiv(prestaf_predicate P1, prestaf_predicate P2);

/**
 * @brief Existential projection.
 *
 * @param v the name of then variable removed from the predicate.
 * @param P the predicate that is projected.
 * @pre v != NULL && P != NULL
 * @return The predicate equal to <i>P</i> existentially projected on
 * variables of <i>P</i> without <i>v</i>
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_exists(ccl_ustring v, prestaf_predicate P);

/**
 * @brief Universal projection.
 *
 * @param v the name of variable removed from the predicate.
 * @param P the predicate that is projected.
 * @pre v != NULL && P != NULL
 * @return The predicate equal to <i>P</i> universally projected on 
 * variables of <i>P</i> without <i>v</i>
 */
CCL_EXTERN prestaf_predicate
prestaf_predicate_forall(ccl_ustring v, prestaf_predicate P);

/**
 * @brief Display the assignments belonging to the predicate ::P. 
 *
 * This function works only for finite subsets of \f${\cal N}^n\f$;
 * if the set represented by <i>P</i> is infinite then nothing is displayed.
 *
 * The displayed verctors are mappings from the variables of <i>P</i> into 
 * \f$\cal N\f$. 
 *
 * @param P the predicate to be displayed.
 *
 * @pre P != NULL
 * @return 1 if the set has been correctly displayed;
 *         0 if the set is infinite.
 *
 * @bug this procedure doesn't properly for infinite sets.
 */
CCL_EXTERN int
prestaf_predicate_display_vectors(prestaf_predicate P);

#endif /* __PRESTAF_PREDICATE_H__ */
