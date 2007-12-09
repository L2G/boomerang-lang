/* $Id: linear-constraint-automaton.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __LINEAR_CONSTRAINT_AUTOMATON_H__
# define __LINEAR_CONSTRAINT_AUTOMATON_H__

# include <prestaf-formula.h>
# include <sataf-automaton.h>

# define LCA_EQ   0
# define LCA_GT   1
# define LCA_GEQ  2

/**
 * Encode a.x - b op 0 with op in { =, >, >= }
 */
extern sataf_automaton
linear_constraint_automaton_create(int type, prestaf_linear_term axb);

#endif /* __LINEAR_CONSTRAINT_AUTOMATON_H__ */
