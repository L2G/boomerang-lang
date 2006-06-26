/* $Id: quantifier-automaton.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __QUANTIFIER_AUTOMATON_H__
# define __QUANTIFIER_AUTOMATON_H__

# include <prestaf-formula.h>
# include <sataf-automaton.h>

extern sataf_automaton
quantifier_automaton_create(int forall,
			    uint32_t i, uint32_t n, sataf_msa fset);

#endif /* __QUANTIFIER_AUTOMATON_H__ */
