/* $Id: apply-automaton.h,v 1.2 2006/01/24 13:03:51 point Exp $ */
#ifndef __APPLY_AUTOMATON_H__
# define __APPLY_AUTOMATON_H__

# include <prestaf-formula.h>
# include <sataf-automaton.h>

extern sataf_automaton
apply_automaton_create(sataf_msa R, sataf_msa A);

extern sataf_automaton
apply_automaton_create_inverse(sataf_msa R, sataf_msa A);

#endif /* __APPLY_AUTOMATON_H__ */
