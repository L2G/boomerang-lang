/* $Id: add-variable-automaton.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __ADD_VARIABLE_AUTOMATON_H__
# define __ADD_VARIABLE_AUTOMATON_H__
 
# include "prestaf-formula.h"
# include <sataf-automaton.h>

extern sataf_automaton
add_variable_automaton_create(sataf_msa msa, uint32_t i, uint32_t n);

#endif /* __ADD_VARIABLE_AUTOMATON_H__ */
