/* $Id: hopcroft2.h,v 1.1 2006/01/04 08:15:59 point Exp $ */
#ifndef __HOPCROFT2_H__
# define __HOPCROFT2_H__

# include "exit-automaton.h"

extern void
hopcroft_init2(void);

extern void
hopcroft_terminate2(void);

extern exit_automaton
exit_automaton_minimize2(exit_automaton ea, 
			 uint32_t *h);

#endif /* __HOPCROFT2_H__ */
