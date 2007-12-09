/* $Id: sataf-shared-automaton.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __SATAF_SHARED_AUTOMATON_H__
# define __SATAF_SHARED_AUTOMATON_H__

typedef struct shared_automaton_st *sataf_sa;
typedef struct exit_automaton_st *sataf_ea;

typedef void
(*sataf_shared_automaton_transformer)(sataf_ea ea,
				      sataf_sa *bind_sa,
				      uint32_t *bind_init);
				      
# define SSA_EXISTS sataf_shared_automaton_exists_closure
# define SSA_FORALL sataf_shared_automaton_forall_closure

extern void
sataf_shared_automaton_exists_closure(sataf_ea ea,
				      sataf_sa *bind_sa,
				      uint32_t *bind_init);

extern void
sataf_shared_automaton_forall_closure(sataf_ea ea,
				      sataf_sa *bind_sa,
				      uint32_t *bind_init);

#endif /* __SATAF_SHARED_AUTOMATON_H__ */
