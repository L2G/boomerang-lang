/* $Id: shared-automaton.h,v 1.3 2006/01/04 08:08:24 point Exp $ */
#ifndef __SHARED_AUTOMATON_H__
# define __SHARED_AUTOMATON_H__

# include <sataf-msa.h>
# include <exit-automaton.h>

typedef struct shared_automaton_st *shared_automaton;
struct shared_automaton_st {
  shared_automaton    next;
  uint32_t      refcount;
  exit_automaton automaton;
  uint32_t         depth;
  sataf_msa           bind[1];
};

extern void
shared_automaton_init(void);

extern void
shared_automaton_terminate(void);

# define shared_automaton_add_reference(_sa) \
(ccl_pre( (_sa) != NULL ), (_sa)->refcount++,(_sa))

# define shared_automaton_del_reference(_sa) \
do { \
  shared_automaton __sa_aux = (_sa); \
  ccl_pre( __sa_aux != NULL ); \
  ccl_pre( __sa_aux->refcount > 0 ); \
  \
  if( --__sa_aux->refcount == 0 ) \
    shared_automaton_del_reference_(__sa_aux); \
} while(0)

extern void
shared_automaton_del_reference_(shared_automaton sa);

extern shared_automaton
shared_automaton_create_one(uint32_t alphabet_size);

extern shared_automaton
shared_automaton_create_zero(uint32_t alphabet_size);

extern shared_automaton
shared_automaton_find_or_add(exit_automaton ea, 
			     shared_automaton *bind_sa,
			     uint32_t *bind_init);

extern void
shared_automaton_display_as_dot(shared_automaton sa, ccl_log_type log, 
				int marked_state, const char **alphabet);

extern void
shared_automaton_display_scc_as_dot(shared_automaton sa, ccl_log_type log);

# define shared_automaton_is_zero(sa) exit_automaton_is_zero((sa)->automaton)

# define shared_automaton_is_one(sa) exit_automaton_is_one((sa)->automaton)

# define shared_automaton_is_zero_or_one(sa) \
 exit_automaton_is_zero_or_one((sa)->automaton)

extern void
shared_automaton_table_statistics(ccl_log_type log);

#endif /* __SHARED_AUTOMATON_H__ */
