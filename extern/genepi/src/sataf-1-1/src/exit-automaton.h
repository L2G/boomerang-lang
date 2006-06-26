/* $Id: exit-automaton.h,v 1.3 2006/01/04 08:02:38 point Exp $ */
#ifndef __EXIT_AUTOMATON_H__
# define __EXIT_AUTOMATON_H__

# include <ccl-types.h>
# include <ccl-assert.h>
# include <ccl-log.h>

typedef struct exit_automaton_st *exit_automaton;
struct exit_automaton_st {
  exit_automaton next;
  uint32_t refcount;
  uint32_t alphabet_size;
  uint32_t nb_local_states;
  uint32_t nb_exit_states;
  uint8_t *is_final;
  uint32_t successor[1];
};

# define exit_automaton_encode_succ_as_local_state(index) ((index)<<1)
# define exit_automaton_encode_succ_as_exit_state(index)  (1+((index)<<1))
# define exit_automaton_decode_succ_state(index) ((index)>>1)

# define exit_automaton_is_exit_state(index) (((index)&0x1) != 0)
# define exit_automaton_is_local_state(index) \
  (!exit_automaton_is_exit_state(index))

extern void
exit_automaton_init(void);

extern void
exit_automaton_terminate(void);

extern exit_automaton 
exit_automaton_create(uint32_t nb_states, 
		      uint32_t nb_exits, 
		      uint32_t alphabet_size);

extern exit_automaton 
exit_automaton_complement(exit_automaton ea);

extern exit_automaton 
exit_automaton_create_with_arrays(uint32_t nb_states, 
				  uint32_t nb_exits, 
				  uint32_t alphabet_size,
				  const uint8_t *is_final,
				  const uint32_t *succ);

extern exit_automaton
exit_automaton_add_reference(exit_automaton ea);

extern void
exit_automaton_del_reference(exit_automaton ea);

extern uint32_t
exit_automaton_successor(exit_automaton ea, 
			 uint32_t  state, 
			 uint32_t letter);

# define exit_automaton_successor(_ea, _state, _letter) \
  (ccl_pre( (_ea) != NULL ), ccl_pre( (_state) < (_ea)->nb_local_states ), \
  ccl_pre( (_letter) < (_ea)->alphabet_size ),			\
 (_ea)->successor[(_ea)->alphabet_size*(_state)+(_letter)])

extern void
exit_automaton_set_successor(exit_automaton ea, 
			     uint32_t src, 
			     uint32_t letter,
			     uint32_t tgt,
			     int exit);

extern void
exit_automaton_display_as_dot(exit_automaton ea, 
			      ccl_log_type log,
			      const char **alphabet);

extern exit_automaton
exit_automaton_minimize(exit_automaton ea, 
			uint32_t *h);

extern exit_automaton
exit_automaton_find_or_add(exit_automaton ea);

extern int
exit_automaton_find(exit_automaton ea);

# define exit_automaton_is_zero(ea) \
  (exit_automaton_is_zero_or_one(ea) && !(ea)->is_final[0])

# define exit_automaton_is_one(ea) \
  (exit_automaton_is_zero_or_one(ea) && (ea)->is_final[0])


# define exit_automaton_is_zero_or_one(ea) \
  ((ea)->nb_local_states == 1 && (ea)->nb_exit_states == 0)


extern int
exit_automaton_equals(exit_automaton ea1, exit_automaton ea2);

extern uint32_t
exit_automaton_hashcode(exit_automaton ea);

extern void
exit_automaton_ut_statistics(ccl_log_type log);

#endif /* __EXIT_AUTOMATON_H__ */
