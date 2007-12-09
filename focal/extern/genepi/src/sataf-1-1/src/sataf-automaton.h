/* $Id: sataf-automaton.h,v 1.2 2006/01/04 08:13:30 point Exp $ */
#ifndef __SATAF_AUTOMATON_H__
# define __SATAF_AUTOMATON_H__

# include <ccl-types.h>
# include <ccl-string.h>
# include <ccl-list.h>

typedef struct sataf_automaton_st *sataf_automaton;
typedef const struct sataf_automaton_methods_st *sataf_automaton_methods;

struct sataf_automaton_st {
  sataf_automaton_methods methods; 
  uint32_t refcount;
};

struct sataf_automaton_methods_st {
  const char *type;
  uint32_t size;

  void  (*destroy)(sataf_automaton self);

  uint32_t (*get_alphabet_size)(sataf_automaton self);

  char *(*to_string)(sataf_automaton self);

  sataf_automaton (*succ)(sataf_automaton self, uint32_t letter);

  int (*is_final)(sataf_automaton self);

  int (*equals)(sataf_automaton self, sataf_automaton other);

  uint32_t (*hashcode)(sataf_automaton self);

  void (*to_dot)(sataf_automaton self, const char **alphabet, 
		 const char *graph_name, const char *graph_type);
};

			/* --------------- */

CCL_EXTERN void
sataf_automaton_init(void);

CCL_EXTERN void
sataf_automaton_terminate(void);

CCL_EXTERN sataf_automaton
sataf_automaton_create(size_t                     size, 
		       sataf_automaton_methods methods);

# define sataf_automaton_add_reference(_a) \
(((sataf_automaton)(_a))->refcount++,(_a))

# define sataf_automaton_del_reference(_a) \
do { \
  sataf_automaton __sa_aux = (_a) ; \
  ccl_pre( __sa_aux != NULL ); \
\
  if( --__sa_aux->refcount == 0 ) \
    sataf_automaton_del_reference_(__sa_aux); \
} while(0)

CCL_EXTERN void
sataf_automaton_del_reference_(sataf_automaton a);

			/* --------------- */

CCL_EXTERN ccl_ustring
sataf_automaton_get_type(sataf_automaton self);

CCL_EXTERN char *
sataf_automaton_to_string(sataf_automaton self);

# define sataf_automaton_get_type(self) \
 ((sataf_automaton)(self))->methods->type

CCL_EXTERN uint32_t 
sataf_automaton_get_alphabet_size(sataf_automaton self);

CCL_EXTERN sataf_automaton
sataf_automaton_succ(sataf_automaton self, uint32_t letter);

CCL_EXTERN int
sataf_automaton_is_final(sataf_automaton self);

CCL_EXTERN int
sataf_automaton_equals(sataf_automaton self, 
		       sataf_automaton other);

CCL_EXTERN uint32_t
sataf_automaton_hashcode(sataf_automaton self);

CCL_EXTERN void
sataf_automaton_to_dot(sataf_automaton   self, 
		       const char  **alphabet,
		       const char *graph_name,
		       const char *graph_type);

CCL_EXTERN void
sataf_automaton_default_to_dot(sataf_automaton   self, 
			       const char  **alphabet,
			       const char *graph_name,
			       const char *graph_type);

			/* --------------- */

CCL_EXTERN sataf_automaton
sataf_automaton_create_zero(uint32_t alphabet_size);

CCL_EXTERN sataf_automaton
sataf_automaton_create_one(uint32_t alphabet_size);

CCL_EXTERN sataf_automaton
sataf_automaton_create_epsilon(uint32_t alphabet_size);

CCL_EXTERN sataf_automaton
sataf_automaton_create_letter(uint32_t alphabet_size, uint32_t a);

CCL_EXTERN sataf_automaton
sataf_automaton_create_alphabet(uint32_t alphabet_size);

CCL_EXTERN sataf_automaton
sataf_automaton_create_from_arrays(uint32_t nb_states, uint32_t alphabet_size,
				   uint32_t s0, const uint8_t *is_final,
				   const uint32_t *succ);

			/* --------------- */

CCL_EXTERN sataf_automaton
sataf_automaton_create_ite(sataf_automaton i, 
			   sataf_automaton t, 
			   sataf_automaton e);

CCL_EXTERN sataf_automaton
sataf_automaton_create_not(sataf_automaton a); 

CCL_EXTERN sataf_automaton
sataf_automaton_create_or(sataf_automaton a1, sataf_automaton a2);

CCL_EXTERN sataf_automaton
sataf_automaton_create_and(sataf_automaton a1, sataf_automaton a2);

CCL_EXTERN sataf_automaton
sataf_automaton_create_multi_or(ccl_list operands);

CCL_EXTERN sataf_automaton
sataf_automaton_create_multi_and(ccl_list operands);

CCL_EXTERN sataf_automaton
sataf_automaton_create_imply(sataf_automaton a1, sataf_automaton a2);

CCL_EXTERN sataf_automaton
sataf_automaton_create_equiv(sataf_automaton a1, sataf_automaton a2);


			/* --------------- */

CCL_EXTERN sataf_automaton
sataf_automaton_create_forall(sataf_automaton a);

CCL_EXTERN sataf_automaton
sataf_automaton_create_exists(sataf_automaton a);

#endif /* __SATAF_AUTOMATON_H__ */
