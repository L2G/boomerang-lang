/* $Id: sataf-msa.h,v 1.5 2006/03/09 16:34:36 point Exp $ */
/**
 * @file
 */
#ifndef __SATAF_MSA_H__
# define __SATAF_MSA_H__

# include <ccl-log.h>
# include <ccl-assert.h>
# include <sataf-automaton.h>
# include <sataf-shared-automaton.h>

typedef struct sataf_msa_st *sataf_msa;

/**
 * @brief Structure encoding a Marked Shared Automaton.
 */
struct sataf_msa_st {
  uint32_t refcount;
  sataf_sa          A;
  uint32_t  initial;
  sataf_msa     next;
};

typedef struct sataf_msa_info_st {
  uint32_t nb_shared_automata;
  uint32_t nb_exit_automata;
  uint32_t nb_states;
  uint32_t depth;
  uint32_t refcount;
} sataf_msa_info;

CCL_EXTERN const char *SATAF_MSA_ID;

			/* --------------- */

CCL_EXTERN void
sataf_msa_init(void);

CCL_EXTERN void
sataf_msa_terminate(void);

			/* --------------- */

CCL_EXTERN sataf_msa
sataf_msa_zero(uint32_t alphabet_size);

# define sataf_msa_empty sataf_msa_zero

CCL_EXTERN sataf_msa
sataf_msa_one(uint32_t alphabet_size);

# define sataf_msa_all sataf_msa_one

CCL_EXTERN sataf_msa
sataf_msa_epsilon(uint32_t alphabet_size);

CCL_EXTERN sataf_msa
sataf_msa_letter(uint32_t alphabet_size, uint32_t a);

CCL_EXTERN sataf_msa
sataf_msa_alphabet(uint32_t alphabet_size);

			/* --------------- */

# define sataf_msa_compute(A) sataf_msa_compute_with_transformer(A,NULL)

CCL_EXTERN sataf_msa
sataf_msa_compute_with_transformer(sataf_automaton                    A, 
				   sataf_shared_automaton_transformer T);

CCL_EXTERN sataf_automaton 
sataf_msa_to_automaton(sataf_msa msa);

CCL_EXTERN void
sataf_msa_log_info(ccl_log_type log, sataf_msa msa);

CCL_EXTERN void
sataf_msa_get_info(sataf_msa msa, sataf_msa_info *info);

			/* --------------- */

CCL_EXTERN sataf_msa
sataf_msa_succ(sataf_msa msa, uint32_t a);

CCL_EXTERN int
sataf_msa_is_final(sataf_msa msa);

CCL_EXTERN uint32_t
sataf_msa_get_alphabet_size(sataf_msa msa);

			/* --------------- */

# define sataf_msa_add_reference(msa) \
(ccl_pre((msa)!=NULL), (msa)->refcount++,(msa))

# define sataf_msa_del_reference(msa) \
do { \
  sataf_msa __msa_aux = (msa); \
  ccl_pre( __msa_aux != NULL ); \
  ccl_pre( __msa_aux->refcount > 0 ); \
  \
  if( --__msa_aux->refcount == 0 ) \
     sataf_msa_del_reference_(__msa_aux); \
} while(0)

CCL_EXTERN void
sataf_msa_del_reference_(sataf_msa msa);

			/* --------------- */

CCL_EXTERN void
sataf_msa_display_as_dot(sataf_msa msa, ccl_log_type log, 
			 const char **alphabet);

CCL_EXTERN void
sataf_msa_display_as_olddot(sataf_msa msa, ccl_log_type log, 
			    const char **alphabet);

			/* --------------- */

CCL_EXTERN int
sataf_msa_is_zero(sataf_msa msa);

CCL_EXTERN int
sataf_msa_is_one(sataf_msa msa);

CCL_EXTERN int
sataf_msa_is_zero_or_one(sataf_msa msa);

			/* --------------- */

CCL_EXTERN sataf_msa
sataf_msa_ite(sataf_msa i, sataf_msa t, sataf_msa e);

CCL_EXTERN sataf_msa
sataf_msa_not(sataf_msa a); 

CCL_EXTERN sataf_msa
sataf_msa_or(sataf_msa a1, sataf_msa a2);

CCL_EXTERN sataf_msa
sataf_msa_and(sataf_msa a1, sataf_msa a2);

CCL_EXTERN sataf_msa
sataf_msa_imply(sataf_msa a1, sataf_msa a2);

CCL_EXTERN sataf_msa
sataf_msa_equiv(sataf_msa a1, sataf_msa a2);

CCL_EXTERN int
sataf_msa_is_included_in(sataf_msa a1, sataf_msa a2);

			/* --------------- */

CCL_EXTERN sataf_msa
sataf_msa_forall(sataf_msa a);

CCL_EXTERN sataf_msa
sataf_msa_exists(sataf_msa a);

			/* --------------- */

CCL_EXTERN sataf_msa
sataf_msa_concat(sataf_msa a1, sataf_msa a2);

CCL_EXTERN sataf_msa
sataf_msa_power(sataf_msa a, int N); 

CCL_EXTERN sataf_msa
sataf_msa_star(sataf_msa a);

			/* --------------- */

extern sataf_msa
sataf_msa_cache_get(sataf_msa a1, sataf_msa a2, sataf_msa a3, const void *aux);

extern void
sataf_msa_cache_put(sataf_msa a1, sataf_msa a2, sataf_msa a3, const void *aux,
		    sataf_msa R);

			/* --------------- */

extern void
sataf_msa_ut_statistics(ccl_log_type log);

extern void
sataf_msa_cache_statistics(ccl_log_type log);

extern void
sataf_msa_sharing_cache_statistics(ccl_log_type log);

			/* --------------- */

#endif /* __SATAF_MSA_H__ */
