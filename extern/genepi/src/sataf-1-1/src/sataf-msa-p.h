/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.2 $
 * @date    : $Date: 2005/12/12 11:02:16 $
 */

#ifndef __SATAF_MSA_P_H__
# define __SATAF_MSA_P_H__

# include <shared-automaton.h>

			/* --------------- */

typedef struct sataf_shared_automaton_st SSA;
struct sataf_shared_automaton_st {
  struct sataf_automaton_st super;
  shared_automaton A;
  uint32_t initial;
};

			/* --------------- */

#define MSA_IS_FINAL(msa) ((msa)->A->automaton->is_final[(msa)->initial])
#define MSA_HVALUE(sa,init) (((19*(uint32_t)(sa))<<3)+13*(init))
#define MSA_SUCC(sa,init)
#define MSAI(msa) ((sataf_msa_internal)(msa))

extern void
sataf_msa_unique_table_init(void);

extern void
sataf_msa_unique_table_terminate(void);

extern void
sataf_msa_sharing_init(void);

extern void
sataf_msa_sharing_terminate(void);

extern sataf_msa
sataf_msa_find_or_add(shared_automaton sa,
		      uint32_t initial);

extern void
sataf_msa_cache_init(void);

extern void
sataf_msa_cache_terminate(void);

#endif /* __SATAF_MSA_P_H__ */
