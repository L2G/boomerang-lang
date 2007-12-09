/* $Id: sataf-memo.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __SATAF_MEMO_H__
# define __SATAF_MEMO_H__

# include <sataf-automaton.h>

typedef struct sataf_memorizer_st *sataf_memorizer;
typedef struct sataf_memo_st sataf_memo;
struct sataf_memo_st {
  sataf_automaton A;  
};

typedef void (*sataf_memo_init_proc)(sataf_memo *m, void *data);
typedef void (*sataf_memo_clean_proc)(sataf_memo *m);

CCL_EXTERN sataf_memorizer
sataf_memorizer_create(size_t memo_size, sataf_memo_init_proc init,
		       void *init_data, sataf_memo_clean_proc clean);

CCL_EXTERN void
sataf_memorizer_delete(sataf_memorizer M);

CCL_EXTERN sataf_memo *
sataf_memorizer_remind(sataf_memorizer M, sataf_automaton A);

CCL_EXTERN sataf_memo *
sataf_memorizer_succ(sataf_memorizer M, sataf_automaton A, int letter);

#endif /* __SATAF_MEMO_H__ */
