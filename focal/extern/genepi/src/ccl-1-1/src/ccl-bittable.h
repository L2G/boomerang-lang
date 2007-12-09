/* $Id: ccl-bittable.h,v 1.3 2006/03/06 07:18:14 point Exp $ */
#ifndef __CCL_BITTABLE_H__
# define __CCL_BITTABLE_H__

# include <ccl-globals.h>
# include <ccl-iterator.h>

typedef struct ccl_bittable_st *ccl_bittable;

CCL_EXTERN ccl_bittable
ccl_bittable_create(int size);

CCL_EXTERN void
ccl_bittable_delete(ccl_bittable bt);

CCL_EXTERN int
ccl_bittable_size(ccl_bittable bt);

CCL_EXTERN int
ccl_bittable_has(ccl_bittable bt, int index);

CCL_EXTERN void
ccl_bittable_set(ccl_bittable bt, int index);

CCL_EXTERN void
ccl_bittable_unset(ccl_bittable bt, int index);

CCL_EXTERN int
ccl_bittable_get_first(ccl_bittable bt);

CCL_EXTERN int
ccl_bittable_get_next(ccl_bittable bt, int prev);

CCL_EXTERN void
ccl_bittable_clear(ccl_bittable bt);

CCL_EXTERN unsigned int
ccl_bittable_hash(const ccl_bittable bt);

CCL_EXTERN int
ccl_bittable_equals(const ccl_bittable bt1, const ccl_bittable bt2);

CCL_EXTERN ccl_bittable
ccl_bittable_dup(const ccl_bittable bt);

CCL_EXTERN int
ccl_bittable_get_nb_one(const ccl_bittable bt);

CCL_EXTERN ccl_int_iterator *
ccl_bittable_get_ones(ccl_bittable bt);

CCL_EXTERN ccl_bittable 
ccl_bittable_resize(ccl_bittable bt, int newsize);

CCL_EXTERN ccl_bittable
ccl_bittable_union(ccl_bittable bt1, ccl_bittable bt2);

CCL_EXTERN ccl_bittable
ccl_bittable_intersection(ccl_bittable bt1, ccl_bittable bt2);

CCL_EXTERN ccl_bittable
ccl_bittable_complement(ccl_bittable bt);

#endif /* __CCL_BITTABLE_H__ */
