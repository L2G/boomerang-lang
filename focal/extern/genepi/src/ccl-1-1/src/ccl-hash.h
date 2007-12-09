/* $Id: ccl-hash.h,v 1.2 2005/11/15 15:04:50 point Exp $ */
#ifndef __CCL_HASH_H__
# define __CCL_HASH_H__

# include <ccl-globals.h>
# include <ccl-protos.h>
# include <ccl-iterator.h>

typedef struct ccl_hash_st *ccl_hash;
typedef struct ccl_hash_entry_st {
  void *key;
  void *object;
} ccl_hash_entry;


CCL_DECLARE_ITERATOR(ccl_hash_entry_iterator,ccl_hash_entry);

CCL_EXTERN ccl_hash
ccl_hash_create(ccl_hash_func key_hash,
		ccl_compare_func key_compare,
		ccl_delete_proc key_delete,
		ccl_delete_proc object_delete);

CCL_EXTERN void
ccl_hash_delete(ccl_hash ht);

CCL_EXTERN int
ccl_hash_find(ccl_hash ht, void *key);

CCL_EXTERN void *
ccl_hash_get(ccl_hash ht);

CCL_EXTERN void
ccl_hash_insert(ccl_hash ht, void *object);

CCL_EXTERN void
ccl_hash_remove(ccl_hash ht);

CCL_EXTERN int
ccl_hash_size(ccl_hash ht);

			/* --------------- */

CCL_EXTERN ccl_pointer_iterator *
ccl_hash_get_keys(ccl_hash ht);

CCL_EXTERN ccl_pointer_iterator *
ccl_hash_get_elements(ccl_hash ht);

CCL_EXTERN ccl_hash_entry_iterator *
ccl_hash_get_entries(ccl_hash ht);

#endif /* __CCL_HASH_H__ */
