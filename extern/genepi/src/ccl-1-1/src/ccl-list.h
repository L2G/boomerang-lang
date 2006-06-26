/* $Id: ccl-list.h,v 1.4 2006/01/04 07:58:36 point Exp $ */
/**
 * @file
 */
#ifndef __CCL_LIST_H__
# define __CCL_LIST_H__

# include <ccl-protos.h>

typedef struct ccl_list_st *ccl_list;
typedef struct ccl_pair_st *ccl_pair;

struct ccl_list_st {
  int       size;
  ccl_pair  first;
  ccl_pair *last;
};

struct ccl_pair_st {
  void    *car;
  ccl_pair cdr;
};

# define FIRST(l) ((l)->first)
# define CAR(p)   ((p)->car)
# define CDR(p)   ((p)->cdr)
# define CADR(p)  (CAR(CDR(p)))
# define CDDR(p)  (CDR(CDR(p)))
# define CADDR(p) (CAR(CDDR(p)))
# define CDDDR(p) (CDR(CDDR(p)))
# define CADDDR(p) (CAR(CDDDR(p)))
# define CDDDDR(p) (CDR(CDDDR(p)))
# define CADDDDR(p) (CAR(CDDDDR(p)))
# define CDDDDDR(p) (CDR(CDDDDR(p)))

CCL_EXTERN ccl_list
ccl_list_create(void);

CCL_EXTERN void
ccl_list_delete(ccl_list l);

CCL_EXTERN void
ccl_list_clear(ccl_list l, ccl_delete_proc del);

CCL_EXTERN void
ccl_list_clear_and_delete(ccl_list l, ccl_delete_proc del);

# define ccl_list_get_size(l)   ((l)->size)

# define ccl_list_get_length(l) ((l)->size)

# define ccl_list_is_empty(l) (ccl_list_get_size(l)==0)

CCL_EXTERN void
ccl_list_add(ccl_list l, void *object);

CCL_EXTERN int
ccl_list_has(ccl_list l, void *object);

CCL_EXTERN void
ccl_list_put_first(ccl_list l, void *obj);

CCL_EXTERN void *
ccl_list_take_first(ccl_list l);

CCL_EXTERN void
ccl_list_sort(ccl_list l, ccl_compare_func cmp);

CCL_EXTERN int
ccl_list_insert(ccl_list l, void *obj, ccl_compare_func cmp);

CCL_EXTERN int
ccl_list_equals(ccl_list l1, ccl_list l2);

CCL_EXTERN ccl_list
ccl_list_dup(ccl_list l);

CCL_EXTERN ccl_list
ccl_list_deep_dup(ccl_list l, ccl_duplicate_func dup);

CCL_EXTERN void *
ccl_list_get_at(ccl_list l, int index);

CCL_EXTERN void 
ccl_list_remove(ccl_list l, void *p);

CCL_EXTERN void
ccl_list_sub(ccl_list l1, ccl_list l2);

CCL_EXTERN void
ccl_list_append(ccl_list l1, ccl_list l2);

CCL_EXTERN int
ccl_list_get_index(ccl_list l, void *ptr, ccl_compare_func cmp);

CCL_EXTERN int
ccl_list_compare(ccl_list l1, ccl_list l2, ccl_compare_func cmp);

CCL_EXTERN uint32_t
ccl_list_hash(ccl_list l);

CCL_EXTERN uint32_t
ccl_list_hash_all(ccl_list l, ccl_hash_func h);

#endif /* __CCL_LIST_H__ */
