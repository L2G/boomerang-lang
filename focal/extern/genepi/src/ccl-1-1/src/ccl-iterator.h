/* $Id: ccl-iterator.h,v 1.3 2005/11/30 14:59:19 point Exp $ */
#ifndef __CCL_ITERATOR_H__
# define __CCL_ITERATOR_H__

# include <ccl-types.h>

# define CCL_DECLARE_ITERATOR(_typename_,_eltype_) \
typedef struct ccl_iterator_ ## _typename_ ## _st _typename_; \
struct ccl_iterator_ ## _typename_ ## _st { \
  int (*has_more_elements)(const _typename_ *i); \
  _eltype_ (*next_element)(_typename_ *i); \
  void  (*delete_iterator)(_typename_ *i); \
}

# define ccl_iterator_has_more_elements(i) ((i)->has_more_elements(i))
# define ccl_iterator_next_element(i) ((i)->next_element(i))
# define ccl_iterator_delete(i) ((i)->delete_iterator(i))

CCL_DECLARE_ITERATOR(ccl_pointer_iterator, void *);
CCL_DECLARE_ITERATOR(ccl_int_iterator, int);

#endif /* __CCL_ITERATOR_H__ */
