/* $Id: ccl-memory.h,v 1.2 2006/03/15 16:02:39 point Exp $ */
#ifndef __CCL_MEMORY_H__
# define __CCL_MEMORY_H__

# include <ccl-exception.h>

CCL_DECLARE_EXCEPTION(memory_exhausted_exception,runtime_exception);

			/* --------------- */

CCL_EXTERN void *
ccl_malloc(size_t size);

CCL_EXTERN void
ccl_free(void *ptr);

CCL_EXTERN void *
ccl_realloc(void *ptr, size_t size);

CCL_EXTERN void *
ccl_calloc(size_t nb_el, size_t el_size);


			/* --------------- */

# define ccl_new_array(_type_,_sz_) ((_type_ *)ccl_calloc(sizeof(_type_),_sz_))
# define ccl_new(_type_) ccl_new_array(_type_,1)
# define ccl_delete(_ptr_) ccl_free(_ptr_)
# define ccl_zdelete(_del,_ptr_) \
  do { if( (_ptr_) != NULL ) _del(_ptr_); } while(0)

# define ccl_memzero(_p,_size) memset(_p,0,_size)
# define ccl_memcpy(_dst,_src,_size) memcpy(_dst,_src,_size)
# define ccl_memcmp(_p1,_p2,_size) memcmp(_p1,_p2,_size)

#endif /* __CCL_MEMORY_H__ */
