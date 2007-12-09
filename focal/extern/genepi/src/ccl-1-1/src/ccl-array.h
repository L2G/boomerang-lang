/* $Id: ccl-array.h,v 1.2 2006/03/14 12:00:12 point Exp $ */
#ifndef __CCL_ARRAY_H__
# define __CCL_ARRAY_H__

# include <ccl-globals.h>

# define CCL_ARRAY(_t_) \
  struct {		\
    int size;		\
    _t_ *data; \
  }

# define ccl_array_init(_a_)				\
  do {							\
    (_a_).size = 0;					\
    (_a_).data = ccl_calloc(sizeof((_a_).data[0]),1);	\
  } while(0)

# define ccl_array_init_with_size(_a_,_sz_)		   \
  do {							   \
    (_a_).size = (_sz_);				   \
    (_a_).data = ccl_calloc(sizeof((_a_).data[0]),(_sz_)); \
  } while(0)

# define ccl_array_add(_a_,_el_)			\
  do {							\
    ccl_array_ensure_size(_a_,(_a_).size+1);		\
    (_a_).data[(_a_).size-1] = _el_;			\
  } while(0)

# define ccl_array_delete(_a_) \
  do { ccl_free((_a_).data); } while(0)
# define ccl_array_ensure_size(_a_,_sz_) \
  ccl_array_resize(&(_a_).size,sizeof((_a_).data[0]),(void **)&((_a_).data),\
		   _sz_,0)
# define ccl_array_ensure_size_plus_one(_a_) \
  ccl_array_ensure_size(_a_,(_a_).size+1)

# define ccl_array_trim(_a_,_sz_) \
  ccl_array_resize(&(_a_).size,sizeof((_a_).data[0]),(void **)&((_a_).data),\
		   _sz_,1)

CCL_EXTERN void
ccl_array_resize(int *poldsz, int elsz, void **data, int newsz, int trim);

#endif /* __CCL_ARRAY_H__ */
