/* $Id: ccl-assert.h,v 1.3 2006/01/23 14:58:33 point Exp $ */
#ifndef __CCL_ASSERT_H__
# define __CCL_ASSERT_H__

# include <ccl-exception.h>

# undef CCL_ENABLE_ASSERTION

CCL_DECLARE_EXCEPTION(assertion,internal_error);

# define ccl_assert(_cond_) \
CCL_CHECK_CONDITION("assertion",_cond_,__FILE__,__LINE__)

# define ccl_pre(_cond_) \
CCL_CHECK_CONDITION("pre-condition",_cond_,__FILE__,__LINE__)

# define ccl_post(_cond_) \
CCL_CHECK_CONDITION("post-condition",_cond_,__FILE__,__LINE__)

# ifdef CCL_ENABLE_ASSERTION
#  define CCL_CHECK_CONDITION(_t,_c,_f,_l) \
ccl_check_condition(_c,_f,_l,"failed " _t " " #_c)
#else
#  define CCL_CHECK_CONDITION(_t,_c,_f,_l) ((void)0)
#endif /* CCL_ENABLE_ASSERTION */

# define ccl_imply(_a,_b)   ((!(_a))||(_b))

CCL_EXTERN void
ccl_check_condition(int cond, const char *file, int line, const char *msg);

#endif /* __CCL_ASSERT_H__ */
