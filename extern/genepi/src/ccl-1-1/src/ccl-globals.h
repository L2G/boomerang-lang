/* $Id: ccl-globals.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __CCL_GLOBALS_H__
# define __CCL_GLOBALS_H__

# include <string.h>
# include <stdlib.h>

# ifndef CCL_EXTERN
#  ifdef __cplusplus
#   define CCL_EXTERN extern "C"
#  else
#   define CCL_EXTERN extern
#  endif
# endif

# ifndef NULL
#  define NULL ((void *)0)
# endif

# define CCL_NOP() ((void)0)

#endif /* __CCL_GLOBALS_H__ */
