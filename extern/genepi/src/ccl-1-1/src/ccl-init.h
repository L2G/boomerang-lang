/* $Id: ccl-init.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __CCL_INIT_H__
# define __CCL_INIT_H__

# include <ccl-globals.h>
# include <ccl-types.h>
# include <ccl-debug.h>

CCL_EXTERN void
ccl_init(uint32_t debug_level);

CCL_EXTERN void
ccl_terminate(void);

#endif /* __CCL_INIT_H__ */
