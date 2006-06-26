/* $Id: ccl-debug.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __CCL_DEBUG_H__
# define __CCL_DEBUG_H__

# include <ccl-globals.h>
# include <ccl-types.h>

# define CCL_DEBUG_TRACE (1<<0)
# define CCL_DEBUG_POOLS (1<<1)

CCL_EXTERN uint32_t ccl_debug_mask;

# ifdef NDEBUG
#  define DBLOCK(_mask,_block) ((void)0)
# else
CCL_EXTERN void
ccl_debug_printf(uint32_t mask, const char *fmt, ...);

#  define DBLOCK(_mask,_block) \
 do { if( ccl_debug_mask & (_mask) ) { _block; } } while(0) 

# endif /* NDEBUG */

#  define DPRINTF(args) DBLOCK(0xFFFFFFFF,ccl_debug_printf args)
#  define TRACE(args) DBLOCK(CCL_DEBUG_TRACE,ccl_debug_printf args)

#endif /* __CCL_DEBUG_H__ */
