/* $Id: ccl-types.h,v 1.2 2005/09/27 07:31:35 point Exp $ */
#ifndef __CCL_TYPES_H__
# define __CCL_TYPES_H__

# include <ccl-globals.h>

# ifdef HAVE_STDINT
#  include <unistd.h>
# else
#  ifdef HAVE_STDINT_H
#   include <stdint.h>
#  else
typedef unsigned char uint8_t;
typedef char int8_t;

typedef unsigned short uint16_t;
typedef short int16_t;

typedef unsigned int uint32_t;
typedef int int32_t;
#   ifdef ARCH64
typedef unsigned long long uint64_t;
typedef long long int64_t;
#   endif /* ARCH64 */
#  endif /* HAVE_STDINT_H */
# endif /* HAVE_STDINT */

# ifdef ARCH32
#  define ccl_intptr uint32_t
# else
#  define ccl_intptr uint64_t
# endif

typedef void *ccl_ptr;

#define CCL_BITPTR(_type,_p)     ((_type)(((ccl_intptr)(_p))|((ccl_intptr)1)))
#define CCL_PTRHASBIT(_p)        (((ccl_intptr)(_p))&((ccl_intptr)1))
#define CCL_BITPTR2PTR(_type,_p) ((_type)(((ccl_intptr)(_p))&(~(ccl_intptr)1)))

#endif /* __CCL_TYPES_H__ */
