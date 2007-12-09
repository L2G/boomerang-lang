/* $Id: ccl-protos.h,v 1.2 2005/10/25 06:30:17 point Exp $ */
#ifndef __CCL_PROTOS_H__
# define __CCL_PROTOS_H__

# include <ccl-types.h>

typedef uint32_t (*ccl_hash_func)(const void *ptr);

# define CCL_DEFAULT_HASH_FUNC ((ccl_hash_func)NULL)

			/* --------------- */

typedef void (*ccl_delete_proc)(void *ptr);

# define CCL_NO_DELETE_PROC ((ccl_delete_proc)NULL)

			/* --------------- */

typedef int (*ccl_equals_func)(const void *ptr1, const void *ptr2);

# define CCL_DEFAULT_EQUALS_FUNC ((ccl_equals_func)NULL)

			/* --------------- */

typedef int (*ccl_compare_func)(const void *ptr1, const void *ptr2);

# define CCL_DEFAULT_COMPARE_FUNC ((ccl_compare_func)NULL)

			/* --------------- */

typedef void *(*ccl_duplicate_func)(void *ptr);

# define CCL_DEFAULT_DUPLICATE_FUNC ((ccl_duplicate_func)NULL)

#endif /* __CCL_PROTOS_H__ */
