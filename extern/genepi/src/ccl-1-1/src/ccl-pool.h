/* $Id: ccl-pool.h,v 1.2 2006/01/04 08:00:23 point Exp $ */
#ifndef __CCL_POOL_H__
# define __CCL_POOL_H__

# include <ccl-globals.h>
# include <ccl-log.h>

typedef struct ccl_pool_st *ccl_pool;

			/* --------------- */

CCL_EXTERN void
ccl_pool_init(void);

CCL_EXTERN void
ccl_pool_terminate(void);

			/* --------------- */

CCL_EXTERN ccl_pool
ccl_pool_create(const char *poolname, size_t object_size, int nb_elements);

CCL_EXTERN void
ccl_pool_delete(ccl_pool pool);

CCL_EXTERN void *
ccl_pool_alloc(ccl_pool pool);

CCL_EXTERN void
ccl_pool_release(ccl_pool pool, void *ptr);

CCL_EXTERN void
ccl_pool_collect(ccl_pool pool);

CCL_EXTERN void
ccl_pool_display_info(ccl_log_type lt, ccl_pool pool);

			/* --------------- */

CCL_EXTERN void
ccl_pools_collect(void);

CCL_EXTERN void
ccl_pools_display_info(ccl_log_type lt);

#endif /* __CCL_POOL_H__ */
