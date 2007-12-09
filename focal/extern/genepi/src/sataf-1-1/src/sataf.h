/* $Id: sataf.h,v 1.2 2006/01/04 08:06:42 point Exp $ */
#ifndef __SATAF_H__
# define __SATAF_H__

# include <sataf-automaton.h>
# include <sataf-msa.h>

			/* --------------- */

CCL_EXTERN sataf_automaton SATAF_ONE;

CCL_EXTERN sataf_automaton SATAF_ZERO;

			/* --------------- */

CCL_EXTERN void
sataf_init(void);

CCL_EXTERN void
sataf_terminate(void);

CCL_EXTERN void
sataf_log_statistics(ccl_log_type log);

			/* --------------- */

#endif /* __SATAF_H__ */
