/* $Id: ccl-log.h,v 1.2 2006/01/04 07:56:44 point Exp $ */
#ifndef __CCL_LOG_H__
# define __CCL_LOG_H__

# include <stdarg.h>
# include <ccl-globals.h>

typedef enum ccl_log_type_enum {
  CCL_LOG_DISPLAY = 0,
  CCL_LOG_WARNING,
  CCL_LOG_ERROR,
  CCL_LOG_DEBUG,
  CCL_LOG_PANIC,
  CCL_LOG_NB_TYPES
} ccl_log_type;

typedef void (*ccl_log_proc)(ccl_log_type type, const char  *msg, void *data);


			/* --------------- */

CCL_EXTERN void
ccl_log_init(void);

CCL_EXTERN void
ccl_log_terminate(void);

			/* --------------- */

CCL_EXTERN int
ccl_log_has_listener(void);

CCL_EXTERN void
ccl_log_add_listener(const ccl_log_proc proc, void *data);

CCL_EXTERN void
ccl_log_remove_listener(const ccl_log_proc proc);

			/* --------------- */

CCL_EXTERN void
ccl_log_redirect(ccl_log_type type, const ccl_log_proc proc, void *data);

CCL_EXTERN void
ccl_log_restore(ccl_log_type type);

			/* --------------- */

CCL_EXTERN void
ccl_log(ccl_log_type type, const char *fmt, ...);

CCL_EXTERN void
ccl_log_va(ccl_log_type type, const char *fmt, va_list pa);

			/* --------------- */

CCL_EXTERN void
ccl_display(const char *fmt, ...);

CCL_EXTERN void
ccl_warning(const char *fmt, ...);

CCL_EXTERN void
ccl_panic(const char *fmt, ...);

CCL_EXTERN void
ccl_error(const char *fmt, ...);

#endif /* __CCL_LOG_H__ */
