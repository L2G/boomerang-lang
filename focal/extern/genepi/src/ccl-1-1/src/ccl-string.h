/* $Id: ccl-string.h,v 1.2 2005/11/24 10:47:44 point Exp $ */
/**
 * @file
 */
#ifndef __CCL_STRING_H__
# define __CCL_STRING_H__

# include <stdarg.h>
# include <ccl-globals.h>
# include <ccl-types.h>
# include <ccl-memory.h>


typedef char *ccl_ustring;

CCL_EXTERN void
ccl_string_init(void);

CCL_EXTERN void
ccl_string_terminate(void);

			/* --------------- */

CCL_EXTERN uint32_t 
ccl_string_hash(const void *ptr);

# define ccl_string_delete ccl_free

CCL_EXTERN int
ccl_string_equals(const void *str1, const void *str2);

# define ccl_string_compare ((ccl_compare_func)strcmp)

# define ccl_string_cmp strcmp

CCL_EXTERN char *
ccl_string_dup(const char *s);

CCL_EXTERN char *
ccl_string_format_new(const char *fmt, ...);

CCL_EXTERN char *
ccl_string_format_new_va(const char *fmt, va_list pa);

CCL_EXTERN void
ccl_string_format(char **dst, size_t *size, const char *fmt, ...);

CCL_EXTERN void
ccl_string_format_va(char **dst, size_t *size, const char *fmt, va_list pa);

CCL_EXTERN void
ccl_string_format_append(char **dst, const char *fmt, ...);

CCL_EXTERN void
ccl_string_format_append_va(char **dst, const char *fmt, va_list pa);

CCL_EXTERN int
ccl_string_parse_int(const char *s);

CCL_EXTERN int
ccl_string_parse_boolean(const char *s);

CCL_EXTERN ccl_ustring
ccl_string_make_unique(const char *s);

CCL_EXTERN ccl_ustring
ccl_string_make_unique_from_int(int i);

#endif /* __CCL_STRING_H__ */
