/* $Id: ccl-exception.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __CCL_EXCEPTION_H__
# define __CCL_EXCEPTION_H__

# include <setjmp.h>
# include <ccl-globals.h>

typedef struct ccl_exception_st       ccl_exception;
typedef struct ccl_exception_type_st *ccl_exception_type;

struct ccl_exception_st {
  ccl_exception_type  type;
  jmp_buf          context;
  ccl_exception      *next;
};

struct ccl_exception_type_st {
  const char              *name;
  const ccl_exception_type super;
};


# define CCL_THROW(_args_) 

# define CCL_DECLARE_EXCEPTION(exc,super) \
CCL_EXTERN struct ccl_exception_type_st CCL_EXCEPTION_NAME(exc)

# define CCL_DEFINE_EXCEPTION(exc,super) \
struct ccl_exception_type_st CCL_EXCEPTION_NAME(exc) = { \
  # exc, &CCL_EXCEPTION_NAME(super) \
}

# define CCL_EXCEPTION_NAME(exc) ccl_exception_ ## exc

CCL_DECLARE_EXCEPTION(___toplevel_throwable,NULL);
CCL_DECLARE_EXCEPTION(error,___toplevel_throwable);
CCL_DECLARE_EXCEPTION(exception,___toplevel_throwable);
CCL_DECLARE_EXCEPTION(internal_error,error);
CCL_DECLARE_EXCEPTION(runtime_exception,exception);
CCL_DECLARE_EXCEPTION(division_by_zero,exception);

CCL_EXTERN const char *ccl_exception_current_message;

# define ccl_try(_extype_) \
do { \
  ccl_exception here; \
  ccl_exception_push(&here,&CCL_EXCEPTION_NAME(_extype_)); \
  if( setjmp(here.context) == 0 ) { 

# define ccl_trycatch(_extype_,_stmt_) \
ccl_try(_extype_) _stmt_  ccl_catch ccl_end_try

# define ccl_catch \
    ccl_exception_pop(); \
  } else { \
    ccl_exception_pop();

# define ccl_end_try \
  } \
} while(0)

# define ccl_no_catch \
  } \
  ccl_exception_pop(); \
} while(0)

# define ccl_throw_anon(_extype) ccl_throw(_extype,# _extype)

# define ccl_throw(_extype,_msg) ccl_throw__(&CCL_EXCEPTION_NAME(_extype), \
_msg,__FILE__,__LINE__)

# define ccl_rethrow() ccl_rethrow__(__FILE__,__LINE__)

CCL_EXTERN void
ccl_throw__(const ccl_exception_type ex, const char *msg,
	    const char *file, int line);

CCL_EXTERN void
ccl_rethrow__(const char *file, int line);

CCL_EXTERN void
ccl_exception_push(ccl_exception *e, const ccl_exception_type type);

CCL_EXTERN void
ccl_exception_pop(void);

# define ccl_exception_is_raised(_e_) \
ccl_exception_is_raised__(&CCL_EXCEPTION_NAME(_e_))

CCL_EXTERN int
ccl_exception_is_raised__(const ccl_exception_type type);

CCL_EXTERN void
ccl_exception_print(void);

#endif /* __CCL_EXCEPTION_H__ */
