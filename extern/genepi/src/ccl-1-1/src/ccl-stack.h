/* $Id: ccl-stack.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __CCL_STACK_H__
# define __CCL_STACK_H__

# include <ccl-globals.h>

typedef struct ccl_stack_st *ccl_stack;

CCL_EXTERN ccl_stack
ccl_stack_create(void);

CCL_EXTERN void
ccl_stack_delete(ccl_stack s);

CCL_EXTERN int
ccl_stack_size(ccl_stack s);

# define ccl_stack_is_empty(_s) (ccl_stack_size(_s) == 0)

CCL_EXTERN void *
ccl_stack_top(ccl_stack s);

CCL_EXTERN void
ccl_stack_push(ccl_stack s, void *obj);

CCL_EXTERN void *
ccl_stack_pop(ccl_stack s);

#endif /* __CCL_STACK_H__ */
