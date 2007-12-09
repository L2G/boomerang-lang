/* $Id: ccl-stack.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-stack.h"

typedef struct stack_cell_st {
  void                 *obj;
  struct stack_cell_st *next;
} stack_cell;

struct ccl_stack_st {
  stack_cell *top;
  int        size;
};

			/* --------------- */

ccl_stack
ccl_stack_create(void)
{
  ccl_stack result = ccl_new(struct ccl_stack_st);

  result->size = 0;
  result->top  = NULL;

  return result;
}

			/* --------------- */

void
ccl_stack_delete(ccl_stack s)
{
  ccl_pre( s != NULL );

  while( ! ccl_stack_is_empty(s) )
    ccl_stack_pop(s);
  ccl_delete(s);
}

			/* --------------- */

int
ccl_stack_size(ccl_stack s)
{
  ccl_pre( s != NULL );

  return s->size;
}

			/* --------------- */

void *
ccl_stack_top(ccl_stack s)
{
  ccl_pre( s != NULL ); ccl_pre( ! ccl_stack_is_empty(s) );
  
  return s->top->obj;
}

			/* --------------- */

void
ccl_stack_push(ccl_stack s, void *obj)
{
  stack_cell *c;

  ccl_pre( s != NULL );
  
  c = ccl_new(stack_cell);
  c->obj = obj;
  c->next = s->top;
  s->top = c;
  s->size++;
}

			/* --------------- */

void *
ccl_stack_pop(ccl_stack s)
{
  void  *result;
  stack_cell *c;

  ccl_pre( s != NULL ); ccl_pre( ! ccl_stack_is_empty(s) );

  c = s->top;
  result = c->obj;
  s->top = c->next;
  ccl_delete(c);
  s->size--;

  return result;
}

