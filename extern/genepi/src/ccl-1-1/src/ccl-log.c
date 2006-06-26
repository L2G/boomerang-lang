/* $Id: ccl-log.c,v 1.2 2006/01/04 07:56:44 point Exp $ */
#include <stdio.h>
#include <stdarg.h>
#include "ccl-memory.h"
#include "ccl-assert.h"
#include "ccl-string.h"
#include "ccl-log.h"

# define LOG_BUFFER_INIT_SIZE 1000

typedef struct log_listener_st log_listener;
struct log_listener_st {
  ccl_log_proc   proc;
  void         *data;
  log_listener *next;
};


			/* --------------- */

static char         *log_buffer = NULL;
static size_t   log_buffer_size = 0;
static log_listener  *listeners = NULL;
static log_listener redirection[CCL_LOG_NB_TYPES];

			/* --------------- */

void
ccl_log_init(void)
{
  int i;

  listeners = NULL;
  for(i = 0; i < CCL_LOG_NB_TYPES; i++)
    redirection[i].proc = NULL;
}

			/* --------------- */

void
ccl_log_terminate(void)
{
  while( listeners != NULL )
    {
      log_listener *next = listeners->next;
      ccl_delete(listeners);
      listeners = next;
    }
  if( log_buffer != NULL )
    {
      ccl_delete(log_buffer);
      log_buffer = NULL;
    }
}

			/* --------------- */

int
ccl_log_has_listener(void)
{
  return listeners != NULL;
}

			/* --------------- */

void
ccl_log_add_listener(const ccl_log_proc proc, void *data)
{
  log_listener *l;

  l = ccl_new(log_listener);
  l->proc = proc;
  l->data = data;
  l->next = listeners;
  listeners = l;
}

			/* --------------- */

void
ccl_log_remove_listener(const ccl_log_proc proc)
{
  log_listener     **plist;
  log_listener  *to_remove;

  for(plist = &listeners; *plist; plist = &((*plist)->next))
    {
      if( (*plist)->proc == proc )
	break;
    }

  ccl_pre( (*plist != NULL) && "unknown listener removal." );

  to_remove =  *plist;
  *plist = to_remove->next;
  ccl_delete(to_remove);
}

			/* --------------- */

void
ccl_log_redirect(ccl_log_type type, const ccl_log_proc proc, void *data)
{
  ccl_pre( (redirection[type].proc == NULL) &&
    "log listener redirection occuring twice.");

  redirection[type].proc = proc;
  redirection[type].data = data;
}

			/* --------------- */

void
ccl_log_restore(ccl_log_type type)
{
  redirection[type].proc = NULL;
  redirection[type].data = NULL;
}

			/* --------------- */

void
ccl_log(ccl_log_type type, const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_log_va(type,fmt,pa);
  va_end(pa);
}

			/* --------------- */

void
ccl_log_va(ccl_log_type type, const char *fmt, va_list pa)
{
  ccl_string_format_va(&log_buffer,&log_buffer_size,fmt,pa);
  
  if( redirection[type].proc != NULL )
    redirection[type].proc(type,log_buffer,redirection[type].data);
  else
    {
      log_listener *l;
      
      for(l = listeners; l; l = l->next)
	l->proc(type,log_buffer,l->data);
    }
}


			/* --------------- */

void
ccl_display(const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_log_va(CCL_LOG_DISPLAY,fmt,pa);
  va_end(pa);
}

			/* --------------- */

void
ccl_warning(const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_log_va(CCL_LOG_WARNING,fmt,pa);
  va_end(pa);
}

			/* --------------- */

void
ccl_panic(const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_log_va(CCL_LOG_PANIC,fmt,pa);
  va_end(pa);
}

			/* --------------- */

void
ccl_error(const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_log_va(CCL_LOG_ERROR,fmt,pa);
  va_end(pa);
}


			/* --------------- */


