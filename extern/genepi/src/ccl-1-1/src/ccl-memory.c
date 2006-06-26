/* $Id: ccl-memory.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include "ccl-memory.h"

CCL_DEFINE_EXCEPTION(memory_exhausted_exception,runtime_exception);

			/* --------------- */

void *
ccl_malloc(size_t size)
{
  void *result = malloc(size);

  if( result == NULL )
    ccl_throw_anon(memory_exhausted_exception);

  return result;
}

			/* --------------- */

void
ccl_free(void *ptr)
{
  free(ptr);
}

			/* --------------- */

void *
ccl_realloc(void *ptr, size_t size)
{
  void *result = realloc(ptr,size);

  if( result == NULL )
    ccl_throw_anon(memory_exhausted_exception);

  return result;
}

			/* --------------- */

void *
ccl_calloc(size_t nb_el, size_t el_size)
{
  void *result = calloc(nb_el,el_size);

  if( result == NULL )
    ccl_throw_anon(memory_exhausted_exception);

  return result;
}
