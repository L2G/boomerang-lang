/* $Id: ccl-list.c,v 1.4 2006/01/04 07:58:36 point Exp $ */
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-list.h"

# define s_alloc_list()  ccl_new(struct ccl_list_st)
# define s_free_list(_l) ccl_delete((_l))
# define s_alloc_pair()  ccl_new(struct ccl_pair_st)
# define s_free_pair(_p) ccl_delete((_p))

			/* --------------- */

ccl_list
ccl_list_create(void)
{
  ccl_list result = s_alloc_list();

  result->size  = 0;
  result->first = NULL;
  result->last = &result->first;

  return result;
}

			/* --------------- */

void
ccl_list_delete(ccl_list l)
{
  ccl_list_clear_and_delete(l,NULL);
}

			/* --------------- */

void
ccl_list_clear(ccl_list l, ccl_delete_proc del)
{
  ccl_pair p, next;

  if( del != NULL )
    {
      for(p = FIRST(l); p; p = next)
	{
	  next = CDR(p);
	  if( del != NULL )
	    del(CAR(p));
	  s_free_pair(p);
	}
    }
  else
    {
      for(p = FIRST(l); p; p = next)
	{
	  next = CDR(p);
	  s_free_pair(p);
	}
    }
  l->first = NULL;
  l->size  = 0;
  l->last= &l->first;
}

			/* --------------- */

void
ccl_list_clear_and_delete(ccl_list l, ccl_delete_proc del)
{
  ccl_list_clear(l,del);
  s_free_list(l);
}

			/* --------------- */

void
ccl_list_add(ccl_list l, void *object)
{
  ccl_pair p = s_alloc_pair();

  CAR(p) = object;
  CDR(p) = NULL;
  *(l->last) = p;
  l->last = &CDR(p);
  l->size++;
}

			/* --------------- */

int
ccl_list_has(ccl_list l, void *object)
{
  ccl_pair p;

  for(p = FIRST(l); p; p = CDR(p))
    {
      if( CAR(p) == object )
	return 1;
    }

  return 0;
}

			/* --------------- */

void
ccl_list_put_first(ccl_list l, void *obj)
{
  if( l->size == 0 )
    ccl_list_add(l,obj);
  else
    {
      ccl_pair first = s_alloc_pair();

      first->car = obj;
      first->cdr = l->first;
      l->first   = first;
      l->size++;
    }
}

			/* --------------- */

void *
ccl_list_take_first(ccl_list l)
{
  ccl_pair first;
  void *result;

  ccl_pre( ! ccl_list_is_empty(l) );

  first = FIRST(l);
  result = CAR(first);
  
  if( l->last == &CDR(first) )
    l->last = &FIRST(l);
  FIRST(l) = CDR(FIRST(l));
  l->size--;
  s_free_pair(first);

  return result;
}

			/* --------------- */

static int
s_default_cmp(const void *p1, const void *p2)
{
  if( p1 < p2 ) return -1;
  if( p1 == p2 ) return 0;
  return 1;
}

			/* --------------- */

static ccl_pair 
s_sort_rec(ccl_pair p, ccl_compare_func cmp, ccl_pair queue)
{
  int          c;
  ccl_pair   cdr;
  ccl_pair pivot = p;
  ccl_pair   inf = NULL;
  ccl_pair   sup = NULL;

  if( p == NULL )
    return queue;

  for(p = CDR(p); p; p = cdr)
    {
      cdr = CDR(p);

      c = cmp(CAR(p),CAR(pivot));
      if( c < 0 ) { CDR(p) = inf; inf = p; }
      else        { CDR(p) = sup; sup = p; }
    }
  
  CDR(pivot) = s_sort_rec(sup,cmp,queue);

  return s_sort_rec(inf,cmp,pivot);
}

			/* --------------- */

static ccl_pair *
s_get_last(ccl_list l)
{
  ccl_pair *last;

  for(last = &l->first; *last; last = &(CDR(*last)))
    CCL_NOP();

  return last;
}

			/* --------------- */

void
ccl_list_sort(ccl_list l, ccl_compare_func cmp)
{
  if( cmp == NULL ) 
    cmp = s_default_cmp;
  l->first = s_sort_rec(l->first,cmp,NULL);
  l->last = s_get_last(l);
}

			/* --------------- */

int
ccl_list_insert(ccl_list l, void *obj, ccl_compare_func cmp)
{
  int index;
  ccl_pair p;
  ccl_pair *pp;

  if( l->size == 0 ) 
    {
      ccl_list_add(l,obj);
      return 0;
    }

  l->size++;
  if( cmp == NULL )
    cmp = s_default_cmp;
  for(index = 0, pp = &(l->first); *pp; pp = &((*pp)->cdr), index++)
    {
      if( cmp(obj,(*pp)->car) < 0 )
	break;
    }
  
  p = s_alloc_pair(); 
  p->car = obj;
  p->cdr = *pp;
  *pp = p;

  for(pp = &(p->cdr); *pp; pp = &((*pp)->cdr))
    /* do nothing */;
  l->last = pp;

  return index;
}

			/* --------------- */

int
ccl_list_equals(ccl_list l1, ccl_list l2)
{
  ccl_pair p1;
  ccl_pair p2;
  if( ccl_list_get_size(l1) != ccl_list_get_size(l2) )
    return 0;

  p1 = FIRST(l1); p2 = FIRST(l2);
  while( p1 != NULL && p2 != NULL )
    {
      if( CAR(p1) != CAR(p2) )
	return 0;

      p1 = CDR(p1);
      p2 = CDR(p2);
    }

  return (p1 == NULL && p2 == NULL);
}

			/* --------------- */

ccl_list
ccl_list_dup(ccl_list l)
{
  ccl_pair p;
  ccl_list result = ccl_list_create();

  ccl_pre( l != NULL );

  for(p = FIRST(l); p; p = CDR(p))
    ccl_list_add(result,CAR(p));

  return result;
}

			/* --------------- */

ccl_list
ccl_list_deep_dup(ccl_list l, ccl_duplicate_func dup)
{
  ccl_pair p;
  ccl_list result;

  ccl_pre( l != NULL );

  if( dup == NULL ) result = ccl_list_dup(l);
  else
    {
      result = ccl_list_create();
      for(p = FIRST(l); p; p = CDR(p))
	ccl_list_add(result,dup(CAR(p)));
    }

  return result;
}

			/* --------------- */

void *
ccl_list_get_at(ccl_list l, int index)
{
  ccl_pair p;

  ccl_pre( l != NULL ); ccl_pre( 0 <= index && index < l->size );

  p = FIRST(l);
  while( index-- )
    p = CDR(p);
  ccl_post( p != NULL );

  return CAR(p);
}

			/* --------------- */

void 
ccl_list_remove(ccl_list l, void *ptr)
{
  ccl_pair *pp;

  for(pp = &l->first; *pp; pp = &((*pp)->cdr))
    {
      if( (*pp)->car == ptr )
	break;
    }
  if( *pp != NULL )
    {
      ccl_pair p = *pp;

      if( l->last == &(p->cdr) )
	l->last = pp;
      *pp = p->cdr;
      s_free_pair(p);
      l->size--;
    }
}

			/* --------------- */

void
ccl_list_sub(ccl_list l1, ccl_list l2)
{
  ccl_pair p;

  for(p = FIRST(l2); p != NULL; p = CDR(p))
    ccl_list_remove(l1,CAR(p));
}

			/* --------------- */

void
ccl_list_append(ccl_list l1, ccl_list l2)
{
  ccl_pair p;

  for(p = FIRST(l2); p != NULL; p = CDR(p))
    ccl_list_add(l1,CAR(p));

}

			/* --------------- */

int
ccl_list_get_index(ccl_list l, void *ptr, ccl_compare_func cmp)
{
  int result = 0;
  ccl_pair p = FIRST(l);

  if( cmp == NULL )
    {
      for(; p != NULL; p = CDR(p), result++)
	{
	  if( CAR(p) == ptr )
	    return result;
	}
    }
  else
    {
      for(; p != NULL; p = CDR(p), result++)
	{
	  if( cmp(CAR(p),ptr) == 0 )
	    return result;
	}
    }

  return -1;
}

			/* --------------- */

int
ccl_list_compare(ccl_list l1, ccl_list l2, ccl_compare_func cmp)
{
  int result = 0;
  ccl_pair p1 = FIRST(l1);
  ccl_pair p2 = FIRST(l2);

  while( p1 != NULL && p2 != NULL )
    {
      if( (result = cmp(CAR(p1),CAR(p2))) != 0 )
	break;

      p1 = CDR(p1); 
      p2 = CDR(p2);
    }

  if( p1 == NULL && p2 == NULL ) result = 0;
  else if( p1 == NULL )          result = -1;
  else if( p2 == NULL )          result = 1;

  return result;
}

			/* --------------- */

uint32_t
ccl_list_hash(ccl_list l)
{
  return ccl_list_hash_all(l,NULL);
}

			/* --------------- */

uint32_t
ccl_list_hash_all(ccl_list l, ccl_hash_func h)
{
  ccl_pair p;
  uint32_t result = 0;

  ccl_pre( l != NULL );

  p = FIRST(l);

  if( h == NULL )
    {
      for(; p; p = CDR(p))
	{
	  if( CAR(p) != NULL )
	    result = 9*result+(uint32_t)(CAR(p));
	}
    }
  else
    {
      for(; p; p = CDR(p))
	result = 9*result+h(CAR(p));
    }

  return result;
}
