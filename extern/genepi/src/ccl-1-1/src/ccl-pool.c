/* $Id: ccl-pool.c,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#include "ccl-assert.h"
#include "ccl-debug.h"
#include "ccl-memory.h"
#include "ccl-pool.h"

typedef struct ccl_pool_page_st *ccl_pool_page;
typedef struct pool_object_st  *pool_object;

struct pool_object_st {
  pool_object next;
};

struct ccl_pool_page_st {
  ccl_pool_page next;
  struct pool_object_st objects[1];
};

struct ccl_pool_st {
  ccl_pool next;
  const char *name;
  int nb_pages;
  int nb_free;
  size_t object_size;
  int nb_elements;
  pool_object free_objects;
  ccl_pool_page pages;
};

			/* --------------- */

static void
s_new_page(ccl_pool p);

static void
s_delete_pools(void);

			/* --------------- */

static ccl_pool POOLS = NULL;

			/* --------------- */

void
ccl_pool_init(void)
{
  POOLS = NULL;
}

			/* --------------- */

void
ccl_pool_terminate(void)
{
  s_delete_pools();
  POOLS = NULL;
  DBLOCK(CCL_DEBUG_POOLS,ccl_pools_display_info(CCL_LOG_DEBUG));
}

			/* --------------- */

ccl_pool
ccl_pool_create(const char *poolname, size_t object_size, int nb_elements)
{
  ccl_pool result = ccl_new(struct ccl_pool_st);

  ccl_pre( object_size >= sizeof(struct pool_object_st) );

  result->next                = POOLS;
  result->name                = poolname;
  result->nb_pages            = 0;
  result->nb_free             = 0;
  result->object_size         = object_size;
  result->nb_elements         = nb_elements;
  result->free_objects        = NULL;
  result->pages               = NULL;

  POOLS = result;

  return result;
}

			/* --------------- */

void
ccl_pool_delete(ccl_pool pool)
{
  ccl_pool    *ppool;
  ccl_pool_page    p;
  ccl_pool_page next;

  for(ppool = &POOLS; *ppool; ppool = &((*ppool)->next))
    {
      if( *ppool == pool )
	break;
    }

  ccl_assert( *ppool != NULL );
  *ppool = pool->next;

  for(p = pool->pages; p; p = next)
    {
      next = p->next;
      ccl_delete(p);
    }
  ccl_delete(pool);
}

			/* --------------- */

void *
ccl_pool_alloc(ccl_pool pool)
{
  pool_object result;

  if( pool->free_objects == NULL )
    s_new_page(pool);
  
  result = pool->free_objects;
  pool->free_objects = result->next;
  pool->nb_free--;

  return (void*)result;
}

			/* --------------- */

void
ccl_pool_release(ccl_pool pool, void *ptr)
{
  pool_object o = (pool_object)ptr;

  DBLOCK(CCL_DEBUG_POOLS,
	 {
	   pool_object p;
	   
	   for(p = pool->free_objects; p; p = p->next)
	     ccl_assert( p != ptr  );
	 });

  pool->nb_free++;
  o->next = pool->free_objects;
  pool->free_objects = o;
}

			/* --------------- */

void
ccl_pool_collect(ccl_pool pool)
{
  pool_object          o;
  pool_object       next;
  pool_object        *po;
  ccl_pool_page        *p;
  ccl_pool_page next_page;
  ccl_pool_page to_remove = NULL;

  if( pool->nb_free < pool->nb_elements )
    return;

  /* first, mark all free objects */
  for(o = pool->free_objects; o; o = next)
    {
      next = o->next;
      o->next = CCL_BITPTR(pool_object,o->next);
    }

  p = &pool->pages;
  while( *p )
    {
      ccl_pool_page page = *p;
      char          *ptr = (char *)page->objects;
      int              i;
      int          count = 0;

      /* count the number of unused blocks for the page */
      for(i = 0; i < pool->nb_elements; i++)
	{
	  o = (pool_object)ptr;
	  if( CCL_PTRHASBIT(o->next) )
	    count++;
	  ptr += pool->object_size;
	}

      if( count == pool->nb_elements )
	{
	  /* This page must be removed.
	   * Unmark all blocks that must be removed 
	   */
	  ptr = (char *)page->objects;
	  for(i = 0; i < pool->nb_elements; i++)
	    {
	      o = (pool_object)ptr;
	      ccl_assert( CCL_PTRHASBIT(o->next) );
	      o->next = CCL_BITPTR2PTR(pool_object,o->next);
	      ptr += pool->object_size;
	    }

	  /* remove the page */
	  *p = page->next;
	  page->next = to_remove;
	  to_remove = page;

	  pool->nb_pages--;
	}
      else
	{
	  p = &((*p)->next);
	}
    }

  /* remove from the list of unused objects those with the ptr bit. */
  po = &pool->free_objects;
  while( *po )
    {
      o = *po;
      if( CCL_PTRHASBIT(o->next) )
	{
	  o->next = CCL_BITPTR2PTR(pool_object,o->next);
	  po = &(o->next);
	}
      else
	{
	  pool->nb_free--;
	  *po = o->next;
	}
    }
  for( ; to_remove; to_remove = next_page)
    {
      next_page = to_remove->next;
      ccl_delete(to_remove);
    }

  ccl_assert(ccl_imply(pool->nb_pages == 0,pool->nb_free == 0));
  ccl_assert(ccl_imply(pool->nb_free == 0,pool->nb_pages == 0));
  ccl_assert(ccl_imply(pool->nb_pages == 0,pool->free_objects == NULL));
  ccl_assert(ccl_imply(pool->free_objects == NULL,pool->nb_pages == 0));
}

			/* --------------- */


void
ccl_pool_display_info(ccl_log_type lt, ccl_pool pool)
{
  ccl_log(lt,
	 "POOL '%s'\n"
	 "  number of pages             : %d\n"
	 "  number of objects per pages : %d\n"
	 "  size of one object          : %d\n"
	 "  number of unused objects    : %d\n"
	 "  total allocated memory      : %z\n"
	 "\n",
	 pool->name, pool->nb_pages, pool->nb_elements,
	 pool->object_size, pool->nb_free, 
	 pool->nb_pages*pool->nb_elements*pool->object_size);
}

			/* --------------- */

void
ccl_pools_collect(void)
{
  ccl_pool p;

  for(p = POOLS; p; p = p->next)
    ccl_pool_collect(p);
}

			/* --------------- */

void
ccl_pools_display_info(ccl_log_type lt)
{
  ccl_pool p;

  
  ccl_log(lt,"-----------------------------------------------------------------"
	 "--------------\n");
  ccl_log(lt,"\t\t\t# P\t# O/P\to/O\tfree\tmem\n");
  ccl_log(lt,"-----------------------------------------------------------------"
	 "--------------\n");

  if( (p = POOLS) == NULL )
    {
       ccl_log(lt,"there is no active pool.\n");
    }
  else
    {
      for(p = POOLS; p; p = p->next)
	{
	  char *unit = "o";
	  float  sz = (float)(p->nb_pages*p->nb_elements*p->object_size);
	  
	  if( sz > 1024.0 ) { sz /= 1024.0; unit = "ko"; }
	  if( sz > 1024.0 ) { sz /= 1024.0; unit = "Mo"; }
	  if( sz > 1024.0 ) { sz /= 1024.0; unit = "Go"; }
	  
	  ccl_log(lt,"%s\t:\t%d\t%d\t%d\t%d\t%10.2f%s\n",
		 p->name, p->nb_pages, p->nb_elements,
		 p->object_size, p->nb_free,sz,unit);
	}
    }
  ccl_log(lt,"-----------------------------------------------------------------"
	 "--------------\n");
}

			/* --------------- */

static void
s_new_page(ccl_pool p)
{
  int i;
  size_t size = sizeof(struct ccl_pool_page_st) + p->object_size*p->nb_elements;
  ccl_pool_page page = (ccl_pool_page)ccl_malloc(size);
  char *ptr;
  pool_object o = NULL;

  page->next = p->pages;
  p->pages   = page;
  p->nb_free += p->nb_elements;
  p->nb_pages++;

  for(ptr = (char *)page->objects, i = 0; i < p->nb_elements; i++)
    {
      o = (pool_object)ptr;
      o->next = (pool_object)(ptr+p->object_size);
      ptr = (char *)o->next;
    }

  o->next = p->free_objects;
  p->free_objects = page->objects;

  for(i=0, o = p->free_objects; o; o = o->next) i++;

  ccl_assert( i == p->nb_elements );
}

			/* --------------- */

static void
s_delete_pools(void)
{
  ccl_pool p, next;

  for(p = POOLS; p; p = next)
    {
      next = p->next;
      ccl_pool_delete(p);
    }
}

			/* --------------- */
