/* $Id: ccl-bittable.c,v 1.6 2006/03/16 16:25:18 point Exp $ */
#include <string.h>
#include "ccl-types.h"
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-bittable.h"

struct ccl_bittable_st {
  int nb_bits;
  int table_size;
  int first;
  uint32_t *table;
};

			/* --------------- */

# define one ((uint32_t)1)
# define bits_to_uint32(_s_) (((_s_)&0x1F)?(((_s_)>>5)+1):((_s_)>>5))
# define index_of(_i)        ((_i)>>5)
# define offset_of(_i)       ((_i)&0x1F)

			/* --------------- */

ccl_bittable
ccl_bittable_create(int size)
{
  ccl_bittable result;

  result             = ccl_new(struct ccl_bittable_st);
  result->nb_bits    = size;
  result->table_size = bits_to_uint32(size);
  result->table      = ccl_new_array(uint32_t,result->table_size);
  result->first      = size;

  return result;
}

			/* --------------- */

void
ccl_bittable_delete(ccl_bittable bt)
{
  ccl_pre( bt != NULL );

  ccl_delete(bt->table);
  ccl_delete(bt);
}

			/* --------------- */

int
ccl_bittable_size(ccl_bittable bt)
{
  ccl_pre( bt != NULL );

  return bt->nb_bits;
}

			/* --------------- */

int
ccl_bittable_has(ccl_bittable bt, int i)
{
  int offset = offset_of(i);
  int index = index_of(i);

  if( index >= bt->table_size || i >= bt->nb_bits )
    return 0;

  return ((bt->table[index] & (one << offset)) != 0);
}

			/* --------------- */

void
ccl_bittable_set(ccl_bittable bt, int i)
{
  int offset = offset_of(i);
  int index = index_of(i);

  if( index >= bt->table_size )
    {
      uint32_t *new_table = ccl_new_array(uint32_t,index+1);
      memcpy(new_table,bt->table,sizeof(uint32_t)*bt->table_size);
      bt->table_size = index+1;
      ccl_delete(bt->table);
      bt->table = new_table;
    }

  if( i >= bt->nb_bits ) 
    {
      if( bt->first == bt->nb_bits )
	bt->first = i+1;
      bt->nb_bits = i+1;
    }

  bt->table[index] |= (one << offset);
  if( i < bt->first )
    bt->first = i;
}

			/* --------------- */

void
ccl_bittable_unset(ccl_bittable bt, int i)
{
  int offset = offset_of(i);
  int  index = index_of(i);

  if( index >= bt->table_size )
    {
      uint32_t *new_table = ccl_new_array(uint32_t,index+1);
      memcpy(new_table,bt->table,sizeof(uint32_t)*bt->table_size);
      bt->table_size = index+1;
      ccl_delete(bt->table);
      bt->table = new_table;
    }

  if( i >= bt->nb_bits )
    {
      if( bt->first == bt->nb_bits )
	bt->first = i+1;
      bt->nb_bits = i+1;
    }

  bt->table[index] &= ~(one << offset);
  if( i == bt->first )
    {
      int next = ccl_bittable_get_next(bt,i);
      if( next < 0 ) 
	bt->first = bt->nb_bits;
      else
	bt->first = next;

      ccl_assert( bt->first > i );
    }
}

			/* --------------- */

int
ccl_bittable_get_first(ccl_bittable bt)
{
  ccl_pre( bt != NULL );

  if( bt->first == bt->nb_bits )
    return -1;

  return bt->first;
}

			/* --------------- */

int
ccl_bittable_get_next(ccl_bittable bt, int prev)
{
  long w;
  int  index, offset, result = -1;

  ccl_pre( prev >= 0);
  ccl_pre( prev < bt->nb_bits );

  index  = index_of(prev);
  offset = offset_of(prev);
  w      = bt->table[index]>>offset;

  for(offset++, w >>= 1; offset < 32; offset++, w>>=1 )
    {
      if(w & 1l) 
	{
	  result = (index<<5)+offset; 
	  break;
	}
    }

  if( result == -1 )
    {
      for(index++; index < bt->table_size && (w = bt->table[index]) == 0; 
	  index++)
	/* empty */;

      if( index < bt->table_size ) 
	{

	  index <<= 5;
	  for(offset = 0; offset < 32; offset++, w >>= 1 )
	    {
	      if(w & 1l) 
		{
		  result = index+offset; 
		  break;
		}
	    }
	}
    }

  if( result >= bt->nb_bits )
    result = -1;

  return result;
}

			/* --------------- */

void
ccl_bittable_clear(ccl_bittable bt)
{
  ccl_pre( bt != NULL );
  memset(bt->table,0,sizeof(uint32_t)*bt->table_size);
  bt->first = bt->nb_bits;
}


			/* --------------- */

unsigned int
ccl_bittable_hash(const ccl_bittable bt)
{
  int          i;
  unsigned int result = bt->nb_bits;
  
  for(i = 0; i < bt->table_size; i++)
    result = (result << 3)+bt->table[i];

  return result;
}

			/* --------------- */

int
ccl_bittable_equals(const ccl_bittable bt1, const ccl_bittable bt2)
{
  int i;

  if( bt1->nb_bits != bt2->nb_bits && bt1->first != bt2->first )
    return 0;

  ccl_assert( bt1->table_size == bt2->table_size );

  for(i = 0; i < bt1->table_size; i++)
    if( bt1->table[i] != bt2->table[i] )
      return 0;

  return 1;
}

			/* --------------- */

ccl_bittable
ccl_bittable_dup(const ccl_bittable bt)
{
  ccl_bittable result;

  ccl_pre( bt != NULL );

  result             = ccl_new(struct ccl_bittable_st);
  result->nb_bits    = bt->nb_bits;
  result->table_size = bt->table_size;
  result->table      = ccl_new_array(uint32_t,result->table_size);
  result->first      = bt->first;
  memcpy(result->table,bt->table,sizeof(int)*bt->table_size);

  return result;
}


			/* --------------- */

int
ccl_bittable_get_nb_one(const ccl_bittable bt)
{
  int i;
  int r = 0;

  for(i = ccl_bittable_get_first(bt); i >= 0; i = ccl_bittable_get_next(bt,i))
    r++;

  return r;
}


			/* --------------- */

typedef struct bittable_iterator_st {
  ccl_int_iterator super;
  ccl_bittable table;
  int current;
} bittable_iterator;

			/* --------------- */

static int
s_bittable_iterator_has_more_elements(const ccl_int_iterator *i)
{
  bittable_iterator *bii = (bittable_iterator *)i;

  ccl_pre( bii != NULL );

  return bii->current >= 0;
}

			/* --------------- */

static int
s_bittable_iterator_next_element(ccl_int_iterator *i)
{
  int result;
  bittable_iterator *bii = (bittable_iterator *)i;

  ccl_pre( bii != NULL );

  result = bii->current;
  bii->current = ccl_bittable_get_next(bii->table,result);

  return result;
}

			/* --------------- */

static void
s_bittable_iterator_delete_iterator(ccl_int_iterator *i)
{
  ccl_pre( i != NULL );
  ccl_delete(i);
}

			/* --------------- */

static ccl_int_iterator BITTABLE_ITERATOR = {
  s_bittable_iterator_has_more_elements,
  s_bittable_iterator_next_element,
  s_bittable_iterator_delete_iterator
};
	
			/* --------------- */

ccl_int_iterator *
ccl_bittable_get_ones(ccl_bittable bt)
{
  bittable_iterator *result = ccl_new(bittable_iterator);

  result->super = BITTABLE_ITERATOR;
  result->table = bt;
  result->current = ccl_bittable_get_first(bt);
  
  return (ccl_int_iterator *)result;
}

			/* --------------- */

ccl_bittable 
ccl_bittable_resize(ccl_bittable bt, int newsize)
{
  ccl_bittable result = ccl_bittable_create(newsize);
  int minsize = bt->table_size;

  if( result->table_size < minsize )
    minsize = result->table_size;
  ccl_memcpy(result->table,bt->table,minsize*sizeof(uint32_t));

  if( bt->first == bt->nb_bits || bt->first >= result->nb_bits )
    result->first = result->nb_bits;
  else 
    result->first = bt->first;

  return result;
}

			/* --------------- */

static int
s_get_first(ccl_bittable bt)
{
  int i;

  for(i = 0; i < bt->table_size; i++)
    {
      if( bt->table[i] != 0 )
	{
	  int result = 32*i;
	  uint32_t w = bt->table[i];	  
	  
	  while( (w & 0x1) == 0 )
	    {
	      w >>= 1;
	      result++;
	    }

	  ccl_assert( ccl_bittable_has(bt,result) );

	  return result;
	}
    }

  return bt->nb_bits;
}

			/* --------------- */

ccl_bittable
ccl_bittable_union(ccl_bittable bt1, ccl_bittable bt2)
{
  int i;
  ccl_bittable bt;
  
  ccl_pre( bt1 != NULL );  ccl_pre( bt2 != NULL );
  ccl_pre( bt1->nb_bits == bt2->nb_bits );

  bt = ccl_bittable_create(bt2->nb_bits);
  for(i = 0; i < bt->table_size; i++)
    bt->table[i] = (bt1->table[i]|bt2->table[i]);
  bt->first = s_get_first(bt);

  return bt;
}

			/* --------------- */

ccl_bittable
ccl_bittable_intersection(ccl_bittable bt1, ccl_bittable bt2)
{
  int i;
  ccl_bittable bt;
  
  ccl_pre( bt1 != NULL );  ccl_pre( bt2 != NULL );
  ccl_pre( bt1->nb_bits == bt2->nb_bits );

  bt = ccl_bittable_create(bt2->nb_bits);
  for(i = 0; i < bt->table_size; i++)
    bt->table[i] = (bt1->table[i]&bt2->table[i]);
  bt->first = s_get_first(bt);

  return bt;
}

			/* --------------- */

ccl_bittable
ccl_bittable_complement(ccl_bittable bt)
{
  int i;
  ccl_bittable rbt;
  
  ccl_pre( bt != NULL );  

  rbt = ccl_bittable_create(bt->nb_bits);
  for(i = 0; i < bt->table_size-1; i++)
    rbt->table[i] = ~bt->table[i];
  rbt->table[i] = ~(bt->table[i]| (0xFFFFFFFF << (bt->nb_bits&0x1F)));
  rbt->first = s_get_first(rbt);

  return rbt;
}
