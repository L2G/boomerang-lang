/* $Id: ccl-hash.c,v 1.2 2005/11/15 15:04:50 point Exp $ */
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-hash.h"

#ifndef CCL_HASHTABLE_INIT_SIZE_INDEX
# define CCL_HASHTABLE_INIT_SIZE_INDEX 12
#endif /* CCL_HASHTABLE_INIT_SIZE_INDEX */

#ifndef CCL_HASHTABLE_FILL_DEGREE
# define CCL_HASHTABLE_FILL_DEGREE 5
#endif /* CCL_HASHTABLE_FILL_DEGREE */

			/* --------------- */

typedef struct htable_entry_st htable_entry;
struct htable_entry_st {
  void         *key;
  void         *object;  
  htable_entry *next;
  uint32_t      hvalue;
};

# define s_alloc_entry()    ccl_new(htable_entry)
# define s_delete_entry(_e) ccl_delete((_e))

			/* --------------- */


typedef enum { KEYS, OBJECTS, PAIRS } iterator_mode;
typedef struct hash_iterator_st {
  ccl_hash_entry_iterator iterator;  
  iterator_mode mode;
  htable_entry *current;
  ccl_hash      table;
  int           index;
} hash_iterator;

			/* --------------- */

struct ccl_hash_st {
  int nb_entries;
  int htable_size_index;
  int htable_size;
  htable_entry **htable;

  htable_entry **cursor;
  uint32_t current_hvalue;
  void *current_key;

  ccl_hash_func    key_hash;
  ccl_compare_func key_compare;
  ccl_delete_proc  key_delete;
  ccl_delete_proc  object_delete;
};

			/* --------------- */

#define hash(ht,k) \
 ((ht)->key_hash==NULL \
  ? (uint32_t)(k) \
  : (ht)->key_hash(k))

#define cmp(ht,k1,k2) \
 ((ht)->key_compare==NULL \
  ? ((int32_t)(k1))-((int32_t)(k2)) \
  : (ht)->key_compare(k1,k2))

			/* --------------- */

#define delkey(ht,k) \
 do { if( (ht)->key_delete != NULL ) (ht)->key_delete(k); } while(0)

#define delobject(ht,o) \
 do { if( (ht)->object_delete != NULL ) (ht)->object_delete(o); } while(0)


			/* --------------- */

static int SIZE_FOR_TABLE[] = {
  1, 3, 7, 17, 37, 79, 163, 331, 673, 1361, 2729, 5471, 10949, 21911, 43853,
  87719, 175447, 350899, 701819, 1403641, 2807303, 5614657, 11229331
};

static const int SIZE_FOR_TABLE_SIZE = 
sizeof(SIZE_FOR_TABLE)/sizeof(SIZE_FOR_TABLE[0]);

			/* --------------- */

static void
s_possibly_increase_table_size(ccl_hash ht);

static void
s_possibly_decrease_table_size(ccl_hash ht);

static void
s_resize_table(ccl_hash ht, size_t newsize);

static int 
s_iterator_has_more_elements(const ccl_hash_entry_iterator *i);

static ccl_hash_entry
s_hash_entry_iterator_next_element(ccl_hash_entry_iterator *i);

static ccl_ptr
s_ptr_iterator_next_element(ccl_pointer_iterator *i);

static void
s_iterator_delete_iterator(ccl_hash_entry_iterator *i);

static hash_iterator *
s_new_iterator(ccl_hash ht, iterator_mode mode);


			/* --------------- */

ccl_hash
ccl_hash_create(ccl_hash_func    key_hash,
	       ccl_compare_func key_compare,
	       ccl_delete_proc  key_delete,
	       ccl_delete_proc  object_delete)
{
  ccl_hash result = NULL;

  result = ccl_new(struct ccl_hash_st);
  result->nb_entries = 0;
  result->htable_size_index = CCL_HASHTABLE_INIT_SIZE_INDEX;
  result->htable_size = SIZE_FOR_TABLE[CCL_HASHTABLE_INIT_SIZE_INDEX];
  result->htable = ccl_new_array(htable_entry *,result->htable_size);
  result->cursor = NULL;
  result->current_hvalue = 0;
  result->current_key = NULL;
  result->key_hash = key_hash;
  result->key_compare = key_compare;
  result->key_delete = key_delete;
  result->object_delete = object_delete;

  return result;
}

			/* --------------- */

void
ccl_hash_delete(ccl_hash ht)
{
  int i;

  ccl_pre( ht != NULL );
  
  for(i = 0; i < ht->htable_size; i++)
    {
      htable_entry *e, *next;
      for(e = ht->htable[i]; e != NULL; e = next)
	{
	  next = e->next;
	  delkey(ht,e->key);
	  delobject(ht,e->object);
	  s_delete_entry(e);
	}
    }
  ccl_delete(ht->htable);
  ccl_delete(ht);
}

			/* --------------- */

int
ccl_hash_find(ccl_hash ht, void *key)
{
  int index;
  htable_entry **c;

  ccl_pre( ht != NULL );

  ht->current_key    = key;
  ht->current_hvalue = hash(ht,key);
  index = ht->current_hvalue % ht->htable_size;
  
  for(c = ht->htable+index; *c; c = &((*c)->next))
    {
      if( (*c)->hvalue == ht->current_hvalue && cmp(ht,(*c)->key,key) == 0 ) 
	break;
    }
  ht->cursor = c;

  return (*c != NULL);
}

			/* --------------- */

ccl_ptr
ccl_hash_get(ccl_hash ht)
{
  ccl_pre( ht != NULL && *(ht->cursor) != NULL );

  return (*(ht->cursor))->object;
}

			/* --------------- */

void
ccl_hash_insert(ccl_hash ht, void *object)
{  
  ccl_pre( ht != NULL && ht->cursor != NULL );

  if( *(ht->cursor) != NULL ) 
    {
      delobject(ht,(*(ht->cursor))->object);
      (*(ht->cursor))->object = object;
    }
  else
    {
      htable_entry *e = s_alloc_entry();
      e->key = ht->current_key;
      e->object = object;
      e->hvalue = ht->current_hvalue;
      e->next = NULL;
      *(ht->cursor) = e;
    }
  ht->nb_entries++;
  s_possibly_increase_table_size(ht);
}

			/* --------------- */

void
ccl_hash_remove(ccl_hash ht)
{
  htable_entry *e;

  ccl_pre( ht != NULL && ht->cursor != NULL && *(ht->cursor) != NULL );

  e = (*(ht->cursor));
  *(ht->cursor) = e->next;
  delkey(ht,e->key);
  delobject(ht,e->object);
  s_delete_entry(e);
  
  ht->nb_entries--;
  s_possibly_decrease_table_size(ht);
}

			/* --------------- */

int
ccl_hash_size(ccl_hash ht)
{
  ccl_pre( ht != NULL );

  return ht->nb_entries;
}

			/* --------------- */

ccl_pointer_iterator *
ccl_hash_get_keys(ccl_hash ht)
{
  ccl_pre( ht != NULL );

  return (ccl_pointer_iterator *)s_new_iterator(ht,KEYS);
}

			/* --------------- */

ccl_pointer_iterator *
ccl_hash_get_elements(ccl_hash ht)
{
  ccl_pre( ht != NULL );

  return (ccl_pointer_iterator *)s_new_iterator(ht,OBJECTS);
}

			/* --------------- */

ccl_hash_entry_iterator *
ccl_hash_get_entries(ccl_hash ht)
{
  ccl_pre( ht != NULL );

  return (ccl_hash_entry_iterator *)s_new_iterator(ht,PAIRS);
}


			/* --------------- */

static void
s_possibly_increase_table_size(ccl_hash ht)
{
  if( ht->nb_entries > CCL_HASHTABLE_FILL_DEGREE*ht->htable_size && 
      ht->htable_size_index+1 < SIZE_FOR_TABLE_SIZE )
    {
      s_resize_table(ht,SIZE_FOR_TABLE[ht->htable_size_index+1]);
      ht->htable_size_index++;
    }
}

			/* --------------- */

static void
s_possibly_decrease_table_size(ccl_hash ht)
{
  if( ht->nb_entries < CCL_HASHTABLE_FILL_DEGREE*ht->htable_size 
      && ht->htable_size_index > CCL_HASHTABLE_INIT_SIZE_INDEX )
    {
      s_resize_table(ht,SIZE_FOR_TABLE[ht->htable_size_index-1]);
      ht->htable_size_index--;
    }
}

			/* --------------- */

static void
s_resize_table(ccl_hash ht, size_t newsize)
{
  int            i;
  int            index;
  htable_entry  *e;
  htable_entry  *next;
  htable_entry **new_table = ccl_new_array(htable_entry *,newsize);

  for(i = 0; i < ht->htable_size; i++)
    {
      for(e = ht->htable[i]; e; e = next)
	{
	  next = e->next;
	  index = e->hvalue % newsize;
	  e->next = new_table[index];
	  new_table[index] = e;
	}
    }

  ccl_delete(ht->htable);
  ht->cursor = NULL;
  ht->htable = new_table;
  ht->htable_size = newsize;
}

			/* --------------- */

static int 
s_iterator_has_more_elements(const ccl_hash_entry_iterator *i)
{
  ccl_pre( i != NULL );

  return ((hash_iterator *)i)->current != NULL;
}

			/* --------------- */

static ccl_hash_entry
s_hash_entry_iterator_next_element(ccl_hash_entry_iterator *i)
{
  ccl_hash_entry result;
  hash_iterator *it = (hash_iterator *)i;

  ccl_pre( i != NULL && ccl_iterator_has_more_elements(i));

  result.key = it->current->key;
  result.object = it->current->object;
  if( it->current->next != NULL )
    it->current = it->current->next;
  else
    {
      int i = it->index+1; 

      while( i < it->table->htable_size && it->table->htable[i] == NULL )
	i++;
      if( i == it->table->htable_size )
	it->current = NULL;
      else
	{
	  it->current = it->table->htable[i];
	  it->index   = i;
	}
    }

  return result;
}

			/* --------------- */

static ccl_ptr
s_ptr_iterator_next_element(ccl_pointer_iterator *i)
{
  ccl_hash_entry e =
    s_hash_entry_iterator_next_element((ccl_hash_entry_iterator *)i);

  return (((hash_iterator *)i)->mode == KEYS)?e.key:e.object;
}

			/* --------------- */

static void
s_iterator_delete_iterator(ccl_hash_entry_iterator *i)
{
  ccl_pre( i != NULL );

  ccl_delete(i);
}

			/* --------------- */

static hash_iterator *
s_new_iterator(ccl_hash ht, iterator_mode mode)
{
  int            i;
  hash_iterator *result;

  ccl_pre( ht != NULL );

  result = ccl_new(hash_iterator);
  result->iterator.has_more_elements = s_iterator_has_more_elements;
  if( mode == PAIRS ) 
    result->iterator.next_element = s_hash_entry_iterator_next_element;
  else
    ((ccl_pointer_iterator *)&result->iterator)->next_element = 
      s_ptr_iterator_next_element;
  result->iterator.delete_iterator = s_iterator_delete_iterator;
  result->mode = mode;
  result->table = ht;
  result->current = NULL;
  result->index = ht->htable_size;
  for(i = 0; i < ht->htable_size; i++)
    {
      if( ht->htable[i] != NULL )
	{
	  result->current = ht->htable[i];
	  result->index = i;
	  break;
	}
    }

  return result;
}

			/* --------------- */

