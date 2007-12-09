/* $Id: ccl-string.c,v 1.2 2005/11/24 10:47:44 point Exp $ */
#include <stdio.h>
#include <ccl-assert.h>
#include <ccl-string.h>

# define BUFFER_INIT_SIZE 1000
# define CACHE_SIZE 10

struct binary_tree {
  char *value;
  unsigned int hvalue;
  struct binary_tree *lt, *gt;
};

static ccl_ustring CACHE[CACHE_SIZE];
static int CACHE_POS;
static struct binary_tree *UNIQUE_STRING_TREE;

void
ccl_string_init(void)
{
  int i;

  for(i = 0; i < CACHE_SIZE; i++)
    CACHE[i] = NULL;
  CACHE_POS = 0;

  UNIQUE_STRING_TREE = NULL;
}

			/* --------------- */

static void
s_delete_binary_tree(struct binary_tree *bt)
{
  if( bt != NULL )
    {
      ccl_delete(bt->value);
      s_delete_binary_tree(bt->gt);
      s_delete_binary_tree(bt->lt);
      ccl_delete(bt);
    }
}

			/* --------------- */

void
ccl_string_terminate(void)
{
  s_delete_binary_tree(UNIQUE_STRING_TREE);
  UNIQUE_STRING_TREE = NULL;
}

			/* --------------- */

uint32_t
ccl_string_hash(const void *key)
{
  const char   *s;
  uint32_t result = 0;

  ccl_pre( key != NULL );
  
  for(s = (const char *)key; *s; s++)
    result = result*19+(uint32_t)*s;

  return result;
}

			/* --------------- */

int
ccl_string_equals(const void *str1, const void *str2)
{
  return strcmp(str1,str2) == 0;
}

			/* --------------- */

char *
ccl_string_dup(const char *s)
{
  size_t   len = strlen(s);
  char *result = ccl_calloc(sizeof(char),len+1);

  strcpy(result,s);

  return result;
}

			/* --------------- */

char *
ccl_string_format_new(const char *fmt, ...)
{
  char *result;
  va_list  pa;

  va_start(pa,fmt);
  result = ccl_string_format_new_va(fmt,pa);
  va_end(pa);

  return result;
}

			/* --------------- */

char *
ccl_string_format_new_va(const char *fmt, va_list pa)
{
  size_t sz = 0;
  char *result = NULL;
    
  ccl_string_format_va(&result,&sz,fmt,pa);

  return result;
}

			/* --------------- */

void
ccl_string_format(char **dst, size_t *size, const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_string_format_va(dst,size,fmt,pa);
  va_end(pa);
}

			/* --------------- */

static void
s_set_buffer_size(char **pbuf, size_t *psize, size_t size)
{
  if( size == 0 )
    {
      if( *pbuf != NULL )
	{
	  ccl_delete(*pbuf);
	  *pbuf = NULL;
	  *psize = 0;
	}
    }
  else if( *psize < size )
    {
      if( *pbuf == NULL )
	*pbuf = ccl_new_array(char,size);
      else 
	*pbuf = ccl_realloc(*pbuf,size);
      *psize = size;
    }
}

			/* --------------- */

void
ccl_string_format_va(char **dst, size_t *psize, const char *fmt, va_list pa)
{
  size_t aux;

  if( psize == NULL )
    psize = &aux;

  s_set_buffer_size(dst,psize,BUFFER_INIT_SIZE);

  for(;;)
    {
      int ret = vsnprintf(*dst,*psize,fmt,pa);

      if( (size_t)ret < *psize )
	break;
      s_set_buffer_size(dst,psize,*psize*2);
    }
}

			/* --------------- */

void
ccl_string_format_append(char **dst, const char *fmt, ...)
{
  va_list  pa;
  
  va_start(pa,fmt);
  ccl_string_format_append_va(dst,fmt,pa);
  va_end(pa);
}

			/* --------------- */

void
ccl_string_format_append_va(char **dst, const char *fmt, va_list pa)
{
  char *tmp = ccl_string_format_new_va(fmt,pa);

  if( *dst == NULL )
    *dst = tmp;
  else
    {
      int len_dst = strlen(*dst);
      int len_tmp = strlen(tmp);
      char *res = ccl_new_array(char,len_dst+len_tmp+1);
      strncpy(res,*dst,len_dst);
      strncpy(res+len_dst,tmp,len_tmp);
      ccl_delete(*dst);
      ccl_delete(tmp);
      *dst = res;
    }

}

# define IS_DIGIT(c) (((c)=='0')||((c)=='1')||((c)=='2')||((c)=='3')|| \
		      ((c)=='4')||((c)=='5')||((c)=='6')||((c)=='7')|| \
		      ((c)=='8')||((c)=='9'))

int
ccl_string_parse_int(const char *s)
{
  int result;

  if( s == NULL ) result = 0;
  else
    {
      result = 0;
      if( *s != '0' )
	{
	  while( *s && IS_DIGIT(*s) ) 
	    result = result*10+((*s)-'0');	  
	}
    }
  return result;
}

			/* --------------- */

int
ccl_string_parse_boolean(const char *s)
{
  int result;
  
  if( s == NULL ) result = 0;
  else if( ccl_string_equals(s,"true") == 0 ||
	   ccl_string_equals(s,"TRUE") == 0 ||
	   ccl_string_equals(s,"enabled") == 0 ||
	   ccl_string_equals(s,"ENABLED") == 0 ||
	   ccl_string_equals(s,"1") == 0 )
    result = 1;
  else result = 0;

  return result;
}

			/* --------------- */

static struct binary_tree **
s_find_unique_string(struct binary_tree **bt, const char *us, unsigned int hv)
{
  int c;

  if( *bt == NULL )
    return bt;

  if( hv < (*bt)->hvalue )
    return s_find_unique_string(&((*bt)->lt),us,hv);
				
  if( hv > (*bt)->hvalue )
    return s_find_unique_string(&((*bt)->gt),us,hv);

  c = strcmp((*bt)->value,us);
  if( c < 0 )
    return s_find_unique_string(&((*bt)->lt),us,hv);

  if( c > 0 )
    return s_find_unique_string(&((*bt)->gt),us,hv);

  return bt;
}

			/* --------------- */

ccl_ustring
ccl_string_make_unique(const char *s)
{
  int i;
  ccl_ustring result = NULL;

  for(i = 0; i < CACHE_SIZE && CACHE[i] != NULL; i++)
    {
      if( strcmp(CACHE[i],s) == 0 )
	result = CACHE[i];
    }

  if( result == NULL )
    {
      unsigned int hval = ccl_string_hash(s);
      struct binary_tree **pbt = 
	s_find_unique_string(&UNIQUE_STRING_TREE,s,hval);

      if( *pbt == NULL )
	{
	  *pbt = ccl_new(struct binary_tree);
	  (*pbt)->value = ccl_string_dup(s);
	  (*pbt)->hvalue = hval;
	  (*pbt)->lt = (*pbt)->gt = NULL;
	}
      result = (*pbt)->value;
      CACHE[CACHE_POS++] = result;
      CACHE_POS %= CACHE_SIZE;
    }

  return result;
}


			/* --------------- */

ccl_ustring
ccl_string_make_unique_from_int(int i)
{
  char tmp[100];

  sprintf(tmp,"%d",i);

  return ccl_string_make_unique(tmp);
}
