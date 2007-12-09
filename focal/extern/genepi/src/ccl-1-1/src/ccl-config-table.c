/* $Id: ccl-config-table.c,v 1.1 2006/03/15 16:01:53 point Exp $ */
#include "ccl-log.h"
#include "ccl-assert.h"
#include "ccl-memory.h"
#include "ccl-string.h"
#include "ccl-config-table.h"


typedef struct preference_st preference;
struct preference_st {
  preference *next;
  char       *name;
  char      *value;
};

struct ccl_config_table_st {
  uint32_t       refcount;
  ccl_config_table *        parent;
  int      nb_preferences;
  preference *preferences;
};

			/* --------------- */

ccl_config_table *
ccl_config_table_add_reference(ccl_config_table *conf)
{
  ccl_pre( conf != NULL );

  conf->refcount++;

  return conf;
}

			/* --------------- */

void
ccl_config_table_del_reference(ccl_config_table *conf)
{
  ccl_pre( conf != NULL );

  if( --conf->refcount == 0 )
    {
      preference *p, *next;

      for(p = conf->preferences; p != NULL; p = next)
	{
	  next = p->next;
	  ccl_string_delete(p->name);
	  ccl_string_delete(p->value);
	  ccl_delete(p);
	}
      ccl_delete(conf);
    }
}
			/* --------------- */

ccl_config_table *
ccl_config_table_inherits(ccl_config_table *parent)
{
  ccl_config_table *result = ccl_new(struct ccl_config_table_st);
  
  result->refcount = 1;
  if( parent != NULL )
    {
      result->parent = ccl_config_table_add_reference(parent);
    }
  result->preferences = NULL;

  return result;
}

			/* --------------- */

void
ccl_config_table_set(ccl_config_table *conf, const char *name, const char *value)
{
  preference **pp;

  for(pp = &conf->preferences; *pp; pp = &((*pp)->next))
    {
      if( ccl_string_equals(name,(*pp)->name) )
	break;
    }
  
  if( *pp == NULL )
    {
      *pp = ccl_new(preference);
      (*pp)->next  = NULL;
      (*pp)->name  = ccl_string_dup(name);
      (*pp)->value = NULL;
      conf->nb_preferences++;
    }
  else
    {
      ccl_assert( (*pp)->value != NULL );
      ccl_string_delete((*pp)->value);
    }
  (*pp)->value = ccl_string_dup(value);
}

			/* --------------- */


void
ccl_config_table_add(ccl_config_table *conf, ccl_config_table *others)
{
  preference *p;

  ccl_pre( conf != NULL ); ccl_pre( others != NULL );

  for(p = others->preferences; p; p = p->next)
    ccl_config_table_set(conf,p->name,p->value);
}

const char *
ccl_config_table_get(ccl_config_table *conf, const char * name)
{
  const char *result = NULL;
  preference      *p;

  ccl_pre( conf != NULL );
  
  for(p = conf->preferences; p && result == NULL; p = p->next)
    {
      if( ccl_string_equals(name,p->name) )
	result = p->value;
    }

  if( result == NULL && conf->parent != NULL )
    result = ccl_config_table_get(conf->parent,name);

  return result;  
}

			/* --------------- */

int
ccl_config_table_get_integer(ccl_config_table *conf, const char * name)
{
  const char *value = ccl_config_table_get(conf,name);
  
  return ccl_string_parse_int(value);
}

			/* --------------- */

int
ccl_config_table_get_boolean(ccl_config_table *conf, const char * name)
{
  const char *value = ccl_config_table_get(conf,name);
  
  return ccl_string_parse_boolean(value);
}

			/* --------------- */

ccl_list
ccl_config_table_get_names(ccl_config_table *conf)
{
  ccl_list result;
  preference  *p;

  ccl_pre( conf != NULL ); 

  if( conf->parent != NULL )
    result = ccl_config_table_get_names(conf->parent);
  else
    result = ccl_list_create();

  for(p = conf->preferences; p != NULL; p = p->next)
    {
      if( ccl_list_get_index(result,p->name,ccl_string_compare) < 0 )
	ccl_list_add(result,p->name);
    }

  return result;
}

			/* --------------- */

void
ccl_config_table_save(ccl_config_table *conf, FILE *output)
{
  ccl_list names = ccl_config_table_get_names(conf);
  ccl_pair     p;

  ccl_pre( conf != NULL ); ccl_pre( output != NULL );


  for(p = FIRST(names); p; p = CDR(p))
    {
      const char *value = ccl_config_table_get(conf,(const char *)CAR(p));
      fprintf(output,"%s = %s\n",(const char *)CAR(p),value);
    }
  fflush(output);
  ccl_list_delete(names);
}

			/* --------------- */

# define ISSPACE(c) (((c) == ' ') || ((c) == '\t'))

static char *
s_append_char(char *s, char c)
{
  int      len = strlen(s);
  char *result = ccl_new_array(char,len+2);

  strcpy(result,s);
  result[len] = c;

  return result;
}

			/* --------------- */

static int
s_read_line(FILE *input, char **name, char **value)
{
  int c, st = 0;
  char *tmp;

  for(;;)
    {
      c = fgetc(input);
      switch( st ) {
      case 0 :
	if( c < 0 ) return 0;
	else if( ! ISSPACE(c) || c == '\n' ) 
	  {
	    *name = ccl_new_array(char,2);
	    **name = c;
	    st = 1;
	  }
	break;
      case 1 :
	if( c < 0 || c == '\n' ) 
	  { 
	    ccl_string_delete(*name); 
	    return 0; 
	  }
	else if( ISSPACE(c) ) st = 4;
	else if( c == '=' )   st = 2;
	else
	  {
	    tmp = s_append_char(*name,(char)c);
	    ccl_string_delete(*name);
	    *name = tmp;
	  }
	break;
      case 4 :
	if( c == '=' ) st = 2;
	else if( ! ISSPACE(c) ) 
	  {
	    ccl_string_delete(*name); 
	    return 0; 
	  }
	break;
      case 2 :
	if( ISSPACE(c) ) 
	  continue;

	if( c == '\n' || c < 0 )
	  {
	    *value = ccl_new_array(char,1);
	    return 1;
	  }
			/* --------------- */


	else if( c == '\\' )
	  {
	    *value = ccl_new_array(char,1);
	    st = 6;
	  }
	else 
	  {
	    *value = ccl_new_array(char,2);
	    **value = c;
	    st = 5;
	  }
	break;
      case 5 :
	if( c == '\n' || c < 0 ) return 1;
	else if( c == '\\' ) st = 6;
	else 
	  {
	    tmp = s_append_char(*value,(char)c);
	    ccl_string_delete(*value);
	    *value = tmp;
	  }
	break;
      case 6 :
	if( c < 0 )
	  {
	    tmp = s_append_char(*value,'\\');
	    ccl_string_delete(*value);
	    *value = tmp;

	    return 1;
	  }
	else if( c == '\n' ) st = 5;
	else 
	  {	    
	    if( c == 'n' )      c = '\n';
	    else if( c == 'r' ) c = '\r';
	    else if( c == 't' ) c = '\t';
	    tmp = s_append_char(*value,c);
	    ccl_string_delete(*value);
	    *value = tmp;
	    st = 5;
	  }
	break;
      };
    } 

  return 0;
}

			/* --------------- */

void
ccl_config_table_load(ccl_config_table *conf, FILE *input)
{
  char *name = NULL;
  char *value = NULL;

  while( s_read_line(input,&name,&value) )
    {
      ccl_config_table_set(conf,name,value);
      ccl_string_delete(name);
      ccl_string_delete(value);
    }  
}

			/* --------------- */

void
ccl_config_table_display(ccl_config_table *conf)
{
  ccl_list names = ccl_config_table_get_names(conf);
  ccl_pair     p;

  ccl_pre( conf != NULL ); 

  for(p = FIRST(names); p; p = CDR(p))
    {
      const char *value = ccl_config_table_get(conf,(const char *)CAR(p));
      ccl_display("%s = %s\n",(const char *)CAR(p),value);
    }
  ccl_list_delete(names);
}
