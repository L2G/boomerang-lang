/* $Id: ccl-config-table.h,v 1.1 2006/03/15 16:01:53 point Exp $ */
#ifndef __CCL_CONFIG_TABLE_H__
# define __CCL_CONFIG_TABLE_H__

# include <stdio.h>
# include <ccl-globals.h>
# include <ccl-list.h>

typedef struct ccl_config_table_st ccl_config_table;

# define ccl_config_table_create() ccl_config_table_inherits(NULL)

CCL_EXTERN ccl_config_table *
ccl_config_table_add_reference(ccl_config_table *conf);

CCL_EXTERN void
ccl_config_table_del_reference(ccl_config_table *conf);

CCL_EXTERN ccl_config_table *
ccl_config_table_inherits(ccl_config_table *parent);

CCL_EXTERN void
ccl_config_table_set(ccl_config_table *conf, const char *name, 
		     const char *value);

CCL_EXTERN void
ccl_config_table_add(ccl_config_table *conf, ccl_config_table *others);

CCL_EXTERN const char *
ccl_config_table_get(ccl_config_table *conf, const char * name);

CCL_EXTERN int
ccl_config_table_get_integer(ccl_config_table *conf, const char * name);

CCL_EXTERN int
ccl_config_table_get_boolean(ccl_config_table *conf, const char * name);

CCL_EXTERN ccl_list
ccl_config_table_get_names(ccl_config_table *conf);

CCL_EXTERN void
ccl_config_table_save(ccl_config_table *conf, FILE *output);

CCL_EXTERN void
ccl_config_table_load(ccl_config_table *conf, FILE *input);

CCL_EXTERN void
ccl_config_table_display(ccl_config_table *conf);

#endif /* __CCL_CONFIG_TABLE_H__ */
