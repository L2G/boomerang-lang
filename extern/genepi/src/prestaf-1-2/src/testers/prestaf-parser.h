/* $Id: prestaf-parser.h,v 1.1.1.1 2005/09/26 12:11:04 point Exp $ */
#ifndef __PRESTAF_PARSER_H__
# define __PRESTAF_PARSER_H__

# include <ccl-parse-tree.h>
# include <ccl-exception.h>

CCL_DECLARE_EXCEPTION(prestaf_parse_exception,exception);

CCL_EXTERN ccl_parse_tree
prestaf_load_file(const char *filename);

#endif /* __PRESTAF_PARSER_H__ */
