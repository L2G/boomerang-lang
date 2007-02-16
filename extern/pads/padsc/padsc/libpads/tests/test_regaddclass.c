/***********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2007 AT&T Knowledge Ventures            *
*                         All Rights Reserved                          *
*         This software is licensed by AT&T Knowledge Ventures         *
*           under the terms and conditions of the license in           *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*                 Robert Gruber <bob.gruber@gmail.com>                 *
*              Kathleen Fisher <kfisher@research.att.com>              *
*            Yitzhak Mandelbaum <yitzhakm@cs.princeton.edu>            *
*                                                                      *
***********************************************************************/
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>
#include <regex.h>
#include <error.h>

int is_f(int c) { return c == 'f'; }

int main(int argc, char** argv) {
  regex_t          preg;
  int              i;
  char            *exp, *str;
  int              c_flags   = REG_AUGMENTED|REG_EXTENDED|REG_DELIMITED|REG_MULTIREF;
  int              e_flags   = 0;
  int              cret, eret, pin;
  regmatch_t       match[100];

  regaddclass("f", is_f);

  exp = "/[[:f:]]+/";
  str = "fff";
  error(0, "Matching pattern %s against string %s", exp, str);
  error(0, "(should match string \"fff\")");
  pin = 0;
  if (pin) { e_flags |= REG_LEFT; }
  cret = regcomp(&preg, exp, c_flags);
  if (cret) {
    error(ERROR_FATAL, "Failed to compile re %s, cret = %d", exp, cret);
  }
  error(0, "compiled %s, nsub = %d", exp, preg.re_nsub);
  eret = regexec(&preg, str, preg.re_nsub+1, match, e_flags);
  error(0, "match of RE %s against string %s produced %s",
	exp, str, (eret ? "FALSE" : "TRUE" ));
  if (!eret) {
    for (i = 0; i <= preg.re_nsub; i++) {
      error(0, "      sub %d so %d eo %d = \"%.*s\"", i, match[i].rm_so, match[i].rm_eo, match[i].rm_eo - match[i].rm_so, str + match[i].rm_so);
    }
  }
  return 0;
}
