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

#include "pads-internal.h"

P_NOGEN

const char* false_true [] = { "TRUE", "FALSE" };

int is_foo(int c) { return c == 'f' || c == 'o' || isdigit(c); }

/* used to register the new class: */
void reg_foo(void) { regaddclass("foo", is_foo); }

int main(int argc, char** argv) {
  P_t          *pads;
  Pdisc_t      my_disc = Pdefault_disc;
  Pio_disc_t  *io_disc;
  P_REGEXP_DECL_NULL(regexp);
  size_t          matchlen;
  int             i, n, eret, bor, eor, pin;
  Pbyte       *begin, *end;
  const char     *exp, *str;
  size_t          exp_len, str_len;
  regflags_t      e_flags;

  reg_foo();

  io_disc = P_nlrec_make(0);
  if (P_ERR == P_libopen(&pads, &my_disc, io_disc, 1)) {
    error(ERROR_FATAL, "*** P_libopen failed ***");
  }

  if (argc != 3 && argc != 6) {
    error(ERROR_FATAL, "\nusage: test_re2 <pattern> <string> [ bor eor pin ]\n");
  }
  bor = eor = pin = 0;
  if (argc == 6) {
    bor = atoi(argv[3]);
    eor = atoi(argv[4]);
    pin = atoi(argv[5]);
  }
  exp      = argv[1];
  str      = argv[2];
  exp_len  = strlen(exp);
  str_len  = strlen(str);
  begin    = (Pbyte*)str;
  end      = (Pbyte*)(str + str_len);

  if (P_ERR == Pregexp_compile_cstr(pads, exp, &regexp)) {
    error(ERROR_FATAL, "Failed to compile re %s", P_qfmt_cstr_n(exp, exp_len));
  }
  error(0, "\ncompiled regexp, nsub = %d\n", regexp.preg.re_nsub);

  e_flags = 0;
  if (pin) { e_flags |= REG_LEFT; }
  if (!bor) { e_flags |= REG_NOTBOL; }
  if (!eor) { e_flags |= REG_NOTEOL; }
  eret = PDCI_regexp_match(pads, &regexp, begin, end, e_flags, Pcharset_ASCII);
  matchlen = regexp.match[0].rm_eo - regexp.match[0].rm_so;
  error(0, "match of RE %s against string %s produced matchlen %d, res %s",
	P_qfmt_cstr_n(exp, exp_len), P_qfmt_cstr_n(str, str_len), (int)matchlen, false_true[eret]);
  if (!eret) {
#ifdef DEBUG_REGEX
    n = regexp.preg.re_nsub;
#else
    n = 0;
#endif
    for (i = 0; i <= n; i++) {
      error(0, "      sub %d so %d eo %d = \"%.*s\"",
	    i,
	    regexp.match[i].rm_so,
	    regexp.match[i].rm_eo,
	    regexp.match[i].rm_eo - regexp.match[i].rm_so,
	    str + regexp.match[i].rm_so);
    }
  }
  error(0, "");
  return 0;
}
