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
#include <error.h>

#include "pads-internal.h"

int main(int argc, char** argv) {
  int             i;
  int             mapped_vals;
  mapped_vals = 0;
  for (i = 0; i < 255; i++) {
    if (P_mod_ae_tab[i] != 255) {
      mapped_vals++;
      if (P_mod_ea_tab[P_mod_ae_tab[i]] != i) {
	error(0, "  i = %02x, ae[i] = %02x, ea[%02x] = %02x",
	      i, P_mod_ae_tab[i], P_mod_ae_tab[i], P_mod_ea_tab[P_mod_ae_tab[i]]);
      }
    }
  }
  error(0, "mapped ae vals = %d", mapped_vals);
  mapped_vals = 0;
  for (i = 0; i < 255; i++) {
    if (P_mod_ea_tab[i] != 255) {
      mapped_vals++;
      if (P_mod_ae_tab[P_mod_ea_tab[i]] != i) {
	error(0, "  i = %02x, ea[i] = %02x, ae[%02x] = %02x",
	      i, P_mod_ea_tab[i], P_mod_ea_tab[i], P_mod_ae_tab[P_mod_ea_tab[i]]);
      }
    }
  }
  error(0, "mapped ea vals = %d", mapped_vals);
  return 0;
}
