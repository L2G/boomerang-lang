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
#include "pads-internal.h" /* for testing - normally do not include internal */
#include <stdio.h>
#include <stdlib.h>

const char* line1 = "%04d   %04d   %04d%04d  \n";
const char* line2 = " %04d   %04d%04d   %04d \n";
const char* line3 = "  %04d%04d   %04d   %04d\n";

P_NOGEN

int main(int argc, char** argv) {
  int i, lines, num_lines;
  const char* fname = "../../data/ex_data.biglibtest1";
  FILE* f;

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <num-lines>\n", argv[0]);
    return -1;
  }
  sscanf(argv[1], "%d", &num_lines);
  printf("\nNUM LINES: %d\n", num_lines);

  f = fopen(fname, "w");
  printf("fname = %s\n", fname);
  for (i = 1, lines = 0; 1; i += 12) {
    if (i > (9999-11)) { i = 1; }
    fprintf(f, line1, i + 0, i + 1, i +  2, i +  3);
    if (++lines >= num_lines) break;
    fprintf(f, line2, i + 4, i + 5, i +  6, i +  7);
    if (++lines >= num_lines) break;
    fprintf(f, line3, i + 8, i + 9, i + 10, i + 11);
    if (++lines >= num_lines) break;
  }
  fclose(f);
  return 0;
}
