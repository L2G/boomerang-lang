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
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char** argv) {
  int i, tests, times, mul;
  int num[100000], tmp;
  unsigned long r1, r2;

  if (argc < 3) {
    goto usage;
  }
  tests = atoi(argv[1]);
  times = atoi(argv[2]);
  mul = tests*times;

  for (i = 0; i < mul; i++) {
    num[i] = i % tests;
  }
  for (i = 0; i < mul*100; i++) {
    r1 = rand() % mul;
    while ((r2 = rand() % mul) == r1);
    tmp = num[r1]; num[r1] = num[r2]; num[r2] = tmp; 
  }
  for (i = 0; i < mul; i++) {
    printf("%d ", num[i]);
  }
  printf("\n");
  return 0;

 usage:
  fprintf(stderr, "\nUsage: %s <#tests> <#times>\n\n", argv[0]);
  return -1;
}
