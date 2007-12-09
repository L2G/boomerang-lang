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
P_NOGEN
#include <stdio.h>

int main(int argc, char** argv) {
  char fname[1000];
  char* h;
  Sfio_t* io;
  Pint32   header = 3;
  Pint32   val1   = 1;
  Pint32   val2   = 2;
  Pint32   val3   = 3;

  h = getenv("HOSTSHORT");
  sprintf(fname, "../../../examples/data/endian.%s", h);
  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");
  sfwrite(io, (void*)&header, sizeof(header));
  sfwrite(io, (void*)&val1,   sizeof(val1));
  sfwrite(io, (void*)&val2,   sizeof(val2));
  sfwrite(io, (void*)&val3,   sizeof(val3));
  sfclose(io);
  return 0;
}
