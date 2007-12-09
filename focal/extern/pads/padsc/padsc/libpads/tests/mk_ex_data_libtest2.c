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

P_NOGEN

int main(int argc, char** argv) {
  char fname[1000];
  char* h;
  Sfio_t* io;
  Pint8    data1 = -2;
  Puint8   data2 = 2;
  Pint16   data3 = -2;
  Puint16  data4 = 2;
  Pint32   data5 = -2;
  Puint32  data6 = 2;
  Pint64   data7 = -2;
  Puint64  data8 = 2;

  h = getenv("HOSTSHORT");
  sprintf(fname, "../../data/ex_data.libtest2.%s", h);
  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");
  sfwrite(io, (void*)&data1, sizeof(data1));
  sfwrite(io, (void*)&data2, sizeof(data2));
  sfwrite(io, (void*)&data3, sizeof(data3));
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfwrite(io, (void*)&data5, sizeof(data5));
  sfwrite(io, (void*)&data6, sizeof(data6));
  sfwrite(io, (void*)&data7, sizeof(data7));
  sfwrite(io, (void*)&data8, sizeof(data8));
  sfputc(io, '\n');
  sfwrite(io, (void*)&data1, sizeof(data1));
  sfwrite(io, (void*)&data2, sizeof(data2));
  sfwrite(io, (void*)&data3, sizeof(data3));
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfwrite(io, (void*)&data5, sizeof(data5));
  sfwrite(io, (void*)&data6, sizeof(data6));
  sfwrite(io, (void*)&data7, sizeof(data7));
  /*  sfwrite(io, (void*)&data8, sizeof(data8)); */
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfputc(io, '\n');
  sfclose(io);
  return 0;
}
