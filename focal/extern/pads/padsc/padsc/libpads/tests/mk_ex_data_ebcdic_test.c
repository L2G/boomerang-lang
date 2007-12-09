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
  P_t*          pads;
  const char* fname = "../../data/ex_data.ebcdic_test";
  char tmp[100];
  Sfio_t* io;
  int len;

  for (len = 0; len < 100; len++) {
    tmp[len] = P_EBCDIC_SPACE;
  }

  if (P_ERR == P_libopen(&pads, 0, 0, 1)) {
    error(2, "*** P_libopen failed ***");
    return -1;
  }

  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");

  if (-1 == (len = PDCI_int64_2e_io(pads, io, 0)))  goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MIN_INT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MAX_INT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e_io(pads, io, P_MAX_UINT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MIN_INT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MAX_INT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e_io(pads, io, P_MAX_UINT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MIN_INT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MAX_INT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e_io(pads, io, P_MAX_UINT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MIN_INT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e_io(pads, io, P_MAX_INT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e_io(pads, io, P_MAX_UINT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, P_EBCDIC_NEWLINE);

  sfclose(io);
  return 0;

 write_err:
  error(0, "XXX write error occurred");
  sfclose(io);
  return -1;
}
