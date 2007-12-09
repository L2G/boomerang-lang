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
/*
 * rbuf interface - internal
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __RBUF_INTERNAL_H__
#define __RBUF_INTERNAL_H__

#include "rbuf.h"

struct RBuf_s {
  RMM_t        *mgr;
  void         *buf;
  size_t       bufSize;
  size_t       eltSize;
  size_t       numElts;
  size_t       extraBytes;
  size_t       maxEltHint;
#ifdef RBUF_DEBUG
  // file and line of new call
  const char  *new_file;
  int          new_line;
  // file and line of last reserve
  const char  *reserve_file;
  int          reserve_line;
#endif
};

struct RMM_s {
  void           *vm;
  RMM_allin1_fn  fn;
};

#endif  /*  __RBUF_INTERNAL_H__  */
