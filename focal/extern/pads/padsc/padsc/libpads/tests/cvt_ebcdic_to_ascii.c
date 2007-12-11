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

/* typedef unsigned char Pbyte; */

Pbyte ea_tab[256] =
{
  0x00, 0x01, 0x02, 0x03, '?',  0x09, '?',  0x7f,
  '?',  '?',  '?',  0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, '?',  '?',  0x08,'?',
  0x18, 0x09, '?',  '?',  0x1c, 0x1d, 0x1e, 0x1f,
  '?',  '?',  '?',  '?',  '?',  0x0a, 0x17, 0x1b,
  '?',  '?',  '?',  '?',  '?',  0x05, 0x06, 0x07,
  '?',  '?',  0x16, '?',  '?',  '?',  '?',  0x04,
  '?',  '?',  '?',  '?',  0x14, 0x15, '?',  0x1a,
  0x20, '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x5b, 0x2e, 0x3c, 0x28, 0x2b, 0x21,
  0x26, '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x5d, 0x24, 0x2a, 0x29, 0x3b, 0x5e, /* not-sign 0xac -> circumflex 0x5e */
  0x2d, 0x2f, '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x7c, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,
  '?',  0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70,
  0x71, 0x72, '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, /* non-spacing macron 0xaf -> tilde 0x7e */
  0x79, 0x7a, '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, '?',  '?',  '?',  '?',  '?', '?',
  0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,
  0x51, 0x52, '?',  '?',  '?',  '?',  '?', '?',
  0x5c, '?',  0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
  0x59, 0x5a, '?',  '?',  '?',  '?',  '?', '?',
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, '?',  '?',  '?',  '?',  '?', '?',
};
Pbyte ae_tab[256] =
{
  0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f, 
  0x16, 0x19, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
  0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26, 
  0x18, '?',  0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f, 
  0x40, 0x4f, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d, 
  0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61, 
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 
  0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f, 
  0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 
  0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 
  0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 
  0xe7, 0xe8, 0xe9, 0x4a, 0xe0, 0x5a, 0x5f, 0x6d, 
  0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 
  0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 
  0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 
  0xa7, 0xa8, 0xa9, 0xc0, 0x6a, 0xd0, 0xa1, 0x07, 
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
};

int main(int argc, char** argv) {
  int c;
  while ((c = sfgetc(sfstdin)) != -1) {
    c = ea_tab[c];
    sfputc(sfstdout, c);
  }
  return 0;
}