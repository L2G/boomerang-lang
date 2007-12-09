(***********************************************************************
*                                                                      *
*             This software is part of the padsml package              *
*           Copyright (c) 2006-2007 Knowledge Ventures Corp.           *
*                         All Rights Reserved                          *
*        This software is licensed by Knowledge Ventures Corp.         *
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
*                   Knowledge Ventures Labs Research                   *
*                           Florham Park NJ                            *
*                                                                      *
*            Yitzhak Mandelbaum <yitzhak@research.att.com>>            *
*                                                                      *
***********************************************************************)
type 'a precord = 'a
type 'a_pdb precord_pd_body = 'a_pdb

module Precord (Alpha : Type.S) =
struct
  type rep = Alpha.rep
  type pd_body = Alpha.pd_body
  type pd = Alpha.pd

  let gen_pd rep = Alpha.gen_pd rep

  let parse pads =
    let (r,(hdr,body)) = Alpha.parse pads in
    let new_hdr = Pads.find_eor hdr pads in
      (r,(new_hdr,body))

  let print rep pd pads = 
    Pads.print_open_rec pads;
    Alpha.print rep pd pads;
    Pads.print_close_rec pads

  module Traverse = Alpha.Traverse
  module Untraverse = Alpha.Untraverse

end
