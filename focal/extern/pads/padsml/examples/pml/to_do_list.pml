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
*                 David Walker <dpw@cs.princeton.edu>                  *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
***********************************************************************)
open Built_ins

let sep_re = "/\\n[ \\t]*\\n/"

ptype classifier = 
    Active of "-" 
  | Urgent of "*-" 
  | Completed of "x"

ptype note_body = pstring_SE(sep_re)

ptype note = pre "/ */" * classifier * note_body

(* XXX really want plongest with separator. *)
ptype note_list = note plist_re(sep_re,"/\\n[ \\t]*\\n[ \\n\\t]*(---|%%% END)/")

ptype section_header = 
  pre "/---+ */" * 
  pstring_SE(sep_re) * 
  pre "/ *-*/" *
  pre "/\\n[ \\t]*\\n/"

ptype section = {header: section_header; body: note_list}

(* XXX really want plongest with separator *)
ptype source = section plist_re(sep_re,"/\\n[ \\t]*\\n[ \\n\\t]*%%% END/")
