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
open Generic_tool.Rec_ver
open Generic_untool.Rec_ver

type rep = int
type pd_body = Pads.base_pd_body
type pd = Pads.base_pd

let default = 0

let parse pads = 
  let (res,pd,rep) = Pint_c.pint32_read (Pads.get_padsc_handle pads) 
    Padsc.p_CheckAndSet in      
  let c_pos = Pads.get_current_pos pads in
  let new_pd = Pads.base_pd_of_pbase_pd pd c_pos in
    match res with
	Padsc.P_OK -> (Int32.to_int rep,new_pd)
      | Padsc.P_ERR -> 
	  if Pads.IO.is_speculative pads then
	    raise Pads.Speculation_failure
	  else (default,new_pd)

let print rep pd pads = 
  let new_pd = Pads.pbase_pd_of_base_pd pd in
    ignore (Pint_c.pint32_write2io 
	      (Pads.get_padsc_handle pads) (Pads.get_out_stream pads) 
	      new_pd (Int32.of_int rep))
		
let gen_pd r = Pads.gen_base_pd

module Traverse = struct
  let init tool = tool.int_t.bt_init
  let traverse tool r pd state = 
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
      tool.int_t.bt_process state res h
end

module Untraverse = struct
  let untraverse untool t = untool.processInt t
end
