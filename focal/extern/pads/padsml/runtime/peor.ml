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
type rep = unit
type pd_body = Pads.base_pd_body
type pd = Pads.base_pd

let default = ()

let parse pads = 
  let at_eor = (Padsc.p_io_at_eor (Pads.get_padsc_handle pads)) = 1 in
    if at_eor then 
      ((),Pads.make_empty_pd pads)
    else
      if Pads.IO.is_speculative pads then
	raise Pads.Speculation_failure
      else 
	let pd = 	  
	  ({Pads.state = Pads.ps_init;
	    Pads.nerr= 1; 
	    Pads.error_code=Pads.Syn; 
	    Pads.error_info=Pads.No_info;
	    Pads.span=Pads.make_empty_span pads},
	   ())
	in
	  (default,pd)
	
let print rep pd pads = ()

let gen_pd () = Pads.gen_base_pd

module Traverse(Tool:Generic_tool.S) = struct
  let init = Tool.Unit.init
  let traverse r pd state = 
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
      Tool.Unit.process state res h
end

open Generic_untool.Rec_ver
module Untraverse = struct
  let untraverse untool t = untool.processUnit t
end
