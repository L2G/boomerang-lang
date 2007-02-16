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

type rep = unit
type pd_body = unit
type pd = unit Pads.pd

let gen_pd () = Pads.gen_base_pd

let parse pads = 
  if Pads.IO.is_speculative pads then
    Pads.IO.commit pads
  else ();
  (), Pads.make_empty_pd pads

let print rep pd pads = ()
    
module Traverse = struct
  let init tool = tool.unit_t.bt_init
  let traverse tool r pd state = 
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
      tool.unit_t.bt_process state res h
end

module Untraverse = struct
  let untraverse untool t = untool.processUnit t
end


(* type 'a pcommit = 'a *)
(* type 'a_pdb pcommit_pd_body = 'a_pdb *)
(* module Pcommit (Alpha : Type.S) = *)
(* struct *)
(*   type rep = Alpha.rep *)
(*   type pd_body = Alpha.pd_body *)
(*   type pd = Alpha.pd *)

(*   let gen_pd = Alpha.gen_pd *)

(*   let parse pads = *)
(*     let x = Alpha.parse pads in *)
(*       Pads.IO.commit(pads);x *)
(*       (\* No need to check whether PD is okay, as any error would have *)
(* 	 raised a Speculation_failure exception. *\) *)

(*   let print = Alpha.print *)
    
(*   module Traverse = Alpha.Traverse *)

(* end *)
