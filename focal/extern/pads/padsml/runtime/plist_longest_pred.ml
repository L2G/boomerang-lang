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
type 'a plist_longest_pred = 'a list
type 'a_pdb plist_longest_pred_pd_body = 'a_pdb Pads.pd list

module Plist_base = 
  Plist_gen_spec.Make_plist(
    struct 
      type sep = unit
      type term = unit
      type proc_sep = sep
      type proc_term = term

      let pre_process_sep sep pads = sep
      let pre_process_term term pads = term

      let post_process_sep proc_sep pads = ()
      let post_process_term proc_term pads = ()

      let absorb_sep s h pads = Some h
      let term_match pads t i = Padsc.P_ERR

      let print_sep () pads = ()
    end)

module Plist_longest_pred (Alpha : Type.S) = struct
  module Tmp = Plist_base(Alpha)
  type rep = Tmp.rep
  type pd_body = Tmp.pd_body
  type pd = Tmp.pd

  type val_param_type = (Alpha.rep, Alpha.pd) Plist_gen_pred.term_pred

  let check_longest_term = function
      [] -> Plist_gen_spec.Proceed
    | pd::pds -> 
	if Pads.pd_is_ok pd then Plist_gen_spec.Proceed
	else Plist_gen_spec.Terminate_discard

  let check_term term_pred rev_reps rev_pds = 
    if term_pred rev_reps rev_pds then
      Plist_gen_spec.Terminate_keep
    else
      check_longest_term rev_pds

  let parse term_pred = Tmp.parse ((),(),check_term term_pred)
  let print term_pred = Tmp.print ((),(),check_term term_pred)
  let gen_pd term_pred = Tmp.gen_pd ((),(),check_term term_pred)    
  module Traverse = Tmp.Traverse
  module Untraverse = Tmp.Untraverse
end
