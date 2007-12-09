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
type 'a plist_longest = 'a list
type 'a_pdb plist_longest_pd_body = 'a_pdb Pads.pd list

module Plist_longest_orig = 
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

module Plist_longest (Alpha : Type.S) = struct
  module Tmp = Plist_longest_orig(Alpha)
  type rep = Tmp.rep
  type pd_body = Tmp.pd_body
  type pd = Tmp.pd

  let check_term _ = function
      [] -> Plist_gen_spec.Proceed
    | pd::pds -> 
	if Pads.pd_is_ok pd then Plist_gen_spec.Proceed
	else Plist_gen_spec.Terminate_discard
      
  let parse = Tmp.parse ((),(),check_term)
  let print = Tmp.print ((),(),check_term)
  let gen_pd = Tmp.gen_pd ((),(),check_term)    
  module Traverse = Tmp.Traverse
  module Untraverse = Tmp.Untraverse
end
