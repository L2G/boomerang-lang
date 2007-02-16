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
(* Version of plist with string separator, terminated by lack of separator. *)
type 'a plist_nosep = 'a list
type 'a_pdb plist_nosep_pd_body = 'a_pdb Pads.pd list

module Plist_nosep_orig = 
  Plist_gen.Make_plist(
    struct 
      type sep = string
      type term = string
      type proc_sep = sep
      type proc_term = term

      let pre_process_sep sep pads = sep
      let pre_process_term term pads = term

      let post_process_sep proc_sep pads = ()
      let post_process_term proc_term pads = ()

      let absorb_sep sep hdr pads = 
	match Padsc.pstr_lit_match (Pads.get_padsc_handle pads) sep 1 with
	    Padsc.P_OK -> Some hdr
	  | Padsc.P_ERR -> None (* No sep found, so list terminates. *)

      let term_match pads sep i = Padsc.P_ERR

      let print_sep = Pads.print_str_lit
    end)

module Plist_nosep (Alpha : Type.S) = struct
  module Tmp = Plist_nosep_orig(Alpha)
  type rep = Tmp.rep
  type pd_body = Tmp.pd_body
  type pd = Tmp.pd

  type val_param_type = string
      
  let parse s = Tmp.parse (s,s)
  let print s = Tmp.print (s,s)
  let gen_pd s = Tmp.gen_pd (s,s)
  module Traverse = Tmp.Traverse
  module Untraverse = Tmp.Untraverse
end
