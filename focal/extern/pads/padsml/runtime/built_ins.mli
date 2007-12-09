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
val peor : string

(* Built-in type constructors. *)

(* Version of plist with character separators and terminators. *)
type 'a plist_ch = 'a list
type 'a_pdb plist_ch_pd_body = 'a_pdb Pads.pd list
module Plist_ch (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = char * char

(* Default variant of lists. Alias of plist_ch. *)
type 'a plist = 'a list
type 'a_pdb plist_pd_body = 'a_pdb Pads.pd list
module Plist (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = char * char

(* Version of plist with regular-expression separators and terminators. *)
type 'a plist_re = 'a list
type 'a_pdb plist_re_pd_body = 'a_pdb Pads.pd list
module Plist_re (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = string * string

(* Version of plist with string separators and terminators. *)
type 'a plist_st = 'a list
type 'a_pdb plist_st_pd_body = 'a_pdb Pads.pd list
module Plist_st (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = string * string

(* Version of plist with string separator, terminated by lack of separator. *)
type 'a plist_nosep = 'a list
type 'a_pdb plist_nosep_pd_body = 'a_pdb Pads.pd list
module Plist_nosep (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = string

type 'a plist_np = 'a list
type 'a_pdb plist_np_pd_body = 'a_pdb Pads.pd list
module Plist_np (Alpha : Type.S) : Type.S
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list

type 'a plist_longest = 'a list
type 'a_pdb plist_longest_pd_body = 'a_pdb Pads.pd list
module Plist_longest (Alpha : Type.S) : Type.S
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list

type 'a plist_longest_pred = 'a list
type 'a_pdb plist_longest_pred_pd_body = 'a_pdb Pads.pd list
module Plist_longest_pred (Alpha : Type.S) : Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = (Alpha.rep, Alpha.pd) Plist_gen_pred.term_pred


type 'a ptry = 'a
type 'a_pdb ptry_pd_body = 'a_pdb
module Ptry (Alpha : Type.S) : Type.S
  with type rep = Alpha.rep
  and  type pd_body = Alpha.pd_body

type 'a precord = 'a
type 'a_pdb precord_pd_body = 'a_pdb
module Precord (Alpha : Type.S) : Type.S
  with type rep = Alpha.rep
  and  type pd_body = Alpha.pd_body

type 'a popt = 'a option
type 'a_pdb popt_pd_body = 'a_pdb Pads.pd option
module Popt (Alpha : Type.S) : Type.S
  with type rep = Alpha.rep option
  and  type pd_body = Alpha.pd option
