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
(* Generic version of plist, taking a separator, terminator, and
   termination predicate. Consumption of the last read element of the
   list is speculative -- it depends on the value returned by 
   predicate.
   Note that, in the case of parse failure, the list is automatically terminated
   and the element discarded. The termination predicate is not consulted.
*)

module type Plist_specs = sig
  type sep
  type term

  (** A processed version of the separator. *)
  type proc_sep
    (** A processed version of the terminator. *)
  type proc_term

  val pre_process_sep : sep -> Pads.handle -> proc_sep
  val pre_process_term : term -> Pads.handle -> proc_term

  val post_process_sep : proc_sep -> Pads.handle -> unit
  val post_process_term : proc_term -> Pads.handle -> unit

    (** Attempt to absorb the next separator.  Also doubles as a test
	for termination. None indicates that the list should be
	terminated, whereas Some indicates that list process should
	continue.
	This function must *not* raise a Speculation_failure.
    *)
  val absorb_sep : proc_sep -> Pads.pd_header -> Pads.handle -> Pads.pd_header option
  val term_match : Pads.padsc_handle -> proc_term -> int -> Padsc.perror_t

  val print_sep : sep -> Pads.handle -> unit
  end

(** Datatype used by list termination predicates. *)
type term_action = 
    Terminate_keep
    | Terminate_discard
    | Proceed

  (* It is an error to return Terminate_discard when the list is empty *)
type ('rep,'pd) term_pred = 'rep list -> 'pd list -> term_action

module Make_plist(Specs: Plist_specs) (Alpha : Type.S): Type.ValParam 
  with type rep = Alpha.rep list
  and  type pd_body = Alpha.pd list
  and  type val_param_type = Specs.sep * Specs.term * (Alpha.rep, Alpha.pd) term_pred
