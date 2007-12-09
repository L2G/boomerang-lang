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

  val absorb_sep : proc_sep -> Pads.pd_header -> Pads.handle -> Pads.pd_header option
  val term_match : Pads.padsc_handle -> proc_term -> int -> Padsc.perror_t

  val print_sep : sep -> Pads.handle -> unit
end

module Make_plist(Specs: Plist_specs) (Alpha : Type.S) =
struct
  type rep = Alpha.rep list
  type pd_body = Alpha.pd list
  type pd = pd_body Pads.pd

  type val_param_type = Specs.sep * Specs.term

  let mklist (hd,tl) = hd::tl

  let gen_pd sep_term  reps =
    let rec gen_pds hdr pds = function
        [] -> (hdr,pds)
      | r::rs ->
	  let (pd, hdr) = Pads.Record.gen_pd hdr (Alpha.gen_pd r)
	  in gen_pds hdr (pd::pds) rs
    in
    let hdr = Pads.spanless_pd_hdr in
    let (hdr, pds) = gen_pds hdr [] reps in
      (hdr, List.rev pds)

  let parse (sep,term) pads =
    (* Perform any necessary pre-processing on the sep and term. *)
    let proc_sep = Specs.pre_process_sep sep pads in
    let proc_term = Specs.pre_process_term term pads in
    let is_done () = 
      (Padsc.p_io_at_eor_OR_EOF (Pads.get_padsc_handle pads) = 1) ||
	match (Specs.term_match (Pads.get_padsc_handle pads) proc_term 0) with
	    Padsc.P_OK -> true	      
	  | Padsc.P_ERR -> false
    in
    let rec continue prev_pos rev_reps  hdr rev_pds = 
      let current_pos = Pads.get_current_pos pads in
	if Pads.eq_pos prev_pos current_pos || is_done () then 
	  (rev_reps, hdr, rev_pds)
	else 
	  (* Note: see interface for explanation of optional result. *)
	  match Specs.absorb_sep proc_sep hdr pads with
	      None -> (rev_reps, hdr, rev_pds)
	    | Some hdr ->
		let (r_e, pd_e, hdr) = Pads.Record.parse_next Alpha.parse hdr pads in
		  continue current_pos (r_e::rev_reps) hdr (pd_e::rev_pds)
    in
    let hdr = Pads.make_empty_pd_hdr pads in
    let (res_rep,(res_hdr,res_body)) =
      if is_done() then ([],(hdr,[]))
      else
	let current_pos = Pads.get_current_pos pads in
        let (r_e, pd_e, hdr) = Pads.Record.parse_next Alpha.parse hdr pads in
	let (rev_reps, hdr, rev_pds) = 
	  continue current_pos [r_e] hdr [pd_e]
	in
	  List.rev rev_reps, (hdr, List.rev rev_pds)
    in
    let _ = Specs.post_process_sep proc_sep pads in
    let _ = Specs.post_process_term proc_term pads in
      (res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))

  let print (sep,term) rep (hdr,elt_pds) pads = 
    let rec _print = function
	([],[]) -> ()
      | ([r],[p]) -> Alpha.print r p pads
      | (r::rs,p::ps) -> (
	  Alpha.print r p pads;
	  Specs.print_sep sep pads;
	  _print (rs, ps))
    in _print (rep,elt_pds)	  

  module Traverse =
  struct
    let init tool () = tool.list_t.l_init ()
    let traverse tool rep (hdr, pds) state =
      let alpha_init = Alpha.Traverse.init tool in
      let alpha_trav = Alpha.Traverse.traverse tool in
      let rec _traverse rep_list pd_list state p_state =
        match rep_list, pd_list with
            [], [] -> tool.list_t.l_process_empty p_state
          | [r], [pd] ->
              let (state, elt_s_opt) = tool.list_t.l_project_next state in
              let elt_s = match elt_s_opt with
                  Some s -> s
                | None -> alpha_init ()
              in
              let elt_s = alpha_trav r pd elt_s in
                tool.list_t.process_last p_state elt_s
          | r::rs, pd::pds ->
              let (state, elt_s_opt) = tool.list_t.l_project_next state in
              let elt_s = match elt_s_opt with
                  Some s -> s
                | None -> alpha_init ()
              in
              let elt_s = alpha_trav r pd elt_s in
              let p_state =
                tool.list_t.process_next p_state elt_s
              in
		_traverse rs pds state p_state
      in
      let p_state = tool.list_t.l_start state hdr in
	_traverse rep pds state p_state
  end

  module Untraverse = struct
    let untraverse untool t = 
      List.map 
        (Alpha.Untraverse.untraverse untool)
        (untool.processList t)
  end
end
