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

(** Datatype used by list termination predicates. *)
type term_action = 
    Terminate_keep
    | Terminate_discard
    | Proceed

  (* It is an error to return Terminate_discard when the list is empty *)
type ('rep,'pd) term_pred = 'rep list -> 'pd list -> term_action

module Make_plist(Specs: Plist_specs) (Alpha : Type.S) =
struct
  type rep = Alpha.rep list
  type pd_body = Alpha.pd list
  type pd = pd_body Pads.pd

  type val_param_type = Specs.sep * Specs.term * (Alpha.rep, Alpha.pd) term_pred

  let mklist (hd,tl) = hd::tl

  let gen_pd args  reps =
    let rec gen_pds hdr pds = function
        [] -> (hdr,pds)
      | r::rs ->
	  let (pd, hdr) = Pads.Record.gen_pd hdr (Alpha.gen_pd r)
	  in gen_pds hdr (pd::pds) rs
    in
    let hdr = Pads.spanless_pd_hdr in
    let (hdr, pds) = gen_pds hdr [] reps in
      (hdr, List.rev pds)

  let parse (sep,term,term_pred) pads =
    (* Perform any necessary pre-processing on the sep and term. *)
    let proc_sep = Specs.pre_process_sep sep pads in
    let proc_term = Specs.pre_process_term term pads in
    let is_done rev_reps rev_pds = 
      (Padsc.p_io_at_eor_OR_EOF (Pads.get_padsc_handle pads) = 1) 
      || match (Specs.term_match (Pads.get_padsc_handle pads) proc_term 0) with
	  Padsc.P_OK -> true	      
	| Padsc.P_ERR -> false
    in
    let rec continue prev_pos rev_reps  hdr rev_pds = 
      let current_pos = Pads.get_current_pos pads in
	if Pads.eq_pos prev_pos current_pos || is_done rev_reps rev_pds then 
	  (rev_reps, hdr, rev_pds)
	else 
	  begin
	    Pads.IO.checkpoint true pads;
	    let spec_level = Pads.IO.get_spec_level pads in
	      (* Note: see interface for explanation of optional result. *)
	      match Specs.absorb_sep proc_sep hdr pads with
		  None -> 
		    Pads.IO.restore pads;
		    (rev_reps, hdr, rev_pds)
		| Some hdr ->
		    let ended, result = 
		      try 
			let (r_e, pd_e, new_hdr) = Pads.Record.parse_next Alpha.parse hdr pads in
			let new_reps = r_e::rev_reps in
			let new_pds = pd_e::rev_pds in
			  false, (new_reps, new_hdr, new_pds)
		      with Pads.Speculation_failure when (Pads.IO.get_spec_level pads = spec_level) ->
			true, (rev_reps, hdr, rev_pds)
		    in
		    let (r_reps, r_hdr, r_pds) = result in
		    let terminated = term_pred r_reps r_pds in
		    let _ = if (Pads.IO.get_spec_level pads) = spec_level then
			if ended then
			  Pads.IO.restore pads
			else
			  match terminated with 
			    Terminate_discard -> Pads.IO.restore pads
			  | Terminate_keep -> Pads.IO.commit pads
			  | Proceed -> Pads.IO.commit pads
		    in
		      if ended then 
			result
		      else
			match terminated with 
			    Terminate_discard -> (rev_reps, hdr, rev_pds)
			  | Terminate_keep -> result
			  | Proceed -> continue current_pos r_reps r_hdr r_pds
	  end
    in
    let hdr = Pads.make_empty_pd_hdr pads in
    let (res_rep,(res_hdr,res_body)) =
      let is_term = match term_pred [] [] with
	  Terminate_discard -> 
	    Pads.Log.report_warning "Plist_gen_spec.parse" None Pads.No_info
	      "Attempt by user-supplied predicated to discard element from empty list"
	      pads; true
	| Terminate_keep -> true
	| Proceed -> false
      in
	if is_term || (is_done [] []) then ([],(hdr,[]))
	else 
	  let current_pos = Pads.get_current_pos pads in
	  let _ = Pads.IO.checkpoint true pads in
	  let spec_level = Pads.IO.get_spec_level pads in
	    try 
              let (r_e, pd_e, new_hdr) = Pads.Record.parse_next Alpha.parse hdr pads in	    
	      let init_reps = [r_e] in
	      let init_pds = [pd_e] in
	      let uncommitted = (Pads.IO.get_spec_level pads) = spec_level in
		match term_pred init_reps init_pds with 
		    Terminate_discard -> 
		      if uncommitted then Pads.IO.restore pads;
		      ([],( hdr, []))
		  | Terminate_keep -> 
		      if uncommitted then Pads.IO.commit pads;
		      (init_reps, (new_hdr, init_pds))
		  | Proceed -> 
		      if uncommitted then Pads.IO.commit pads;
		      let (rev_reps, hdr, rev_pds) =
			continue current_pos init_reps new_hdr init_pds 
		      in
			List.rev rev_reps, (hdr, List.rev rev_pds)
	    with
		Pads.Speculation_failure ->
		  let sl = Pads.IO.get_spec_level pads in
		    if sl = spec_level then 
		      (Pads.IO.restore pads; ([],( hdr, [])))
		    else
		      raise Pads.Speculation_failure
    in
    let _ = Specs.post_process_sep proc_sep pads in
    let _ = Specs.post_process_term proc_term pads in
      (res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))

  let print (sep,_,_) rep (hdr,elt_pds) pads = 
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
