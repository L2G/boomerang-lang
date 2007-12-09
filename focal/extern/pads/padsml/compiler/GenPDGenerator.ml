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
module C = Common
module D = Description
module N = Names
module HL = HostLanguage
  
(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : Id.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = (Lexing.dummy_pos,Lexing.dummy_pos)

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>

let gen_record_genpd gg fields is_tuple =
  (** list of field names in record. *)
  let names =
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      fields
  in

  (* The rep and pd parameter patterns of the genpd function *)
  let (reps_p,pds_p) = 
    (* The first two cases deal with empty and singleton tupels. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple patterns.  
       Singleton records appear to be okay.
    *)
    match names with
	[] -> <:patt<()>>, <:patt<()>>
      | [name] ->
	  if is_tuple then
	    (<:patt<$r_p name$>>, 
	     <:patt<$p_p name$>>)
	  else (<:patt<{$list:[(r_p name, r_p name)]$}>>, 
		<:patt<{$list:[(p_p name, p_p name)]$}>>)
      | _ ->
	  if is_tuple then
	    (<:patt<($list:List.map r_p names$)>>, 
	     <:patt<($list:List.map p_p names$)>>)
	  else (<:patt<{$list:List.map (fun n -> r_p n, r_p n) names$}>>, 
		<:patt<{$list:List.map (fun n -> p_p n, p_p n) names$}>>)
  in


  (* Make the genpd function given a body. *)
  let fun_e body = <:expr<fun $reps_p$ -> $body$>> in
 
  (* Make the first expression, given the rest. *)
  let first_e the_rest_e = 
    <:expr<let $lid:N.pd_hdr_var$ = $C.pads_mod$.spanless_pd_hdr in $the_rest_e$>> in
    
  (* Make the pattern-expression pair for a full or gen field. *)
  let mk_full_pe id tp = 
    (<:patt<($p_p id$,$lid:N.pd_hdr_var$)>>,
     let genpd_fn = gg tp in
       <:expr<$C.pads_mod$.Record.gen_pd $lid:N.pd_hdr_var$ ($genpd_fn$ $r_e id$)>>)
  in

  (* Assemble pattern-expression pairs used to build let expressions below. *)
  let patt_expr_list = 
    C.munge_fields
      (fun ctp -> []) (* Absorb field *)
      (fun (id,tp) -> [mk_full_pe id tp]) (* Normal field *)
      (fun (id,tp,_) ->  [mk_full_pe id tp]) (* Compute field *)
      fields
  in

  (* Make the list of lets, given the final expression. *)
  let mk_lets_e = List.fold_right (fun pe e -> <:expr<let $list:[pe]$ in $e$>>) 
    patt_expr_list in

  (* The body of the pd *)
  let pd_body_e = 
    match names with
	[] -> <:expr<()>>
      | [name] ->
	  if is_tuple then p_e name
	  else <:expr<{$list:[(p_p name,p_e name)]$}>> 
      | _ ->
	  if is_tuple then <:expr<($list:List.map p_e names$)>>
	  else <:expr<{$list:List.map (fun id -> p_p id,p_e id) names$}>> 
  in

  let pd_e = <:expr<($lid:N.pd_hdr_var$, $pd_body_e$)>> in

    fun_e (first_e (mk_lets_e pd_e))

let rec gen_genpd dt tc loc = function

    Ast.TupleTp ctps ->
      let fresh_name = (Id.id2string (Id.freshid "elt")) ^ "_" in
      let rid i = fresh_name ^ (string_of_int i) in
      let pid i = (rid i)^"_pd" in

      (* Convert a tuple-element AST node to a record-field AST node. *)
      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
      let fields = List.rev fields_rev in
	gen_record_genpd (gen_genpd dt tc loc) fields true
	  
  | Ast.TidTp tid ->
      (match D.lookup_descr dt tid with
	   None -> PError.report_error loc ("GenPDGenerator: Type " ^ (Id.id2string tid) ^ " not found.")
	 | Some {name=_;kind=D.Base;tyclass=D.Current} -> 
	     <:expr<$lid:N.genpd_fun$>>	     
	 | Some {name=_;kind=D.Base;tyclass=_} ->
	     <:expr<$uid:Id.id2string tid$.$lid:N.genpd_fun$>>
	 | _ -> PError.report_error loc
	     "GenPDGenerator: Unsupported feature: type identifiers cannot have higher kinds.")

  | Ast.ValAppTp (tp_fun,exp) -> 
      let genpd_fn = gen_genpd dt tc loc tp_fun in
      let e = HL.ocaml_of_host_e exp in
	<:expr<$genpd_fn$ $e$>>

  | Ast.TpAppTp (tp_args,tp_fun) ->
      (match D.lookup_descr dt tp_fun with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	 | Some {name=_;kind=D.Fun(_);tyclass=D.Current} ->
	     <:expr<$lid:N.genpd_fun$>>
	 | Some {name=_;kind=D.Fun(_);tyclass=_} ->
	     C.apply_project tp_fun tp_args N.genpd_fun
	 | _ -> PError.report_error loc "Type use does not match kind."
      )
	
  | Ast.WhereTp (id,tp,exp) ->
      let genpd_fn = gen_genpd dt tc loc tp in
      let e = HL.ocaml_of_host_e exp in
	<:expr<fun $lid:Id.id2string id$ -> $uid:N.pads_mod$.Where.$lid:N.genpd_fun$ ($genpd_fn$ $C.id_e id$) $e$>>

  | _ -> PError.report_error loc "unsupported feature"
	    
let gen_tp_genpd dt tc loc = function
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level.
       
       XXX: doesn't this belong in the type checker?
    *)
    Ast.RecordTp fields ->
      gen_record_genpd (gen_genpd dt tc loc) fields false
  | b -> gen_genpd dt tc loc b

let gen_dtp_genpd dt tc loc tp = 
  
  let fresh_rep = Id.id2string (Id.freshid "r") in

  (* Build the genpd expression for a variant with a subcomponent. *)
  let mk_full_expr rep_con pd_con sub_tp = 
    let vpatt = <:patt<$uid:rep_con$ $lid:fresh_rep$>> in
    let genpd_fn = gen_genpd dt tc loc sub_tp in
    let vexpr = <:expr<$C.pads_mod$.Datatype.gen_pd 
      ($genpd_fn$ $lid:fresh_rep$) 
      (fun pd -> $uid:pd_con$ pd)>> 
    in
      (vpatt,None,vexpr)
  in
    
  (* Build the genpd expression for a variant with no subcomponent *)
  let mk_absorb_expr rep_con pd_con = 
    let vpatt = <:patt<$uid:rep_con$>> in
    let vexpr = <:expr<$C.pads_mod$.Datatype.gen_pd_empty $uid:pd_con$>> in
      (vpatt,None,vexpr)
  in
    
  let all_cases = match tp with
      Ast.ImplicitDT (vs,def_opt) ->

	let mkcase = function
	    Ast.FullVar(id,tp) -> mk_full_expr (rid id) (pid id) tp 
	  | Ast.AbsorbVar(id,_) -> mk_absorb_expr (rid id) (pid id) 
	in
	let def_case = match def_opt with
	    Some (Ast.GenDefault(id_opt,tp,_)) | Some (Ast.FullDefault(id_opt,tp)) ->
	      let (rep_name,pd_name) = 
		match id_opt with
		    None -> N.def_vt,N.def_pd_vt
		  | Some id -> N.mk_rep_str id, N.mk_pd_str id
	      in
		mk_full_expr rep_name pd_name tp
	  | None -> mk_absorb_expr N.err_vt N.err_pd_vt
	in
	  (List.map mkcase vs)@[def_case]

    | Ast.CaseDT (descr,cases) -> 

	let mkcase = function
	    Ast.FullCase(_,id,tp) | Ast.GenCase(_,id,tp,_) ->
	      mk_full_expr (rid id) (pid id) tp 
	  | Ast.AbsorbCase(_,id,_) -> 
	      mk_absorb_expr (rid id) (pid id) 
	in
	  (* Default error case, in case nothing else matches. *)
	let def_case = mk_absorb_expr N.err_vt N.err_pd_vt in
	  (List.map mkcase cases)@[def_case]
  in
    <:expr<fun [$list:all_cases$]>>
      
let gen dt current_descr tc loc (val_param_opt, tp_def) =

  let genpd_body_t = <:ctyp<rep -> pd>> in

  let genpd_body_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_genpd dt tc loc tp
    | Ast.DtDef dtp_body -> gen_dtp_genpd dt tc loc dtp_body
  in

  (* parameterize body with val. param *)
  let (genpd_t, genpd_fun) = match val_param_opt with
      None -> (genpd_body_t, genpd_body_fun)
    | Some (p_id,p_type) -> 
	(<:ctyp<$HL.ctyp_of_tp p_type$ -> $genpd_body_t$>>,
	 let p_name = Id.id2string p_id in
	   <:expr<fun $lid:p_name$ -> $genpd_body_fun$>>)
  in
    ([<:sig_item<value gen_pd : $genpd_t$>>],
     [<:str_item<value rec $lid:N.genpd_fun$ = $genpd_fun$>>],
     current_descr)
