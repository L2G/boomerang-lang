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
module N = Names

exception Unsupported_feature
exception Internal_error

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = (Lexing.dummy_pos,Lexing.dummy_pos)

let global_loc = ref loc

let rep_t  id = <:ctyp<$uid:Id.id2string id$.$lid:Names.rep$>>
let pd_body_t   id = <:ctyp<$uid:Id.id2string id$.$lid:Names.pd_body$>>
let pd_t   id = <:ctyp<$uid:Id.id2string id$.$lid:Names.pd$>>

(* MLast expressions *)

(** parse function expression, parameterized by type id. *)
let parser_e tid = <:expr<$uid:Id.id2string tid$.$lid:Names.parser_fun$>>

(** pads handle *)
let pads_e = <:expr<$lid:Names.pads_handle$>>

(** make an expression from an id *)
let id_e id  = <:expr<$lid:Id.id2string id$>>

(* MLast patterns *)

(** pads handle *)
let pads_p = <:patt<$lid:Names.pads_handle$>>

(** make a pattern from an id *)
let id_p id = <:patt<$lid:Id.id2string id$>>

(* Pads library content *)

(** pads module expression *)
let pads_mod = <:expr<$uid:N.pads_mod$>>

(** pads where module expression *)
let pads_where_mod = <:expr<$uid:N.pads_mod$.Where>>

(** pads record module expression *)
let pads_record_mod = <:expr<$uid:N.pads_mod$.Record>>

(** pads datatype module expression *)
let pads_dt_mod = <:expr<$uid:N.pads_mod$.Datatype>>

(* types *)
let pd_header_t = <:ctyp<$uid:N.pads_mod$.$lid:N.pd_header_t$>>
let base_pd_t   = <:ctyp<$uid:N.pads_mod$.$lid:N.base_pd_t$>>
let handle_t    = <:ctyp<$uid:N.pads_mod$.$lid:N.handle_t$>>

(* functions *)
let get_pd_hdr = <:expr<$pads_mod$.$lid:N.get_pd_hdr$>>

(* Utility functions *)

(** wrap a pd body type with a header *)
let mk_pd_t body_t = <:ctyp<$uid:N.pads_mod$.pd $body_t$>>

let rev_flatten l = 
  let rec rf new_l = function 
      [] -> new_l
    | (l::ls) -> rf (l @ new_l) ls
  in rf [] l

let split3 ts = 
  let rec do_split (xs,ys,zs) = function
      [] -> (List.rev xs, List.rev ys, List.rev zs)
    | ((x,y,z)::ts) -> do_split (x::xs,y::ys,z::zs) ts
  in
    do_split ([],[],[]) ts

let rev_split3 ts = 
  let rec do_split (xs,ys,zs) = function
      [] -> (xs,ys,zs)
    | ((x,y,z)::ts) -> do_split (x::xs,y::ys,z::zs) ts
  in
    do_split ([],[],[]) ts

let munge_fields a f g =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
  in
  let rec mfs = function
      [] -> []
    | fd::fds -> let m = munge_field fd in (m @ (mfs fds))
  in 
    mfs

(** init is the initial state *)
let munge_fields_state a f g init =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
  in
  let rec mfs s = function
      [] -> []
    | fd::fds -> let m,s' = munge_field fd s in (m @ (mfs s' fds))
  in 
    mfs init

(** Like List.rev_map, its a tail recursive version of munge_fields,
which is equivalent to List.rev o munge_fields*)
let rev_munge_fields a f g =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
  in
  let rec mfs munged_fields = function
      [] -> munged_fields
    | fd::fds -> mfs (munge_field fd @ munged_fields) fds
  in 
    mfs []

let ty2mod tp = <:module_expr<$uid:Id.id2string tp$>>

(* convert a type application to a module application. *)
let rec gen_tyapp_exp = function
    Ast.TidTp tid -> <:module_expr<$uid:Id.id2string tid$>>
  | Ast.TpAppTp (tp_args,tp_fun) -> ty2mod_app tp_fun tp_args
  | _ -> PError.report_error loc "expected nested type application."
and apply_fun f arg = <:module_expr<$f$ $gen_tyapp_exp arg$>> 
and ty2mod_app tp_fun tp_args = List.fold_left apply_fun 
  <:module_expr<$uid:Id.id2string tp_fun$>> tp_args 

let bind_local_project mod_expr proj_fn =
  let mod_id = Id.freshid "M" in
  let mod_name = Id.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_expr$ in $proj_fn mod_name$>>

let apply_project tp_fun tp_args proj_str = 
  bind_local_project (ty2mod_app tp_fun tp_args) (fun m -> <:expr<$uid:m$.$lid:proj_str$>>)

let process_tp_lam dt ids = dt
let process_val_lam tp_ctxt vp_opt = tp_ctxt


(* needs commments about metadata. *)
type core_phase = 
    Description.table -> Description.metadata
  -> HostLanguage.tp_ctxt
  -> MLast.loc
  -> Ast.tp_def 
  -> (MLast.sig_item list * MLast.str_item list * Description.metadata)
  
module type COMP_PHASE = 
sig
  val gen : core_phase
end

type core_phase_v2 = 
    Description.table -> Description.metadata
  -> HostLanguage.tp_ctxt
  -> MLast.loc
  -> Ast.decl 
  (* Top-level declarations, followed by the module-level items. *)
  -> MLast.str_item list * (MLast.sig_item list * MLast.str_item list) * Description.metadata
  
module type COMP_PHASE_v2 = 
sig
  val gen : core_phase_v2
end

let cp_upgrade_1to2 cp1 = 
  fun dt current_descr tc loc (_,_,Ast.DefDecl def) -> 
    let s,st,m =  cp1 dt current_descr tc loc def in
      [],(s,st),m
