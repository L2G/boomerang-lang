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
exception Unsupported_feature
exception Internal_error

val global_loc : MLast.loc ref

val rep_t  : Id.id  -> MLast.ctyp
val pd_body_t : Id.id  -> MLast.ctyp
val pd_t   : Id.id  -> MLast.ctyp

(** wrap a pd body type with a header *)
val mk_pd_t : MLast.ctyp -> MLast.ctyp

(* MLast expressions *)

(** parse function expression, parameterized by type id. *)
val parser_e : Id.id -> MLast.expr

(** pads handle *)
val pads_e : MLast.expr

(** make an expression from an id. id must start with lowercase letter. *)
val id_e : Id.id -> MLast.expr

(* MLast patterns *)

(** pads handle *)
val pads_p : MLast.patt

(** make a pattern from an id *)
val id_p : Id.id -> MLast.patt

(* Pads library content *)

(** pads module expression *)
val pads_mod : MLast.expr

(** pads record module expression *)
val pads_record_mod : MLast.expr

(** pads where module expression *)
val pads_where_mod : MLast.expr

(** pads datatype module expression *)
val pads_dt_mod : MLast.expr

(* types *)
val pd_header_t   : MLast.ctyp
val base_pd_t     : MLast.ctyp
val handle_t      : MLast.ctyp

(* functions *)
val get_pd_hdr : MLast.expr

(* Utility functions *)

val rev_flatten : 'a list list -> 'a list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val rev_split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

(* munge fields from beginning to end of list. Not tail recursive.*)
val munge_fields :
  (Ast.comp_tp -> 'a list) ->
  (Ast.id * Ast.tp -> 'a list) ->
  (Ast.id * Ast.tp * Ast.exp -> 'a list) -> Ast.field list -> 'a list

(* munge fields from beginning to end of list. Not tail recursive.
   Thread abstract state from beginning to end.
*)
val munge_fields_state :
  (Ast.comp_tp -> 's -> 'a list * 's) ->
  (Ast.id * Ast.tp -> 's -> 'a list * 's) ->
  (Ast.id * Ast.tp * Ast.exp -> 's -> 'a list *'s) -> 
  's ->
  Ast.field list -> 'a list

(** Like List.rev_map, its a tail recursive version of munge_fields,
    which is equivalent to List.rev o munge_fields*)
val rev_munge_fields :
  (Ast.comp_tp -> 'a list) ->
  (Ast.id * Ast.tp -> 'a list) ->
  (Ast.id * Ast.tp * Ast.exp -> 'a list) -> Ast.field list -> 'a list

(** Convert a type name to a module expression. *)
val ty2mod : Id.id -> MLast.module_expr

(** Convert a type application to a module application. *)
val ty2mod_app : Id.id -> Ast.tp list -> MLast.module_expr

(** Bind the module expression to a fresh module variable and pass the
    name of the fresh variable to the supplied function to create an expr. *)
val bind_local_project : MLast.module_expr -> (string -> MLast.expr) -> MLast.expr

(** Apply a functor to its arguments (potentially containing nested
    applications), and then project out the specified value. 
    Assumes that third argument is a lower-case identifier. 
    
    This function is a composition of ty2mod_app and bind_local_project.
*)
val apply_project : Id.id -> Ast.tp list -> string -> MLast.expr


val process_tp_lam: Description.table -> Id.id list 
  -> Description.table
val process_val_lam: HostLanguage.tp_ctxt -> Ast.val_param option 
  -> HostLanguage.tp_ctxt

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
  
val cp_upgrade_1to2: core_phase -> core_phase_v2 
