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
(* Standard identifier names *)
val rep       : string
val pd        : string
val pd_body   : string
val mask      : string
val mask_body : string

val parser_fun : string
val genpd_fun : string
val default_fun : string
val printer_fun : string

val make_rep : string
val make_pd : string

(** Name of pads handle variable *)
val pads_handle : string
(** Name of constraint variable *)
val where_constraint : string

(** Name of the variable that stores the pd_hdr under construction during parsing. *)
val pd_hdr_var : string

(* Functions for creating rep and pd constructor names as strings from capitalized
   identifiers *)
val mk_rep_c_str  : Id.id -> string
val mk_pd_c_str  : Id.id -> string

(* Functions for creating rep and pd variable names as strings from uncapitalized
   identifiers *)
val mk_rep_str  : Id.id -> string
val mk_pd_str  : Id.id -> string

(* Functions for creating top level rep and pd type definition names as strings from
   ptype names. *)
val mk_top_level_rep_str  : Id.id -> string
val mk_top_level_pd_str  : Id.id -> string
val mk_top_level_pd_body_str  : Id.id -> string

(** Make a rep type variable name from a ptype name.*)
val mk_rep_tyvar_str : Id.id -> string
(** Make a pd body type variable name from a ptype name.*)
val mk_pd_body_tyvar_str : Id.id -> string

  (** Make a parser function variable name from a ptype name. *)
val mk_parser_funvar_str : Id.id -> string

(** Make a top-level traversal functor name from a ptype name. *)
val mk_top_level_traversal_str  : Id.id -> string
(** Make a traversal functor variable name from a ptype name. *)
val mk_traversal_funvar_str : Id.id -> string

(** A name for the default variant constructors. *)
val def_vt : string
val def_pd_vt : string
val err_vt     : string
val err_pd_vt  : string

(* Library member names *)
val pads_mod          : string (** name of pads library module. *)
val scan_lit_char     : string
val scan_lit_string   : string
val scan_lit_int      : string
val make_pd_hdr       : string
val make_tuple_pd_hdr : string
val get_pd_hdr        : string
val print_lit_char    : string
val print_lit_string  : string
val print_lit_int     : string
val print_lit_regexp_str : string

val record_init_pd : string
val record_finish_pd : string

val record_parse_first : string
val record_parse_next  : string

val record_absorb_first : string
val record_absorb_next : string

(** Name of char literal parsing functions *)
val record_absorb_first_litc : string
val record_absorb_next_litc  : string

(** Name of string literal parsing functions *)
val record_absorb_first_lits : string
val record_absorb_next_lits  : string

(** Name of int literal parsing functions *)
val record_absorb_first_liti : string
val record_absorb_next_liti  : string

(** Name of regular expression parsing functions *)
val record_absorb_first_litre : string
val record_absorb_next_litre  : string

(** Name of regular expression parsing functions for string-encoded reg. exps. *)
val record_absorb_first_litres : string
val record_absorb_next_litres  : string

(** Name of generic parsing function for datatypes. *)
val dt_make_rep : string
val dt_make_absorb_pd : string

(** Name of generic parsing function for datatype variants. *)
val dt_parse_vt : string
val dt_absorb_vt: string
val dt_absorb_litc: string
val dt_absorb_lits: string
val dt_absorb_liti: string
val dt_absorb_litre: string
val dt_absorb_litres: string

(** Name of generic parsing function for datatype cases. *)
val dt_parse_case : string
val dt_absorb_case: string
val dt_absorb_litc_case : string
val dt_absorb_lits_case : string
val dt_absorb_liti_case : string
val dt_absorb_litre_case : string
val dt_absorb_litres_case : string
val dt_gen_case : string

(** Name of generic parsing function for parsing underlying value of constrainted types. *)
val where_parse_underlying : string

(** Name of pads handle type *)
val handle_t     : string
val pd_header_t       : string
val mask_header_t     : string
val base_pd_t         : string
val base_mask_t       : string

(* Traversal related names. *)
val traversal_functor : string
val traversal_mod : string
val tool_mod : string
val tool_rec : string
val init_fun : string
val traversal_fun : string

(* Traversal related names. *)
val untraversal_functor : string
val untraversal_mod : string
val untool_mod : string
val untool_rec : string
val untraversal_fun : string
