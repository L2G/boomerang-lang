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
(** Abstract syntax tree types for PadsML language *)

type pos = (int*int) * (int*int)   (* beginning and end position in source code *)
type id = Id.id                    (* possibly implemented as a string * int *)
    
val make_pos      : (int*int) -> (int*int) -> pos
val string_of_pos : pos -> string
val start_pos     : pos
  
type exp = HostLanguage.expr
    
type tp = TidTp of id 
	  | TupleTp of comp_tp list
	  | RecordTp of field list
	  | TpAppTp of tp list * id  (* Application to a type. *)
	  | ValAppTp of tp * exp     (* Applicaiton to a value. *)
	  | WhereTp of id * tp * exp 
	  | ArrayTp of tp * array_comp list

and  field = AbsorbField of comp_tp 
             | FullField of (id * tp)
	     | GenField of (id * tp * exp)  (* Computed/generated field. *)
		 
and array_comp = ArrSep of exp | ArrTerm of term_comp | ArrPred of exp
and term_comp = ArrNosep | ArrTermExp of exp
  
and comp_tp = Type of tp | Exp of exp
  
type val_param = id * HostLanguage.tp
    
type case_pat = HostLanguage.patt
		      
type dt_case = 
    AbsorbCase of case_pat * id * comp_tp
  | FullCase of case_pat * id * tp
  | GenCase of case_pat * id * tp * exp

type dt_variant = 
    AbsorbVar of id * comp_tp
  | FullVar of id * tp

type dt_default_variant = 
    FullDefault of id option * tp
  | GenDefault of id option * tp * exp

type dt_body = ImplicitDT of dt_variant list * dt_default_variant option
	       | CaseDT of exp * dt_case list
		   
type tp_def_body = TpDef of tp
                 | DtDef of dt_body

type tp_def = val_param option * tp_def_body

type decl_body = DefDecl of tp_def

type decl = id list * id * decl_body
		
type descr = decl list
