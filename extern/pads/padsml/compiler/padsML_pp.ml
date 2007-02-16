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
(* !!! make sure to include pa_extend.cmo in camlp4 command line when
   doing syntax extensions. e.g.
ocamlc -pp "camlp4 pa_o.cmo pa_extend.cmo pr_o.cmo q_MLast.cmo" -c foo.ml
*)

open Pcaml
open Ast

module D = Description
type metadata = D.metadata = {
  name : Id.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

let base_descriptions = [{name=Id.makeid "Pint";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint8";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint16";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint32";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint64";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint8";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint16";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint32";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint8_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint16_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint32_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pint64_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint8_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint16_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Puint32_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pchar";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pfloat32";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pfloat64";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pstring";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Ptimestamp_explicit_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pstring_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pstring_ME";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Pstring_SE";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Peor";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Peof";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Popt";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_re";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_st";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_nosep";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_np";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_longest";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Plist_longest_pred";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Precord";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Ptry";kind=D.Fun [
			    {name=Id.freshid "arg";kind=D.Base;tyclass=D.Defined}
			  ];tyclass=D.Defined;};
			 {name=Id.makeid "Pcommit";kind=D.Base;tyclass=D.Defined;};
			 {name=Id.makeid "Punit";kind=D.Base;tyclass=D.Defined;}]

let base_type_table = List.fold_left (fun t d -> D.add_descr t d) D.new_table base_descriptions

let descr_table = ref base_type_table
let val_ctxt = ref HostLanguage.empty_ctxt

let hoc_e = HostLanguage.host_of_ocaml_e
let hoc_p = HostLanguage.host_of_ocaml_p

let make_type_id id_s = Id.makeid (String.capitalize id_s)

EXTEND
GLOBAL: str_item;
  exprlit :
    [ [ i = INT -> <:expr<$int:i$>>
      | s = STRING -> <:expr<$str:s$>>
      | c = CHAR -> <:expr<$chr:c$>>]]
  ;

(*   pfoo : *)
(*     [[ x1 = SELF; "+"; x2 = SELF -> x1 + x2 *)
(*      | i = INT -> i]] *)
(*   ; *)

  hlexpr :
    [[
	(* Need to limit to use of literals as full expressions
	   confuse the parser.  The problem is that the parser for
	   expressions tries to eat any ';' that are intened as part
	   of the description (field separators). For actual
	   expressions, then, need to surround by ':'. *)
	lit_e = exprlit -> hoc_e lit_e
    | ":"; e = expr; ":" -> hoc_e e
    |"pre"; str_e = STRING -> HostLanguage.host_of_regexp <:expr<$str:str_e$>>
    |"pre"; id_e = LIDENT -> HostLanguage.host_of_regexp <:expr<$lid:id_e$>>]]
  ;

  (* XXX: Why use two separate precedence levels here? *)
  cptype : 
    [ [tp = ptype  LEVEL "ptype1" -> Type tp] 
    | [e = hlexpr -> Exp e]]
  ;

  pfield :
    [[tp = ptype -> 
      (match tp with
	     TupleTp([Exp e]) -> AbsorbField (Exp e)
	   | _              -> AbsorbField (Type tp))
     | id = LIDENT; ":"; c_tp = ptype; "="; e = expr LEVEL "expr1" -> 
	 GenField (Id.makeid id, c_tp, hoc_e e)
     | id = LIDENT; ":"; tp = ptype -> FullField (Id.makeid id,tp)
     ]]
  ;

  ptype : 
    [ "star"
	(* XXX: Why use two separate rules, instead of ctp = cptype; ... ? *)
	[ tp = SELF;  "*"; ctps = LIST1 cptype SEP "*"-> TupleTp (Type tp::ctps) 
	| e = hlexpr; "*"; ctps = LIST1 cptype SEP "*"-> TupleTp (Exp e::ctps) 
	| e = hlexpr -> TupleTp([Exp e])]
    | "ptype1"
	[ tp = SELF; arg = ptype_arg -> ValAppTp (tp,arg)
	| tp = SELF; "Parray"; "["; args = LIST0 parray_args SEP ","; "]" -> ArrayTp (tp,args)
	| "("; tp = SELF; ")" -> tp
	| "{"; fields = LIST1 pfield SEP ";"; "}" -> RecordTp fields
	| "["; id = LIDENT; ":"; tp = ptype; "|"; e = expr; "]" ->
            WhereTp (Id.makeid id, tp, hoc_e e)
	| arg_tp = SELF; id = LIDENT -> TpAppTp ([arg_tp],make_type_id id)
	| "("; arg_tp1 = SELF; ","; arg_tps = LIST1 SELF SEP ","; ")"; id = LIDENT -> TpAppTp (arg_tp1::arg_tps,make_type_id id)
        | id = LIDENT -> TidTp (make_type_id id)] ]
  ;

  (*   ptydef: *)
(*     [[ "("; tp = SELF; ")" -> tp *)
(*      | arg_tp = SELF; id = LIDENT -> TpAppTp ([arg_tp],make_type_id id) *)
(*      | "("; arg_tp1 = SELF; ","; arg_tps = LIST1 SELF SEP ","; ")"; id = LIDENT -> TpAppTp (arg_tp1::arg_tps,make_type_id id) *)
(*      | id = LIDENT -> TidTp (make_type_id id)] ] *)
(*   ; *)

  parray_args:
    [[ "Psep"; e = hlexpr -> ArrSep e
     | "Pterm"; t = parray_term -> ArrTerm t 
     | "Ppred"; e = hlexpr -> ArrPred e ]]
  ;

  parray_term:
    [[ "Pnosep" -> ArrNosep
     | e = hlexpr -> ArrTermExp e]]
  ;	 

  ptype_vparam:
    [[ "("; id = LIDENT; ":"; t = ctyp; ")" -> (Id.makeid id, HostLanguage.tp_of_ctyp t)]]
  ; 
  
  ptype_tparam:
    [[ -> [] (*empty*) 
     | "("; ids = LIST1 LIDENT SEP ","; ")" -> List.map make_type_id ids] ]
    ;

  ptype_arg:
    [[ "("; e = expr; ")" -> hoc_e e]]
  ; 
  
(*   ptype_decl :  *)
(*     [[ param = OPT ptype_vparam; "="; tp = ptype -> DefDecl (param, TpDef tp) ]] *)
(*   ; *)

  pvariant:
    [ [ id = UIDENT; "of"; v_tp = ptype -> 
	  (match v_tp with
	      TupleTp([Exp e]) -> AbsorbVar(Id.makeid id, Exp e)
	    | _              -> FullVar(Id.makeid id, v_tp))
      | id = UIDENT; "of"; "omit"; v_tp = ptype -> AbsorbVar(Id.makeid id, Type v_tp) ]]
  ;

  pdefault_var:
    [[ "with"; "pdefault"; id_opt = OPT UIDENT; "of"; d_tp = ptype; "="; e = expr -> 
	 let io = match id_opt with None -> None | Some id -> Some(Id.makeid id) in
	   GenDefault(io, d_tp,hoc_e e)
     | "with"; "pdefault"; id_opt = OPT UIDENT; "of"; d_tp = ptype -> 
	 let io = match id_opt with None -> None | Some id -> Some(Id.makeid id) in
	   FullDefault (io, d_tp)]]
  ;

  pcase:
    [ [ p = patt; "->"; id = UIDENT; "of"; v_tp = ptype -> 
	  (match v_tp with
	       TupleTp([Exp e]) -> AbsorbCase(hoc_p p, Id.makeid id, Exp e)
	     | _              -> FullCase(hoc_p p, Id.makeid id, v_tp))
    (* XXX: decide on "absorb" syntax. "of omit" is place holder. *)
      | p = patt; "->"; id = UIDENT; "of"; "omit"; o_tp = ptype -> AbsorbCase(hoc_p p, Id.makeid id, Type o_tp)
      | p = patt; "->"; id = UIDENT; "of"; c_tp = ptype; "="; e = expr -> GenCase(hoc_p p, Id.makeid id,c_tp,hoc_e e)] ]
  ;

  ptype_decl : 
    [[ param = OPT ptype_vparam; "="; tp = ptype -> DefDecl (param, TpDef tp)
     | param = OPT ptype_vparam; "="; OPT "|"; vdl = LIST1 pvariant SEP "|"; def_opt = OPT pdefault_var ->
	 DefDecl (param, DtDef (ImplicitDT (vdl,def_opt))) 
     | param = OPT ptype_vparam; "="; "pmatch"; e = expr; "with"; OPT "|"; cl = LIST1 pcase SEP "|" ->
	 DefDecl (param, DtDef(CaseDT (hoc_e e, cl)))]]
  ;

(*   pdatatype_decl :  *)
(*     [[ param = OPT ptype_vparam; "="; OPT "|"; vdl = LIST1 pvariant SEP "|"; def_opt = OPT pdefault_var -> *)
(* 	 DefDecl (param, DtDef (ImplicitDT (vdl,def_opt)))  *)
(*      | param = OPT ptype_vparam; "="; "Pmatch"; e = expr; "with"; OPT "|"; cl = LIST1 pcase SEP "|" -> *)
(* 	 DefDecl (param, DtDef(CaseDT (hoc_e e, cl)))]] *)
(*   ; *)

(*   pdatatype_decl :  *)
(*     [[ param = OPT ptype_vparam; "="; def = pdefault_var ->  *)
(* 	 DefDecl (param, DtDef (ImplicitDT ([],Some def)))  *)
(*      | param = OPT ptype_vparam; "="; OPT "|"; vdl = LIST1 pvariant SEP "|" -> *)
(* 	 DefDecl (param, DtDef (ImplicitDT (vdl,None)))  *)
(*      | param = OPT ptype_vparam; "="; OPT "|"; vdl = LIST1 pvariant SEP "|"; def = pdefault_var -> *)
(* 	 DefDecl (param, DtDef (ImplicitDT (vdl,Some def)))  *)
(*      | param = OPT ptype_vparam; "="; "Pmatch"; e = expr; "with"; OPT "|"; cl = LIST1 pcase SEP "|" -> *)
(* 	 DefDecl (param, DtDef(CaseDT (hoc_e e, cl)))]] *)
(*   ; *)

  ptype_binding :
    [[ tparams = ptype_tparam; id = LIDENT; decl_body = ptype_decl -> (tparams, make_type_id id, decl_body)]]
  ;
  
  str_item : 
    [ [ "ptype"; tp_decls = LIST1 ptype_binding SEP "and" ->
	  let _ = Common.global_loc := loc in
	  let (si,dt) = match tp_decls with 
	      [decl] -> Compiler.process_decl !descr_table !val_ctxt loc decl
	    | tp_decls -> Compiler.process_rec_decls !descr_table !val_ctxt loc tp_decls 
	  in
	  let _ = descr_table := dt in si
      (* need distinct "datatype" keyword becuase of LL parsing. *)
(*       | "pdatatype"; tparams = ptype_tparam; id = LIDENT; decl_body = pdatatype_decl -> *)
(* 	  let dt_decl = (tparams, make_type_id id, decl_body) in *)
(* 	  let _ = Common.global_loc := loc in *)
(* 	  let (si,dt) = Compiler.process_decl !descr_table !val_ctxt loc dt_decl in *)
(* 	  let _ = descr_table := dt in si *)
      ]] 
  
  ;
END;;
