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
(** Compiler module. Entry into compilation. *)

module C = Common
module D = Description

type metadata = D.metadata = {
  name : Id.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass: D.tyclass; (** Is this description the one currently being compiled? *)
}


let core_phases : C.core_phase_v2 list = 
  [C.cp_upgrade_1to2 TypeChecker.gen;
   TypeGenerator.gen;
   C.cp_upgrade_1to2 GenPDGenerator.gen;
   ParserGenerator.gen;
   C.cp_upgrade_1to2 PrinterGenerator.gen;
   TraversalGenerator.gen;
   UntraversalGenerator.gen 
  ]
	
(** descriptions is a description table.
    vc is a value context.
    decl is a PadsML declaration.
    loc is the location of the declaration in the source file
*)
(* let gen_code_decl descriptions tc loc (tp_params,name,decl_body) =  *)
let process_decl descriptions tc loc decl = 
  let (tp_params,name,decl_body) = decl in
  let str_name =  Id.id2string name in 

  let param2descr pname = {name=pname;kind=D.Base;tyclass=D.Ty_var} in
  let params_metadata = List.map param2descr tp_params in

  let current_kind = match tp_params with
      [] -> D.Base 
    | _  -> D.Fun (params_metadata) 
  in
  let current_md = {name=name;kind=current_kind;tyclass=D.Current} in

  (* Extend the description table with the type parameters' metadata. *)
  let add_param_md dtable pmd = D.add_descr dtable pmd in
  let descriptions_params = List.fold_left add_param_md descriptions params_metadata in
  
  (* Build table of descriptions extended with type parameter descriptions
     and the name of the current declaration for recursive reference. The
     latter is later replaced by the a (potentially) updated version
     after processing by the phases. 
  *) 
  let descriptions_ext = D.add_descr descriptions_params current_md in

  (* TEMPORARY: would want to add code here to check whether decl is
     really just an abbreviation for another type or application of
     other types. *)
  (* tp_is_abbrev: Ast.tp -> boolean *)
  let tp_is_abbrev = function
      Ast.TidTp _ -> true
    | Ast.TpAppTp _ -> true
    | _ -> false
  in

  (* def_is_abbrev: Ast.tp_def -> boolean *)
  let def_is_abbrev = function
      (None,Ast.TpDef tp) -> tp_is_abbrev tp
    | _ -> false
  in

  (* This function should only be called on type abbreviations. 
     XXX: does not do any well-formedness checking.*)
  let abbrev2mod = function
      Ast.TidTp id -> C.ty2mod id
    | Ast.TpAppTp (tps, id) -> C.ty2mod_app id tps
    | _ -> raise C.Internal_error 
  in
    
  (* Process type parameters *)
  let package_mod mod_si = 
    match tp_params with
	[] -> mod_si
      | _ ->
	  let internal_mod = <:module_expr<struct $mod_si$; include $uid:str_name$; end>> in
	  let id2functor id body = <:module_expr<functor ($Id.id2string id$:Type.S) -> $body$>> in
	  let fun_mod = List.fold_right id2functor tp_params internal_mod in
	    <:str_item<module $str_name$ = $fun_mod$>>
  in	      
  let Ast.DefDecl def = decl_body in
    if def_is_abbrev def then
      let descriptions_new = D.add_descr descriptions {name=name;kind=current_kind;tyclass=D.Defined} in
	(* Must be a TpDef, as checked already by def_is_abbrev. *)
      let (_,Ast.TpDef tp) = def in
      let mod_si = <:str_item<module $str_name$ = $abbrev2mod tp$>> in
	(package_mod mod_si, descriptions_new)
    else
      (* current_descr is the description being generated based
	 on the decl being processed. *)
      let run_phase (tl_str_items,(sig_items,str_items),current_descr) phase = 
	let (tl_strs,(sigs,strs),current_descr') = phase descriptions_ext current_descr tc loc decl in
	  (tl_strs::tl_str_items,(sigs::sig_items,strs::str_items),current_descr') 
      in
	(* current_description = description metadata after processing by phases. *)
      let (tl_strs,(sigs,strs), current_description) = List.fold_left run_phase ([],([],[]),current_md) core_phases in
	
      let si_sig = <:module_type<sig $list:Common.rev_flatten sigs$ end>> in
      let si_str = <:module_expr<struct $list:Common.rev_flatten strs$ end>> in
	
      (* XXX: work-around of camlp4 quotation bug. The quotation should be:
	 <:str_item<module rec $str_name$ : sig $list:sig_items$ end = 
	 struct $list:str_items$ end>>    *)
      let rec_si = MLast.StRecMod (loc, [(str_name, si_sig, si_str)]) in
(*       let rec_si = <:str_item<module $str_name$ : $si_sig$ = $si_str$>> in *)
	
      (* Augment the base descriptions with the generated
	 description. Do not include the parameter descriptions as they
	 are specific to the particular descriptions being processed. 
	 Also, must modify the metadata so that it is not flagged as the 
	 current description. 
      *)
      let descriptions_new = D.add_descr descriptions {current_description with tyclass=D.Defined} in
	(<:str_item<declare $list:(C.rev_flatten([package_mod rec_si]::tl_strs))$ end>>, descriptions_new)

(* let process_decl descriptions tc loc decl =  *)
(*   let generated = gen_code_decl descriptions tc loc decl in *)
      

let gen_code_rec_decl descriptions tc loc decl = 
  let (_,name,decl_body) = decl in
  let str_name =  Id.id2string name in 

  let current_md = {name=name;kind=D.Base;tyclass=D.Current} in

  let Ast.DefDecl def = decl_body in
    (* current_descr is the description being generated based
       on the declaration being processed. *)
  let run_phase (tl_str_items,(sig_items,str_items),current_descr) phase = 
    let (tl_strs,(sigs,strs),current_descr') = phase descriptions current_descr tc loc decl in
      (tl_strs::tl_str_items,(sigs::sig_items,strs::str_items),current_descr') 
  in
    (* current_description = description metadata after processing by phases. *)
  let (tl_strs,(sigs,strs), current_description) = List.fold_left run_phase ([],([],[]),current_md) core_phases in
    
  let si_sig = <:module_type<sig $list:Common.rev_flatten sigs$ end>> in
  let si_str = <:module_expr<struct $list:Common.rev_flatten strs$ end>> in
    (Common.rev_flatten tl_strs),(str_name, si_sig, si_str),current_description

let process_rec_decls descriptions tc loc rdecls = 
  (* Pre-process decls to add to description table.*)
  let rdecl2descr (tp_params,rd_name,_) = 
    match tp_params with
	[] -> {name=rd_name;kind=D.Base;tyclass=D.Current}
      | _ -> PError.report_error loc ("Recursive declaration " ^ (Id.id2string rd_name) ^ "has type parameters, but type parameters are not supported for recursive declarations.")       
  in
  let rdecls_metadata = List.map rdecl2descr rdecls in

  (* Extend the description table with the rec. decls' metadata. *)
  let descriptions_ext = List.fold_left D.add_descr descriptions rdecls_metadata in
  
  (* Process each decl in sequence. *)
  let gc_decl decl = gen_code_rec_decl descriptions_ext tc loc decl in
  let (tl_strs, gen_mods, rdecl_ds) = C.split3 (List.map gc_decl rdecls) in
    (* XXX: work-around of camlp4 quotation bug. The quotation should be:
       <:str_item<module rec $str_name$ : sig $list:sig_items$ end = 
       struct $list:str_items$ end>>    *)
  let rec_si = MLast.StRecMod (loc, gen_mods) in
    
  let tl_str_items = List.flatten(tl_strs@[[rec_si]]) in

  (* Augment the base descriptions with the generated
     descriptions.
  *)
  let descriptions_new = List.fold_left D.add_descr descriptions rdecl_ds in
    (<:str_item<declare $list:tl_str_items$ end>>, descriptions_new)

(* 
description name is irrelevant to processing. descr. info should be bound to name after
procssing by phases. Type variables and simultaneous recursion should each be handled.

B,P |- d ->  ?


1. Should phases operate on top-level descsriptions are just the type bodies?
I think that the latter approach is correct. Then, phases should carry metadata for a description
that they build up. After all phases are run, the metadata is bound to the name of the description.
We could change the sig for phases to:
type core_phase = 

   D.table 
   -> HostLanguage.tp_ctxt
   -> MLast.loc
   -> Ast.decl 
   -> metadata
   -> (MLast.sig_item list * MLast.str_item list * metadata)
That is, they receive the "current" metadata and can functionally update it.
*)
