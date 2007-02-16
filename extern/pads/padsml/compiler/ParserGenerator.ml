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

(** create expression for parsing a type. 
    @args: parse_fn, is_first, pd_hdr_var name *)
let record_parse parser_fn is_first hdr_var = 
  if is_first then
    <:expr<$C.pads_record_mod$.$lid:N.record_parse_first$ $parser_fn$>>
  else
    <:expr<$C.pads_record_mod$.$lid:N.record_parse_next$ $parser_fn$ $lid:hdr_var$>>

(** create expression for absorbing a type. 
    @args: parse_fn, is_first, pd_hdr_var name *)
let record_absorb parser_fn is_first hdr_var = 
  if is_first then
    <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first$ $parser_fn$>>
  else
    <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next$ $parser_fn$ $lid:hdr_var$>>

(** create expression for absorbing a literal as a record field.
    @args: literal expression, is_first, pd_hdr variable name *)
let record_absorb_lit lit_expr is_first hdr_var =
  let e = HL.ocaml_of_host_e lit_expr in
    if is_first then
      match HL.infer_lit_tp lit_expr with
	  HL.Char_lit   -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_litc$ $e$>>
	| HL.String_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_lits$ $e$>>
	| HL.Int_lit    -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_liti$ $e$>>
	| HL.RegExp_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_litres$ $e$>>
    else
      let h = <:expr<$lid:hdr_var$>> in
	match HL.infer_lit_tp lit_expr with
	    HL.Char_lit   -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_litc$ $e$ $h$>>
	  | HL.String_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_lits$ $e$ $h$>>
	  | HL.Int_lit    -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_liti$ $e$ $h$>>
	  | HL.RegExp_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_litres$ $e$ $h$>>

(** create expression for absorbing a literal as a datatype variant.
    @args: literal expression *)
let datatype_absorb_lit lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litc$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_lits$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_liti$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litres$ $e$>>

(** create expression for absorbing a literal as a datatype case.
    @args: literal expression *)
let datatype_absorb_lit_case lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litc_case$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_lits_case$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_liti_case$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litres_case$ $e$>>

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>

let rec gen_record_parser dt tc loc fields is_tuple = 
  (** list of field names in record. *)
  let names = 
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      fields
  in

  (* Assemble pattern-expression pairs used to build let expressions below. *)
  let patt_expr_list = 
    let hdr_name = N.pd_hdr_var in
      C.munge_fields_state

	(* Absorb field *)
	(fun ctp n -> 
	   let e = match ctp with 
	       Ast.Type tp -> record_absorb (gen_parser dt tc loc tp) (n=1) hdr_name
	     | Ast.Exp exp -> record_absorb_lit exp (n=1) hdr_name
	   in
	     [(<:patt<$lid:hdr_name$>>, 
	       <:expr<$e$ $C.pads_e$>>)],
	     n+1)

	(* Normal field *)
	(fun (id,tp) n ->  
	   let f = n = 1 in
	     [(<:patt<($C.id_p id$,$p_p id$,$lid:hdr_name$)>>, 
	       <:expr<$record_parse (gen_parser dt tc loc tp) f hdr_name$ $C.pads_e$>>)],
	     n+1)
	
	(* Compute field *)
	(fun (id,tp,exp) n ->  
	   let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in
	   let e = HostLanguage.ocaml_of_host_e exp in
	     [(<:patt<($C.id_p id$,$p_p id$)>>, 
	       <:expr<$C.pads_mod$.Compute.generate_parser $e$ $genpd_fn$ $C.pads_e$>>)],
	     n)
	1 fields
  in

  (* Make the patt-expr pair for the record rep. Note that the camlp4
     documentation is wrong and a patt-expr list is required, not an
     expr-expr list. 
  *)
  let mk_recrep_e = List.map (fun n -> r_p n, r_e n) in
    
  (* Make the patt-expr pair for the record pd *)
  let mk_recpd_e = List.map (fun n -> p_p n,p_e n) in
    
  (* Make the expression for the tuple rep *)
  let mk_tuprep_e = List.map (fun n -> r_e n) in
    
  (* Make the expression for the tuple pd *)
  let mk_tuppd_e = List.map (fun n -> p_e n) in
    

  (* Make a list of identifier expressions *)
  let mk_elist mk = List.map
    (fun name -> <:expr<$lid:mk name$>>) in

  (* Make a list of identifier patterns *)
  let mk_plist mk = List.map
    (fun name -> <:patt<$lid:mk name$>>) in

  (* Generates a list expression from a function and a list of ids *)
  let mk_list_e mk_e ids = 
    List.fold_right (fun id le -> <:expr<[$mk_e id$::$le$]>>) ids <:expr<[]>>
  in

  let mklets = List.fold_right (fun pe e -> <:expr<let $list:[pe]$ in $e$>>) in
    
    (* The first two cases deal with empty and singleton tupels. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple expressions and patterns. 
       Singleton records appear to be okay.
    *)
    match names with
	[] -> 
	  let final_e = <:expr<	    
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ((), ($lid:N.pd_hdr_var$, ())) >> 
	  in
	    (<:expr<fun $C.pads_p$ -> $mklets patt_expr_list final_e$>>)
      | [name] ->
	  let rep_e = <:expr<$lid:Id.id2string name$>> in
	  let pd_e  = <:expr<$lid:pid name$>> in
	  let hdr_e = <:expr<[$C.get_pd_hdr$ $lid:pid name$]>> in

	  let rep_p = <:patt<$lid:rid name$>> in
	  let pd_p  = <:patt<$lid:pid name$>> in

	  (* The bodies of the make_rep and make_pd functions *)
	  let mr_body = if is_tuple then r_e name
	    (* records can have one field, unlike tuples. *)
          else <:expr<{$list:mk_recrep_e names$}>> in
	  let mp_body = if is_tuple then p_e name
	    (* records can have one field, unlike tuples. *)
          else <:expr<{$list:mk_recpd_e names$}>> in

	  let final_e = <:expr<
	    let rep = $rep_e$ in
	    let pd  = $pd_e$ in
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ($lid:N.make_rep$ rep, $lid:N.make_pd$ $lid:N.pd_hdr_var$ pd)
	      >> 
	  in
	    <:expr< 
	      fun $C.pads_p$ -> 
		let $lid:N.make_rep$ $rep_p$ = $mr_body$ in
		let $lid:N.make_pd$ hdr $pd_p$ = (hdr, $mp_body$) 
		in $mklets patt_expr_list final_e$
		   >>
      | _ ->
	  let reps_e = mk_elist Id.id2string names in
	  let pds_e  = mk_elist pid names in
	  let hdrs_e = mk_list_e (fun n -> <:expr<$C.get_pd_hdr$ $lid:pid n$>>) names in

	  let reps_p = mk_plist rid names in
	  let pds_p  = mk_plist pid names in

	  (* The bodies of the make_rep and make_pd functions *)
	  let mr_body = if is_tuple then <:expr<($list:mk_tuprep_e names$)>>
          else <:expr<{$list:mk_recrep_e names$}>> in
	  let mp_body = if is_tuple then <:expr<($list:mk_tuppd_e names$)>>
          else <:expr<{$list:mk_recpd_e names$}>> in

	  let final_e = <:expr<
	    let reps = ($list:reps_e$) in
	    let pds  = ($list:pds_e$) in
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ($lid:N.make_rep$ reps, $lid:N.make_pd$ $lid:N.pd_hdr_var$ pds)
	      >> 
	  in
	    <:expr< 
	      fun $C.pads_p$ -> 
		let $lid:N.make_rep$ ($list:reps_p$) = $mr_body$ in
		let $lid:N.make_pd$ hdr ($list:pds_p$) = (hdr, $mp_body$) in
		  $mklets patt_expr_list final_e$
		   >>

and gen_parser dt tc loc = function 

    Ast.TupleTp ctps ->
      let fresh_rep = (Id.id2string (Id.freshid "r")) ^ "_" in
      let fresh_pd = (Id.id2string (Id.freshid "p")) ^ "_" in
      let rid i = fresh_rep ^ (string_of_int i) in
      let pid i = fresh_pd ^ (string_of_int i) in

      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      let _,fds = List.fold_left convert (1,[]) ctps in
      let fields = List.rev fds in
	gen_record_parser dt tc loc fields true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tid) ^ " not found.")
	 | Some {name=_;kind=D.Base;tyclass=D.Current} -> 
	     <:expr<$lid:N.parser_fun$>>
	 | Some {name=_;kind=D.Base;tyclass=_} -> C.parser_e tid
	 | _ -> PError.report_error loc "ParserGenerator: Unsupported feature: type identifiers cannot have higher kinds.")

  | Ast.ValAppTp (tp_fun,exp) -> 
      let pfn = gen_parser dt tc loc tp_fun in
      let e   = HostLanguage.ocaml_of_host_e exp in
	<:expr<$pfn$ $e$>>

  | Ast.TpAppTp (tp_args,tp_fun) -> 
      (match D.lookup_descr dt tp_fun with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	 | Some {name=_;kind=D.Fun(params_md);tyclass=D.Current} -> 
	     	     <:expr<$lid:N.parser_fun$>> 
		       (* Call parser function directly, not through
			  recursion via surrounding module. *)
		       
	 | Some {name=_;kind=D.Fun(_);tyclass=_} ->	      
	     C.apply_project tp_fun tp_args Names.parser_fun

	 | _ -> PError.report_error loc "Use does not match kind."
      )
	
  | Ast.WhereTp (id,tp,exp) ->	
      let where_fn = <:expr<$C.pads_where_mod$.$lid:N.where_parse_underlying$>> in
      let parse_fn = gen_parser dt tc loc tp in
      let pred = <:expr<fun $C.id_p id$ -> $HostLanguage.ocaml_of_host_e exp$>> in
	<:expr< fun $C.pads_p$ -> $where_fn$ $parse_fn$ $pred$ $C.pads_e$>>

	  | _ -> PError.report_error loc "unsupported feature"
	      
let gen_tp_parser_body dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? *)
    Ast.RecordTp fields -> 
      gen_record_parser dt tc loc fields false
  | b -> gen_parser dt tc loc b      

     
(* let rec parse pads =  *)
(*   let make_rep r = r in *)
(*   let make_pd h p =  *)
(*     Pads.Datatype.make_pd_hdr h, p in *)
(*   let make_err_pd pads = ... *)
(*   match Pads.Datatype.parse_variant VT1.parse pads with *)
(*       Some (r,p) -> make_rep V1 r, make_pd (Pads.get_pd_hdr p), V1_pd p *)
(*     | None -> *)
(* 	match Pads.Datatype.parse_variant VT2.parse pads with *)
(* 	    Some (r,p) -> make_rep V2 r, make_pd ((Pads.get_pd_hdr p), V2_pd p) *)
(* 	  | None ->  *)
(* (\*  ... *\) *)
(* 	      match Pads.Datatype.parse_variant VTn.parse pads with *)
(* 		  Some (r,p) -> make_rep Vn r, make_pd ((Pads.get_pd_hdr p), Vn_pd p) *)
(* 		| None -> Verr, Pads.Datatype.make_err_pd pads Verr_pd *)

let gen_dtp_parser_body dt tc loc = function
    Ast.ImplicitDT (vs,def_opt) -> 

      (* mkmatch - generate the match expression for a particular variant.
	 var    : the given variant
	 none_e : the expression to call in case this variant fails to parse. *)
      let mkmatch var none_e =
	match var with
	  Ast.FullVar(id,tp) -> 
	    (* dt_fn : the error-handling function from the datatype module.
	       parser_fn: the parsing function for the variant.
	       c_rep : the rep constructor for the variant
	       c_pd : the pd constructor for the variant *)
	    let dt_fn = <:expr<$lid:N.dt_parse_vt$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd = <:expr<$uid:N.mk_pd_str id$>>  in
	    let descr_e = <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $C.pads_e$>> in
	      (* XXX: r and p are not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with 
                  [ Some (r,p) -> ($C.pads_dt_mod$.make_rep ($c_rep$ r),
				   $C.pads_dt_mod$.make_pd ($C.get_pd_hdr$ p, $c_pd$ p))
	          | None -> $none_e$ ] >>
	| Ast.AbsorbVar(id,Ast.Type tp) -> 
	    let dt_fn = <:expr<$lid:N.dt_absorb_vt$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    let descr_e = <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $C.pads_e$>> in
	      (* XXX: sp is not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with
		        [ Some sp -> ($C.pads_dt_mod$.$lid:N.dt_make_rep$ $c_rep$, 
			              $C.pads_dt_mod$.$lid:N.dt_make_absorb_pd$ sp $c_pd$)
	                | None -> $none_e$ ] >>
	| Ast.AbsorbVar(id,Ast.Exp e) -> 
	    (* XXX: Why limited to literals? *)
	    let dt_fn = datatype_absorb_lit e in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    let descr_e = <:expr<$dt_fn$ $C.pads_e$>> in
	      (* XXX: sp is not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with
		        [ Some sp -> ($C.pads_dt_mod$.$lid:N.dt_make_rep$ $c_rep$, 
			              $C.pads_dt_mod$.$lid:N.dt_make_absorb_pd$ sp $c_pd$)
	                | None -> $none_e$ ] >>
      in
      let mkmatches = List.fold_right mkmatch in
      let def_e = match def_opt with
	  (* For both Some branches, use one of the case functions, as the default branch acts
	     like a case rather than a variant. That is because it
	     can contain errors (like cases), while variants must
	     succeed to be chosen. *)		
	  Some (Ast.GenDefault(id_opt,tp,e)) -> 
	    let dt_fn = <:expr<$lid:N.dt_gen_case$>> in
	    let gen_val = HL.ocaml_of_host_e e in
	    let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in	      
	    let (c_rep,c_pd) = 
	      match id_opt with
		  None -> <:expr<fun r -> $uid:N.def_vt$ r>>,
		     <:expr<fun p -> $uid:N.def_pd_vt$ p>>
    	        | Some id -> <:expr<fun r -> $uid:N.mk_rep_str id$ r>>,
		     <:expr<fun p -> $uid:N.mk_pd_str id$ p>>
	    in
	      <:expr<$C.pads_dt_mod$.$dt_fn$ $gen_val$ $genpd_fn$ $c_rep$ $c_pd$ $C.pads_e$>>	    

	| Some (Ast.FullDefault(id_opt,tp)) ->
	    let dt_fn = <:expr<$lid:N.dt_parse_case$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let (c_rep,c_pd) = 
	      match id_opt with
		  None -> <:expr<fun r -> $uid:N.def_vt$ r>>,
		     <:expr<fun p -> $uid:N.def_pd_vt$ p>>
	        | Some id -> <:expr<fun r -> $uid:N.mk_rep_str id$ r>>,
		     <:expr<fun p -> $uid:N.mk_pd_str id$ p>>
	    in
	      <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>	    

	| None -> <:expr< ($uid:N.err_vt$, $C.pads_dt_mod$.handle_error_variant
			     $C.pads_e$ $uid:N.err_pd_vt$) >> 
      in
	<:expr<fun $C.pads_p$ -> $mkmatches vs def_e$ >>
		
        | Ast.CaseDT (descr,cases) -> 
	    (*
	      fun parse pads = 
	      match e with
	      p1 -> Pads.Datatype.parse_case parse1 (fun r -> C1 r) (fun p -> C1_pd p) pads
	      | p2 -> Pads.Datatype.absorb_case parse2 (fun r -> C2 r) (fun p -> C2_pd p) pads
	      | p3 -> Pads.Datatype.absorb_char_case c (fun r -> C3 r) (fun p -> C3_pd p) pads
	      | p4 -> Pads.Datatype.gen_case e (fun r -> C4 r) (fun p -> C4_pd p) pads
	    *)
	    (* none_e : the expression to call in case this variant fails to parse. *)
	    let mkcase = function
		Ast.FullCase(pat,id,tp) -> 
		  (* dt_fn : the error-handling function from the datatype module.
		     parser_fn: the parsing function for the variant.
		     c_rep : the rep constructor for the variant
		     c_pd : the pd constructor for the variant *)
		  let dt_fn = <:expr<$lid:N.dt_parse_case$>> in
		  let parser_fn = gen_parser dt tc loc tp in
		  let c_rep = <:expr<fun r -> $uid:N.mk_rep_str id$ r>> in
		  let c_pd = <:expr<fun p -> $uid:N.mk_pd_str id$ p>> in
		    HL.ocaml_of_host_p pat, None, <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	      | Ast.AbsorbCase(pat,id,Ast.Type tp) -> 
		  let dt_fn = <:expr<$lid:N.dt_absorb_case$>> in
		  let parser_fn = gen_parser dt tc loc tp in
		  let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
		  let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
		    HL.ocaml_of_host_p pat, None, <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	      | Ast.AbsorbCase(pat,id,Ast.Exp e) -> 
		  (* The expression "fn" is fully qualified, that is - it's not relative to the datatype module. 
		     Also, it is already applied to the expression "e"*)
		  let fn = datatype_absorb_lit_case e in
		  let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
		  let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
		    HL.ocaml_of_host_p pat, None, <:expr<$fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	      | Ast.GenCase(pat,id,tp,e) -> 
		  let dt_fn = <:expr<$lid:N.dt_gen_case$>>  in
		  let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in
		  let c_rep = <:expr<fun r -> $uid:N.mk_rep_str id$ r>> in
		  let c_pd  = <:expr<fun p -> $uid:N.mk_pd_str id$ p>> in
		    HL.ocaml_of_host_p pat, None, 
		    <:expr<$C.pads_dt_mod$.$dt_fn$ $HL.ocaml_of_host_e e$ $genpd_fn$ $c_rep$ $c_pd$ $C.pads_e$>> 
	    in
	    let mkcases = List.map mkcase in
	      (* Default error case, in case nothing else matches. *)
	    let def_case = <:patt<_>>,None,
            <:expr< ($uid:N.err_vt$, $C.pads_dt_mod$.make_err_pd $C.pads_e$ $uid:N.err_pd_vt$) >>
	    in
	      <:expr<fun $C.pads_p$ -> match $HL.ocaml_of_host_e descr$ with [$list:(mkcases cases)@[def_case]$]>>
      
let gen dt current_descr tc loc (tp_params,name,Ast.DefDecl (val_param_opt, tp_def)) = 
  (* extend environment with value params. *)
  let tc_ext = C.process_val_lam tc val_param_opt in

(*   (\* package_fun: MLast.expr -> MLast.expr *\) *)
(*   let package_fun fun_body = *)
(*     let id2fun id body = <:expr<fun $lid:N.mk_parser_funvar_str id$ -> $body$>> in *)
(*       List.fold_right id2fun tp_params fun_body *)
(*   in *)

(*   (\* package_funty: MLast.ctyp -> MLast.ctyp *\) *)
(*   let package_funty fun_body_t = *)
(*     let id2funty id body_t =  *)
(*       let arg_repty = <:ctyp<$lid:N.mk_top_level_rep_str id$>> in *)
(*       let arg_pdbty = <:ctyp<$lid:N.mk_top_level_pd_body_str id$>> in *)
(* 	<:ctyp<Pads.parser $arg_repty$ $arg_pdbty$ -> $body_t$>>  *)
(*     in *)
(*       List.fold_right id2funty tp_params fun_body_t *)
(*   in *)

  let body_parser_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_parser_body dt tc_ext loc tp
    | Ast.DtDef dtp_body -> gen_dtp_parser_body dt tc_ext loc dtp_body
  in

  let body_parser_t = <:ctyp<$C.handle_t$ -> ($lid:Names.rep$ * $lid:Names.pd$)>> in

  (* parameterize body with val. param *)
  let (val_t, val_fun) = match val_param_opt with
      None -> (body_parser_t, body_parser_fun)
    | Some (p_id,p_type) -> 
	(<:ctyp<$HostLanguage.ctyp_of_tp p_type$ -> $body_parser_t$>>,
	 let p_name = Id.id2string p_id in
	   <:expr<fun $lid:p_name$ -> $body_parser_fun$>>)
  in

(*   let poly_t   = package_funty val_t in *)
(*   let poly_fun = package_fun   val_fun in *)
    
  let tp_pf_name = Names.parser_fun in
    (* create the final parser function *)
    ([], 
    ([<:sig_item<value $lid:tp_pf_name$ : $val_t$>>],
    [<:str_item<value rec $lid:tp_pf_name$  = $val_fun$ >>]),
    current_descr)
    
