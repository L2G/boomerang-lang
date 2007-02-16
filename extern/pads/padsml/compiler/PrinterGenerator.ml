module C = Common
module D = Description
module N = Names
module HL = HostLanguage
  
(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = (Lexing.dummy_pos,Lexing.dummy_pos)

(*****************************)

(********************)
(* Helper functions *)
(********************)
let abs_type_warning phase_name = 
  PError.report_warning loc 
    (phase_name ^ 
       ": Unsupported feature: absorbed types cannot be printed.")

let gen_dt_full f rep_c pd_c =
  let rep_var = Id.id2string (Id.freshid "r") in
  let pd_var = Id.id2string (Id.freshid "pd") in
    (<:patt<($uid:rep_c$ $lid:rep_var$, (_,$uid:pd_c$ $lid:pd_var$))>>, None,
     <:expr<$f$ $lid:rep_var$ $lid:pd_var$ $C.pads_e$>>)

let print_lit lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_mod$.$lid:N.print_lit_char$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_mod$.$lid:N.print_lit_string$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_mod$.$lid:N.print_lit_int$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_mod$.$lid:N.print_lit_regexp_str$ $e$>>

let gen_dt_abs_lit e rep_c pd_c =
  (<:patt<($uid:rep_c$, (_,$uid:pd_c$))>>, None,
   <:expr<$print_lit e$ $C.pads_e$>>)

let gen_dt_abs_type f tp rep_c pd_c =
  (<:patt<($uid:rep_c$, (_,$uid:pd_c$))>>, None,
   (* Nothing to print. *)
   <:expr<()>>)

let gen_dt_err rep_c pd_c = 
  (<:patt<($uid:rep_c$, (_,$uid:pd_c$))>>, None,
   (* Nothing to print, as none of the branches succeeded. *)
   <:expr<()>>) 

(* Computed variant/case *)
let gen_dt_comp rep_c pd_c = 
  (<:patt<($uid:rep_c$ _, (_,$uid:pd_c$ _))>>, None,
   (* Nothing to print. *)
   <:expr<()>>) 

(********************)

let f_name = Names.printer_fun
let phase_name = "PrinterGenerator"

let gen_where_f sub_f id exp =
  let r_s = N.mk_rep_str id in
  let pd_s = N.mk_pd_str id in
  <:expr<fun $lid:r_s$ (_,$lid:pd_s$) $C.pads_p$ ->
	   $sub_f$ $lid:r_s$ $lid:pd_s$ $C.pads_e$>>

let gen_record_abs_type f tp n =
  abs_type_warning phase_name; []

let gen_record_abs_lit exp n = 
   [<:expr<$print_lit exp$ $C.pads_e$>>]

let gen_record_full id f n =
  let r_s = N.mk_rep_str id in
  let pd_s = N.mk_pd_str id in
    [<:expr<$f$ $lid:r_s$ $lid:pd_s$ $C.pads_e$>>]

(* Computed field: nothing to print. *)
let gen_record_comp id tp exp n = []

let gen_record_compose is_tuple names processed_fields =
  let print_es = <:expr<do {$list:processed_fields$}>> in
  let r_p,pd_p =
    (* The first two cases deal with empty and singleton tupels. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple patterns.  
       Singleton records appear to be okay.
    *)
    match names with
	[] -> <:patt<()>>, <:patt<(_,())>>
      | [name] ->
	  if is_tuple then
	    <:patt<$lid:N.mk_rep_str name$>>, <:patt<(_,$lid:N.mk_pd_str name$)>>	  
	  else
	    let r_p = <:patt<$lid:N.mk_rep_str name$>> in 
	    let pd_p = <:patt<$lid:N.mk_pd_str name$>> in
	      <:patt<{$list:[(r_p,r_p)]$}>>, <:patt<(_,{$list:[(pd_p,pd_p)]$})>>	  
      | _ ->
	  if is_tuple then
	    let r_ps = List.map (fun n -> <:patt<$lid:N.mk_rep_str n$>>) names
	    in
	    let pd_ps = List.map (fun n -> <:patt<$lid:N.mk_pd_str n$>>) names
	    in
	      <:patt<($list:r_ps$)>>, <:patt<(_,($list:pd_ps$))>>	  
	  else
	    let r_ps = List.map (fun n -> 
				   let p = <:patt<$lid:N.mk_rep_str n$>> in 
				     p,p) names
	    in
	    let pd_ps = List.map (fun n -> 
				    let p = <:patt<$lid:N.mk_pd_str n$>> in 
				      p,p) names
	    in
	      <:patt<{$list:r_ps$}>>, <:patt<(_,{$list:pd_ps$})>>	  
  in
    <:expr<fun $r_p$ $pd_p$  $C.pads_p$ -> $print_es$>>
      
let gen_dt_full_var vt_f rep_c pd_c cases =
  (gen_dt_full vt_f rep_c pd_c):: cases

let gen_dt_abs_type_var vt_f tp rep_c pd_c cases =
  abs_type_warning phase_name; 
  (gen_dt_abs_type vt_f tp rep_c pd_c) :: cases

(*   let rep_e = GetDefaultGenerator.gen_f in *)
(*   let pd_e = Id.id2string (Id.freshid "pd") in *)
(*     (<:patt<$rep_c$>>, None, *)
(*     <:expr<fun (_,$pd_c$) $C.pads_p$ ->  *)
(*       $vt_f$ $str:rep_var$ $str:pd_var$ $C.pads_e$>>)  *)
(*     :: cases *)

let gen_dt_abs_lit_var e rep_c pd_c cases =
  (gen_dt_abs_lit e rep_c pd_c)::cases

(* computed default *)
let gen_dt_comp_def tp e rep_c pd_c = 
  [gen_dt_comp rep_c pd_c]

(* full default *)
let gen_dt_full_def f rep_c pd_c = [gen_dt_full f rep_c pd_c]

(* no default *)
let gen_dt_no_def rep_c pd_c = [gen_dt_err rep_c pd_c]

(* compose the generated code for each variant into a function. *)
let gen_dt_compose cases = 
  let all_cases = cases @ [<:patt<_>>,None,
		   <:expr<Pads.Log.report_error "print" None Pads.No_info 
		             "Invalid data passed to print" $C.pads_e$>>]
  in
    <:expr<fun rep pd $C.pads_p$-> match (rep,pd) with [$list:all_cases$]>>

let gen_dt_full_case pat case_f rep_c pd_c = gen_dt_full case_f rep_c pd_c

let gen_dt_abs_type_case pat case_f tp rep_c pd_c = 
  abs_type_warning phase_name; 
  gen_dt_abs_type case_f tp rep_c pd_c
    
let gen_dt_abs_lit_case pat e rep_c pd_c = gen_dt_abs_lit e rep_c pd_c

let gen_dt_comp_case pat tp e rep_c pd_c = 
  gen_dt_comp rep_c pd_c

let gen_dt_def_case rep_c pd_c = gen_dt_err rep_c pd_c

let gen_dt_match_compose descr cases = gen_dt_compose cases

let f_body_t = <:ctyp<$lid:Names.rep$ -> $lid:Names.pd$ -> $C.handle_t$ -> unit>>

(*********************************)

(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : Id.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>


let rec gen_record_f dt tc loc fields is_tuple = 
  (** list of field names in record. *)
  let names = 
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      fields
  in

  let processed_fields = 
    C.munge_fields_state
      
      (* Absorb field *)
      (fun ctp n -> 
	 (match ctp with 
	     Ast.Type tp -> gen_record_abs_type (gen_f dt tc loc tp) tp n
	   | Ast.Exp exp -> gen_record_abs_lit exp n),
	 n+1)
      
      (* Normal field *)
      (fun (id,tp) n ->  
	 gen_record_full id (gen_f dt tc loc tp) n,
	 n+1)
      
      (* Compute field *)
      (fun (id,tp,exp) n -> gen_record_comp id tp exp n,n)
      
      1 fields
  in
    gen_record_compose is_tuple names processed_fields

and gen_f dt tc loc = function 

    Ast.TupleTp ctps ->
      let fresh_rep = (Id.id2string (Id.freshid "r")) ^ "_" in
      let fresh_pd = (Id.id2string (Id.freshid "p")) ^ "_" in
      let rid i = fresh_rep ^ (string_of_int i) in
      let pid i = fresh_pd ^ (string_of_int i) in

      (* Convert tuple elements into record fields *)
      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      (* Use fold left so that fields will be numbered from 1...n,
	 rather than n...1 *)
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
	gen_record_f dt tc loc (List.rev fields_rev) true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tid) ^ " not found.")
	 | Some {name=_;kind=D.Base;tyclass=D.Current} -> 
	     <:expr<$lid:f_name$>>
	 | Some {name=_;kind=D.Base;tyclass=_} ->
	     <:expr<$uid:Id.id2string tid$.$lid:f_name$>>
	 | _ -> PError.report_error loc (phase_name ^ ": Unsupported feature: type identifiers cannot have higher kinds."))

  | Ast.ValAppTp (tp_fun,exp) -> 
      let f = gen_f dt tc loc tp_fun in
      let e   = HostLanguage.ocaml_of_host_e exp in
	<:expr<$f$ $e$>>

  | Ast.TpAppTp (tp_args,tp_fun) -> 
      (match D.lookup_descr dt tp_fun with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	 | Some {name=_;kind=D.Fun(params_md);tyclass=D.Current} -> 
	     	     <:expr<$lid:f_name$>> 
		       (* Call parser function directly, not through
			  recursion via surrounding module. *)
		       
	 | Some {name=_;kind=D.Fun(_);tyclass=_} ->	      
	     C.apply_project tp_fun tp_args f_name

	 | _ -> PError.report_error loc "Use does not match kind."
      )
	
  | Ast.WhereTp (id,tp,exp) ->	
      let sub_f = gen_f dt tc loc tp in
	gen_where_f sub_f id exp

  | _ -> PError.report_error loc "unsupported feature"
	      
let gen_tp_f_body dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? *)
    Ast.RecordTp fields -> 
      gen_record_f dt tc loc fields false
  | b -> gen_f dt tc loc b      
    
(**
   returns: strings rep_c,pd_c
   rep_c : the rep data constructor 
   pd_c : the pd data constructor *)
let mk_cons id = 
  N.mk_rep_c_str id, N.mk_pd_c_str id

let gen_dtp_f_body dt tc loc = function
    Ast.ImplicitDT (vs,def_opt) -> 

      (* mkmatch - generate the match expression for a particular variant.
	 var    : the given variant
	 acc : the accumulator for the fold. *)
      let mk_case var acc =
	match var with
	  Ast.FullVar(id,tp) -> 
	    (* vt_f : the function for the variant. *)
	    let vt_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_full_var vt_f rep_c pd_c acc

	| Ast.AbsorbVar(id,Ast.Type tp) -> 
	    (* vt_f : the function for the variant. *)
	    let vt_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_type_var vt_f tp rep_c pd_c acc

	| Ast.AbsorbVar(id,Ast.Exp e) -> 
	    let rep_c, pd_c = mk_cons id in
	      (* XXX: Why limited to literals? *)
	      gen_dt_abs_lit_var e rep_c pd_c acc
      in

	(* Make the default constructors from the optional id. *)
      let mk_def_cons id_opt =
	(* Choose the right string for the constructor names. *)
	match id_opt with
	      None -> N.def_vt, N.def_pd_vt
	    | Some id -> N.mk_rep_c_str id, N.mk_pd_c_str id
      in

      let def_vt = match def_opt with
	  Some (Ast.GenDefault(id_opt,tp,e)) -> 
	    let (rep_c,pd_c) = mk_def_cons id_opt in
	      gen_dt_comp_def tp e rep_c pd_c

	| Some (Ast.FullDefault(id_opt,tp)) ->
	    let def_f = gen_f dt tc loc tp in
	    let (rep_c, pd_c) = mk_def_cons id_opt in
	      gen_dt_full_def def_f rep_c pd_c

	| None -> 
	    let rep_c = N.err_vt in
	    let pd_c =  N.err_pd_vt in
	      gen_dt_no_def rep_c pd_c
      in

      let cases = List.fold_right mk_case vs def_vt in
	gen_dt_compose cases
	  
  | Ast.CaseDT (descr,cases) -> 
      let mk_case = function
	  Ast.FullCase(pat,id,tp) -> 
	    (* case_f: the function for the case. *)
	    let case_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_full_case pat case_f rep_c pd_c

	| Ast.AbsorbCase(pat,id,Ast.Type tp) -> 
	    (* case_f: the function for the case. *)
	    let case_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_type_case pat case_f tp rep_c pd_c

	| Ast.AbsorbCase(pat,id,Ast.Exp e) -> 
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_lit_case pat e rep_c pd_c

	| Ast.GenCase(pat,id,tp,e) -> 
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_comp_case pat tp e rep_c pd_c
 
     in
	(* Default error case, in case nothing else matches. *)
      let def_case = 
	let rep_c = N.err_vt in
	let pd_c = N.err_pd_vt in 
	  gen_dt_def_case rep_c pd_c
      in
	(* cs is the list of processed cases. *)
      let cs = (List.map mk_case cases) @ [def_case] in
	gen_dt_match_compose descr cs
      
let gen dt current_descr tc loc (val_param_opt, tp_def) = 

  (* extend environment with value params. *)
  let tc_ext = C.process_val_lam tc val_param_opt in

  let f_body =  match tp_def with
      Ast.TpDef tp -> gen_tp_f_body dt tc_ext loc tp
    | Ast.DtDef dtp_body -> gen_dtp_f_body dt tc_ext loc dtp_body
  in

  (* parameterize body with val. param *)
  let (val_t, val_fun) = match val_param_opt with
      None -> (f_body_t, f_body)
    | Some (p_id,p_type) -> 
	(<:ctyp<$HostLanguage.ctyp_of_tp p_type$ -> $f_body_t$>>,
	 let p_name = Id.id2string p_id in
	   <:expr<fun $lid:p_name$ -> $f_body$>>)
  in
    (* create the final parser function *)
    ([<:sig_item<value $lid:f_name$ : $val_t$>>],
     [<:str_item<value rec $lid:f_name$  = $val_fun$ >>],
     current_descr)
