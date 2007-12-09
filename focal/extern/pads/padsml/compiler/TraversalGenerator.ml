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

(* Helper functions *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str
let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>
let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

(* Apply the tp_fun to its args, project out the traversal functor, 
   project from the result the specified value, and apply it to the tool. *)
let proj_val_from_trav_mod tp_fun tp_args proj_val =
  let get_val tp_mod_name = 
    <:expr<$uid:tp_mod_name$.$uid:N.traversal_mod$.$lid:proj_val$ $lid:N.tool_rec$>>
  in    
  let tp_mod = C.ty2mod_app tp_fun tp_args in
    C.bind_local_project tp_mod get_val

(**************** Traversal FUNCTOR and Tool MODULE Version **********
 *****  We might eventually allow a compiler option to enable this 
 *****  compilation method.
let record_init = "init"
let record_start = "start"
let record_project = "project"
let record_process_field = "process_field"
let record_process_last_field = "process_last_field"
let record_pre = <:expr<$uid:N.tool_mod$.Record>>

let dt_init = "init"
let dt_start = "start"
let dt_project = "project"
let dt_process_variant = "process_variant"
let dt_init_empty = "init"
let dt_process_empty = "process"
let dt_pre = <:expr<$uid:N.tool_mod$.Datatype>>
let dt_empty_pre = <:expr<$uid:N.tool_mod$.Datatype.Empty>>

let con_init = "init"
let con_start = "start"
let con_project = "project"
let con_process = "process"
let con_pre = <:expr<$uid:N.tool_mod$.Constraint>>

(* Apply the tp_fun to its args, project out the traversal functor, 
   apply it to the tool and project from the result the specified value. *)
let proj_val_from_trav_func tp_fun tp_args proj_val =
  let get_trav_mod tp_mod_name =
    <:module_expr<$uid:tp_mod_name$.$uid:N.traversal_functor$ $uid:N.tool_mod$>> 
  in    
  let tp_mod = C.ty2mod_app tp_fun tp_args in
    C.bind_local_project tp_mod (fun tp_mod_name -> 
    C.bind_local_project (get_trav_mod tp_mod_name) (fun trav_mod_name ->
      <:expr<$uid:trav_mod_name$.$lid:proj_val$>>))

let init_from_tid tid =
  let mod_exp = 
    <:module_expr<$uid:Id.id2string tid$.$uid:N.traversal_functor$ 
      $uid:N.tool_mod$>> in
  let mod_id = Id.freshid "M" in
  let mod_name = Id.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_exp$ in 
	     $uid:mod_name$.$lid:N.init_fun$>>

let init_from_tpapp tp_fun tp_args = proj_val_from_trav_func tp_fun tp_args N.init_fun

let traverse_from_tid tid =
  let mod_exp = 
    <:module_expr<$uid:Id.id2string tid$.$uid:N.traversal_functor$ 
      $uid:N.tool_mod$>> in
  let mod_id = Id.freshid "M" in
  let mod_name = Id.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_exp$ in 
	     $uid:mod_name$.$lid:N.traversal_fun$>>

let traverse_from_tpapp tp_fun tp_args = proj_val_from_trav_func tp_fun tp_args N.traversal_fun

***********************************************************************)

let record_init = "r_init"
let record_start = "r_start"
let record_project = "r_project"
let record_process_field = "process_field"
let record_process_last_field = "process_last_field"
let record_pre = <:expr<$lid:N.tool_rec$.record_t>>

let dt_init = "dt_init"
let dt_start = "dt_start"
let dt_project = "dt_project"
let dt_process_variant = "process_variant"
let dt_init_empty = "dt_init_empty"
let dt_process_empty = "dt_process_empty"
let dt_pre = <:expr<$lid:N.tool_rec$.datatype_t>>
let dt_empty_pre = dt_pre

let con_init = "c_init"
let con_start = "c_start"
let con_project = "c_project"
let con_process = "c_process"
let con_pre = <:expr<$lid:N.tool_rec$.constraint_t>>

let init_from_tid tid = <:expr<$uid:Id.id2string tid$.$uid:N.traversal_mod$.$lid:N.init_fun$ $lid:N.tool_rec$>>
let init_from_tpapp tp_fun tp_args = proj_val_from_trav_mod tp_fun tp_args N.init_fun

let traverse_from_tid tid = <:expr<$uid:Id.id2string tid$.$uid:N.traversal_mod$.$lid:N.traversal_fun$ $lid:N.tool_rec$>>
let traverse_from_tpapp tp_fun tp_args = proj_val_from_trav_mod tp_fun tp_args N.traversal_fun

(********************* init gen code ****************************)

let gen_record_init gi fields is_tuple = 

  (* Make the init function given a list of initialized sub-states. *)
  let fun_e fd_states = 
    <:expr<fun () -> $record_pre$.$lid:record_init$ $fd_states$>> in
 
  let process_field fd inits =
    match fd with
      Ast.AbsorbField _ -> inits
    | Ast.FullField(id,tp) | Ast.GenField(id,tp,_) -> 
	let id_string = Id.id2string id in
	let fd_name = <:expr<$str:id_string$>> in
	let init_fn = gi tp in
	  <:expr< [($fd_name$,$init_fn$ ()):: $inits$] >>
  in
  (* Make the list of fields inits. *)
  let fields_inits = List.fold_right process_field fields <:expr< [] >> in
     fun_e fields_inits

let rec gen_init dt tc loc = function 

    Ast.TupleTp ctps ->
      let rid i = "elt"^(string_of_int i) in
      let pid i = (rid i)^"_pd" in

      (* Convert a tuple-element AST node to a record-field AST node. *)
      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
      let fields = List.rev fields_rev in
	gen_record_init (gen_init dt tc loc) fields true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	   None -> PError.report_error loc ("TraversalGenerator: Type " ^ (Id.id2string tid) ^ " not found.")
	 | Some {kind=D.Base;tyclass=D.Current} -> <:expr<$lid:N.init_fun$ $lid:N.tool_rec$>>
	 | Some {kind=D.Base;tyclass=_} -> init_from_tid tid
	 | _ -> PError.report_error loc 
	     "TraversalGenerator: Unsupported feature: type identifiers cannot have higher kinds.")

  | Ast.ValAppTp (tp_fun,_) -> gen_init dt tc loc tp_fun

  | Ast.TpAppTp (tp_args,tp_fun) -> 
      (match D.lookup_descr dt tp_fun with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	 | Some {kind=D.Fun(_);tyclass=D.Current} -> <:expr<$lid:N.init_fun$ $lid:N.tool_rec$>>
	 | Some {kind=D.Fun(_);tyclass=_} -> init_from_tpapp tp_fun tp_args	      
	 | _ -> PError.report_error loc "Use does not match kind."
      )
	
  | Ast.WhereTp (id,tp,_) ->	
      let init_fn = gen_init dt tc loc tp in
	<:expr<fun () -> $con_pre$.$lid:con_init$ ($init_fn$ ())>>

  | _ -> PError.report_error loc "unsupported feature"
	    
let gen_tp_init dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? 
    *)
    Ast.RecordTp fields -> 
      gen_record_init (gen_init dt tc loc) fields false
  | b -> gen_init dt tc loc b      

let gen_dtp_init dt tc loc tp = 
    <:expr<fun () -> $dt_pre$.$lid:dt_init$ () >>
      
(****** Old version that initialized all variants eagerly - i.e. at init time. ******)
(* let gen_dtp_init dt tc loc tp =  *)

(*   (\* Build the init expression for a variant with a subcomponent. *\) *)
(*   let mk_full_expr rep_con sub_tp = *)
(*     let init_fn = gen_init dt tc loc sub_tp in *)
(*       <:expr<($str:rep_con$,$init_fn$ ())>> *)
(*   in *)
    
(*   (\* Build the init expression for a variant with no subcomponent *\) *)
(*   let mk_absorb_expr rep_con = <:expr<($str:rep_con$, $uid:N.tool_mod$.Datatype.Empty.init ())>> *)
(*   in *)
    
(*   let all_cases,def_case = match tp with *)
(*       Ast.ImplicitDT (vs,def_opt) -> *)

(* 	let mkcase = function *)
(* 	    Ast.FullVar(id,tp) -> mk_full_expr (N.mk_rep_str id) tp  *)
(* 	  | Ast.AbsorbVar(id,_) -> mk_absorb_expr (N.mk_rep_str id) *)
(* 	in *)
(* 	let def_case = match def_opt with *)
(* 	    Some (Ast.GenDefault(tp,_)) | Some (Ast.FullDefault(tp)) -> *)
(* 	      mk_full_expr N.def_vt tp *)
(* 	  | None -> mk_absorb_expr N.err_vt *)
(* 	in *)
(* 	  (List.map mkcase vs),def_case *)

(*     | Ast.CaseDT (_,cases) ->  *)

(* 	let mkcase = function *)
(* 	    Ast.FullCase(_,id,tp) | Ast.GenCase(_,id,tp,_) -> *)
(* 	      mk_full_expr (N.mk_rep_str id) tp  *)
(* 	  | Ast.AbsorbCase(_,id,_) ->  *)
(* 	      mk_absorb_expr (N.mk_rep_str id)  *)
(* 	in *)
(* 	  (\* Default error case, in case nothing else matches. *\) *)
(* 	let def_case = mk_absorb_expr N.err_vt in *)
(* 	  (List.map mkcase cases),def_case *)
(*   in *)
(*   let inits_list = List.fold_right (fun c l -> <:expr< [$c$ :: $l$] >>)  *)
(*     all_cases <:expr< [$def_case$] >> *)
(*   in *)
(*     <:expr<fun () -> $dt_pre$.$lid:dt_init$ $inits_list$ >> *)
      
(********************* traversal gen code ****************************)
;;
let make_fresh s = Id.id2string (Id.freshid s)

let gen_record_traversal gt fields is_tuple = 

  (* list of field names in record. *)
  let names = 
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      fields
  in

  (* The rep and pd parameter patterns of the traversal function *)
  let (reps_p, pds_p) = 
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
	    (<:patt<$r_p name$>>, <:patt<$p_p name$>>)
	  else 
	    let recrep_p = r_p name in
	    let recpd_p = p_p name in
	      (<:patt<{$list:[(recrep_p,recrep_p)]$}>>, 
	       <:patt<{$list:[(recpd_p,recpd_p)]$}>>)
      | _ ->
	  if is_tuple then 
	    (<:patt<($list:List.map r_p names$)>>, <:patt<($list:List.map p_p names$)>>)
	  else 
	    let recrep_p = List.map (fun n -> r_p n, r_p n) names in    
	    let recpd_p = List.map (fun n -> p_p n,p_p n) names in
	      (<:patt<{$list:recrep_p$}>>, <:patt<{$list:recpd_p$}>>)
  in

  let state_var = make_fresh "state" in
  let p_state_var = make_fresh "p_state" in

  (* Make the traversal function given a body. *)
  let fun_e body = <:expr<fun $reps_p$ (hdr,$pds_p$) $lid:state_var$ -> $body$>> in
 
  (* Make the start let expression given the remainder of the lets. *)
  let start_e the_rest = 
    <:expr<let $lid:p_state_var$ = $record_pre$.$lid:record_start$ $lid:state_var$ hdr in $the_rest$>> in

  let process_field fd (last,e) =
    match fd with
      Ast.AbsorbField _ -> (last,e)
    | Ast.FullField(id,tp) | Ast.GenField(id,tp,_) -> 
	let id_string = Id.id2string id in
	let s_p = <:patt<$lid:id_string ^ "_s"$>> in
	let s'_p = <:patt<$lid:id_string ^ "_s'"$>> in
	let s_e = <:expr<$lid:id_string ^ "_s"$>> in
	let s'_e = <:expr<$lid:id_string ^ "_s'"$>> in
	let fd_name = <:expr<$str:id_string$>> in
	let traversal_fn = gt tp in
	let process_fn_name = if last then record_process_last_field else record_process_field in
	  (false,
	  <:expr<
            let $s_p$ = $record_pre$.$lid:record_project$ $lid:state_var$ $fd_name$ in	  
	    let $s'_p$ = $traversal_fn$ $r_e id$ $p_e id$ $s_e$ in
	    let $lid:p_state_var$ = $record_pre$.$lid:process_fn_name$ $lid:p_state_var$ $fd_name$ $s'_e$ in
	      $e$>>)
  in
    if names = [] then fun_e <:expr<$lid:state_var$>> else
      (* Make the lets to process the fields, given the final expression. *)
      let (_,process_fields) = List.fold_right process_field fields (true,<:expr<$lid:p_state_var$>>) in
	fun_e (start_e process_fields)

let rec gen_traversal dt tc loc = function 

    Ast.TupleTp ctps ->
      let rid i = "elt"^(string_of_int i) in
      let pid i = (rid i)^"_pd" in

      (* Convert a tuple-element AST node to a record-field AST node. *)
      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
      let fields = List.rev fields_rev in
	gen_record_traversal (gen_traversal dt tc loc) fields true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	   None -> PError.report_error loc ("TraversalGenerator: Type " ^ (Id.id2string tid) ^ " not found.")
	 | Some {name=_;kind=D.Base;tyclass=D.Current} -> <:expr<$lid:N.traversal_fun$ $lid:N.tool_rec$>>
	 | Some {name=_;kind=D.Base;tyclass=_} -> traverse_from_tid tid
	 | _ -> PError.report_error loc 
	     "TraversalGenerator: Unsupported feature: type identifiers cannot have higher kinds.")

  | Ast.ValAppTp (tp_fun,exp) -> gen_traversal dt tc loc tp_fun

  | Ast.TpAppTp (tp_args,tp_fun) -> 
      (match D.lookup_descr dt tp_fun with
	   None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	 | Some {name=_;kind=D.Fun(params_md);tyclass=D.Current} -> <:expr<$lid:N.traversal_fun$ $lid:N.tool_rec$>>
	 | Some {name=_;kind=D.Fun(params_md);tyclass=_} -> traverse_from_tpapp tp_fun tp_args
	 | _ -> PError.report_error loc "Use does not match kind."
      )
	
  | Ast.WhereTp (id,tp,exp) ->	
      let traversal_fn = gen_traversal dt tc loc tp in
	<:expr<fun r (hdr,pd) state ->
	  let p_state = $con_pre$.$lid:con_start$ state hdr in	
	  let s = $con_pre$.$lid:con_project$ state in	  
	  let s' = $traversal_fn$ r pd s in
	    $con_pre$.$lid:con_process$ p_state s'>>

  | _ -> PError.report_error loc "unsupported feature"
	    
let gen_tp_traversal dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? 
    *)
    Ast.RecordTp fields -> 
      gen_record_traversal (gen_traversal dt tc loc) fields false
  | b -> gen_traversal dt tc loc b      

let gen_dtp_traversal dt tc loc tp = 

  (* Build the traversal expression for a variant with a subcomponent. *)
  let mk_full_expr rep_con pd_con sub_tp = 
    let vpatt = <:patt<($uid:rep_con$ r,$uid:pd_con$ pd)>> in
    let vexpr = <:expr<
      let s_opt = $dt_pre$.$lid:dt_project$ state $str:rep_con$ in
      let s = match s_opt with 
	[ Some s -> s 
	| None -> $gen_init dt tc loc sub_tp$ ()]
      in
      let s' = $gen_traversal dt tc loc sub_tp$ r pd s in
	$dt_pre$.$lid:dt_process_variant$ p_state $str:rep_con$ s'
	>>
    in
      (vpatt,None,vexpr)
  in
    
  (* Build the traversal expression for a variant with no subcomponent *)
  let mk_absorb_expr rep_con pd_con = 
    let vpatt = <:patt<($uid:rep_con$,$uid:pd_con$)>> in
    let vexpr = <:expr<
       let s_opt = $dt_pre$.$lid:dt_project$ state $str:rep_con$ in
       let s = match s_opt with
         [  Some s -> s
	 | None -> $dt_empty_pre$.$lid:dt_init_empty$ () ]
       in
       let s' = $dt_empty_pre$.$lid:dt_process_empty$ s in
	 $dt_pre$.$lid:dt_process_variant$ p_state $str:rep_con$ s'
    >>
    in
      (vpatt,None,vexpr)
  in
    
  let all_cases = match tp with
      Ast.ImplicitDT (vs,def_opt) ->

	let mkcase = function
	    Ast.FullVar(id,tp) -> mk_full_expr (N.mk_rep_str id) (N.mk_pd_str id) tp 
	  | Ast.AbsorbVar(id,_) -> mk_absorb_expr (N.mk_rep_str id) (N.mk_pd_str id) 
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

    | Ast.CaseDT (_,cases) -> 

	let mkcase = function
	    Ast.FullCase(_,id,tp) | Ast.GenCase(_,id,tp,_) ->
	      mk_full_expr (N.mk_rep_str id) (N.mk_pd_str id) tp 
	  | Ast.AbsorbCase(_,id,_) -> 
	      mk_absorb_expr (N.mk_rep_str id) (N.mk_pd_str id) 
	in
	  (* Default error case, in case nothing else matches. *)
	let def_case = mk_absorb_expr N.err_vt N.err_pd_vt in
	  (List.map mkcase cases)@[def_case]
  in
  let err_case = <:patt<_>>,None,
		   <:expr<state>>
  in
    <:expr<fun r (hdr,pd_body) state -> 
      let p_state = $dt_pre$.$lid:dt_start$ state hdr in	  	    
        match (r,pd_body) with [$list:all_cases@[err_case]$]>>
      
let gen dt current_descr tc loc (tp_params,name,Ast.DefDecl (_, tp_def)) = 

  (* Generate init function *)
  let init_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_init dt tc loc tp
    | Ast.DtDef dtp_body -> gen_dtp_init dt tc loc dtp_body
  in
 
  (* Generate traversal function *)
  let traversal_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_traversal dt tc loc tp
    | Ast.DtDef dtp_body -> gen_dtp_traversal dt tc loc dtp_body
  in
 
(*   let package_mod m = *)
(*     let id2functor id body =  *)
(*       <:module_expr<functor ($N.mk_traversal_funvar_str id$:Type.Traversal)  *)
(* 	             -> $body$>> in *)
(*       List.fold_right id2functor tp_params m *)
(*   in *)

  (* Generate traversal module signature. *)
  let init_si =
    <:sig_item<value init : Generic_tool.Rec_ver.t 'state 'rps 'dps 'cps 'lps -> unit -> 'state>> in
  let traverse_si =
    <:sig_item<value traverse : Generic_tool.Rec_ver.t 'state 'rps 'dps 'cps 'lps -> rep -> pd -> 'state -> 'state>> in
  let module_sig = <:module_type<
       sig $list:[init_si;traverse_si]$ end>>
  in
  let module_sig = <:sig_item<module $uid:N.traversal_functor$ : $module_sig$>> in

(* Generate module *)
  let init_st = <:str_item<value rec init $lid:N.tool_rec$ = $init_fun$>> in
  let traverse_st = <:str_item<value rec traverse $lid:N.tool_rec$ = $traversal_fun$>> in
  let tr_mod_body = <:module_expr<struct $list:[init_st;traverse_st]$ end >> 
  in
  let tr_mod = 
    <:str_item<module $uid:N.traversal_mod$ = $tr_mod_body$>>
  in
    [], ([module_sig],[<:str_item<open Generic_tool.Rec_ver>>;tr_mod]), current_descr

(**************** Traversal FUNCTOR and Tool MODULE Version **********
 *****  We might eventually allow a compiler option to enable this 
 *****  compilation method.

  (* Generate traversal module signature. *)
  let init_si =
    <:sig_item<value init : unit -> $uid:N.tool_mod$.state>> in
  let traverse_si =
    <:sig_item<value traverse : rep -> pd -> $uid:N.tool_mod$.state -> $uid:N.tool_mod$.state>> in
  let functor_result_sig = <:module_type<
       sig $list:[init_si;traverse_si]$ end>>
  in
  let functor_sig =
    <:module_type<functor ($uid:N.tool_mod$:Generic_tool.S) -> $functor_result_sig$>> in
  let module_sig = <:sig_item<module $uid:N.traversal_functor$ : $functor_sig$>> in

(* Generate functor *)
  let init_st = <:str_item<value rec init = $init_fun$>> in
  let traverse_st = <:str_item<value rec traverse = $traversal_fun$>> in
  let tr_mod_body = <:module_expr<
       functor ($uid:N.tool_mod$:Generic_tool.S) ->
       struct $list:[init_st;traverse_st]$ end
      >>
  in
  let tr_mod =
    <:str_item<module $uid:N.traversal_functor$ = $tr_mod_body$>>
  in
    [], ([module_sig],[tr_mod]), current_descr

*******************************************************)

(* Solution to recursive module problem.  Generate traversal functions
   as module of functions instead of functor.  Then, provide
   injection/projection functions from Tool module to record of functions.
*)
