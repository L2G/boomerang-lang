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

(* dummy location *)
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
    <:expr<$uid:tp_mod_name$.$uid:N.untraversal_mod$.$lid:proj_val$ $lid:N.untool_rec$>>
  in    
  let tp_mod = C.ty2mod_app tp_fun tp_args in
    C.bind_local_project tp_mod get_val

let record_pre = <:expr<$lid:N.untool_rec$.processRecord>>
let tuple_pre = <:expr<$lid:N.untool_rec$.processTuple>>
let dt_pre = <:expr<$lid:N.untool_rec$.processDatatype>>

let init_from_tid tid = <:expr<$uid:Id.id2string tid$.$uid:N.untraversal_mod$.$lid:N.init_fun$ $lid:N.untool_rec$>>
let init_from_tpapp tp_fun tp_args = proj_val_from_trav_mod tp_fun tp_args N.init_fun

let untraverse_from_tid tid = <:expr<$uid:Id.id2string tid$.$uid:N.untraversal_mod$.$lid:N.untraversal_fun$ $lid:N.untool_rec$>>
let untraverse_from_tpapp tp_fun tp_args = proj_val_from_trav_mod tp_fun tp_args N.untraversal_fun
      
(********************* untraversal gen code ****************************)
let make_fresh s = Id.id2string (Id.freshid s)

let gen_record_untraversal gt is_tuple fields = 
  (* traversal function-creating function *)
  let t_s = "t" in 
  let t_p = <:patt<$lid:t_s$>> in 
  let t_e = <:expr<$lid:t_s$>> in 
  let fun_e body = <:expr<fun $t_p$ -> $body$>> in 
    
  (* names and types of all fields *)
  let names_tps = C.munge_fields
    (fun ctp -> [])
    (fun (id,tp) -> [id,tp])
    (fun (id,tp,_) -> [id,tp])
    fields in
    
  (* big fold to obtain:
     - pattern of all names
     - expression of all names as strings
     - assignments of to record
     - function to construct the body *)
  let names_p,names_e,assgns_e,body_fn = List.fold_left 
    (fun (acc_ps,acc_es,acc_as,e_fn) (ni,tp) -> 
      let ni_str = Id.id2string ni in 
      let ni_str' = ni_str ^ "'" in 
      let ni_p = <:patt<$lid:ni_str$>> in 
      let ni_e = <:expr<$lid:ni_str$>> in 
      let ni_p' = <:patt<$lid:ni_str'$>> in 
      let ni_e' = <:expr<$lid:ni_str'$>> in 
      let ni_s = <:expr<$str:ni_str$>> in 
      let acc_ps' = <:patt<[$ni_p$::$acc_ps$]>> in 
      let acc_es' = <:expr<[$ni_s$::$acc_es$]>> in 
      let acc_as' = (ni_p,ni_e')::acc_as in 
      let e_fn' = (fun e -> <:expr<let $ni_p'$ = $gt tp$ $ni_e$ in $e_fn e$>>) in 
        (acc_ps',acc_es', acc_as', e_fn'))
    (<:patt<[]>>, <:expr<[]>>, [], (fun e -> e))
    (List.rev names_tps) in 

  let unpack = 
    if is_tuple then <:expr<$tuple_pre$>> 
    else <:expr<$record_pre$ $names_e$>> in
    
  (* apply assgns to fun_e to obtain traversal *)
  let body = match is_tuple, assgns_e with 
      true,[e] -> snd e
    | true,_   -> <:expr<( $list:List.map snd assgns_e$ )>>
    | _        -> body_fn <:expr<{ $list:assgns_e$ }>> in      
  fun_e <:expr<let $names_p$ = $unpack$ $t_e$ in $body_fn body$>>
                                                  
let rec gen_untraversal dt tc loc = function
    Ast.TupleTp ctps -> 
      (* XXX: 
       * code copied from TraversalGenerator's gen_init
       * THIS SHOULD REALLY LIVE IN ONE PLACE! 
       *)
      let rid i = "elt"^(string_of_int i) in
        
      (* Convert a tuple-element AST node to a record-field AST node. *)
      let convert (i,fds) = function
	  Ast.Type tp -> i+1,Ast.FullField(Id.makeid (rid i),tp)::fds
	| (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
	
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
      let fields = List.rev fields_rev in
	gen_record_untraversal (gen_untraversal dt tc loc) true fields

  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	  None -> PError.report_error loc ("UntraversalGenerator: Type " ^ (Id.id2string tid) ^ " not found.")
	| Some {name=_;kind=D.Base;tyclass=D.Current} -> <:expr<$lid:N.untraversal_fun$ untool>>
	| Some {name=_;kind=D.Base;tyclass=_} -> untraverse_from_tid tid
	| _ -> PError.report_error loc 
	     "UntraversalGenerator: Unsupported feature: type identifiers cannot have higher kinds.")

  | Ast.ValAppTp (tp_fun,exp) -> gen_untraversal dt tc loc tp_fun
  | Ast.TpAppTp (tp_args,tp_fun) -> 
      (match D.lookup_descr dt tp_fun with
	  None -> PError.report_error loc ("Type " ^ (Id.id2string tp_fun) ^ " not found.")
	| Some {name=_;kind=D.Fun(params_md);tyclass=D.Current} -> <:expr<$lid:N.untraversal_fun$ untool>>
	| Some {name=_;kind=D.Fun(params_md);tyclass=_} -> untraverse_from_tpapp tp_fun tp_args
	| _ -> PError.report_error loc "Use does not match kind."
      )

  | Ast.WhereTp (id,tp,exp) -> gen_untraversal dt tc loc tp 

  | _ -> PError.report_error loc "unsupported feature"
   
let rec gen_tp_untraversal dt tc loc = function                                                               
    Ast.RecordTp fields -> 
      gen_record_untraversal (gen_untraversal dt tc loc) false fields
  | t -> gen_untraversal dt tc loc t
   
let gen_dtp_untraversal dt tc loc dtp = 
  let mk_full x tp = 
    let x_s = Id.id2string x in 
    let x' = "x'" in 
      (<:patt<($str:x_s$,x)>>, 
      None,
      <:expr<let $lid:x'$ = $gen_untraversal dt tc loc tp$ x in 
             $uid:x_s$ $lid:x'$>>) in

  let mk_absorb x = 
    let x_s = Id.id2string x in 
      (<:patt<($str:x_s$,x)>>,None,<:expr<$uid:x_s$>>) in

  let cases = match dtp with 
      Ast.ImplicitDT (vs,_) -> 
        let mkcase = function
            Ast.FullVar(id,tp) -> mk_full id tp
          | Ast.AbsorbVar(id,_) -> mk_absorb id in 
          List.map mkcase vs
    | Ast.CaseDT (_,cases) -> 
        let mkcase = function
            Ast.FullCase(_,id,tp) | Ast.GenCase(_,id,tp,_) -> 
              mk_full id tp
          | Ast.AbsorbCase(_,id,_) -> 
              mk_absorb id in
          List.map mkcase cases in 
    <:expr<fun t -> match($dt_pre$ t) with [$list:cases$]>>
        
let gen dt current_descr tc loc (tp_params,name,Ast.DefDecl (_, tp_def)) = 
  (* untraversal function *)
  let untraversal_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_untraversal dt tc loc tp
    | Ast.DtDef dtp_body -> gen_dtp_untraversal dt tc loc dtp_body in
 
  (* untraversal module signature. *)
  let untraverse_si = <:sig_item<value untraverse : Generic_untool.Rec_ver.t 'a -> 'a -> rep>> in 
  let module_sig = <:module_type<sig $list:[untraverse_si]$ end>> in
  let module_sig = <:sig_item<module $uid:N.untraversal_functor$ : $module_sig$>> in

  (* untraversal module *)
  let untraverse_st = <:str_item<value rec untraverse $lid:N.untool_rec$ = $untraversal_fun$>> in
  let untr_mod_body = <:module_expr<struct $list:[untraverse_st]$ end >> in
  let untr_mod = <:str_item<module $uid:N.untraversal_mod$ = $untr_mod_body$>> in
    [], ([module_sig],[<:str_item<open Generic_untool.Rec_ver>>;untr_mod]), current_descr
