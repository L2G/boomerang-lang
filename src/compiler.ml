(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - Focal checker and interpreter                  *)
(****************************************************************)
(* $Id$ *)

open Syntax

(* --------------- Imports --------------- *)
let sprintf = Printf.sprintf  
let (@) = Safelist.append
let mk_rv = Registry.make_rv
let s_of_rv = Registry.sort_of_rv
let v_of_rv = Registry.value_of_rv

(* --------------- Unit tests --------------- *)

(* unit tests either succeed, yielding a value, or fail with a msg *)
type testresult = OK of V.t | Error of (unit -> unit)

let tests = Prefs.createStringList
  "test"
  "run unit test for the specified module"
  "run unit tests for the specified module"
let _ = Prefs.alias tests "t"
  
let test_all = Prefs.createBool "test-all" false
  "run unit tests for all modules"
  "run unit tests for all modules"

(* [check_test m] returns true iff the command line arguments
   '-test-all' or '-test m' are set *)
let check_test m = 
  Safelist.fold_left 
    (fun r qs -> r or (Syntax.qid_prefix (Value.parse_qid qs) m))
    (Prefs.read test_all)
    (Prefs.read tests)

let no_assert = Prefs.createBool "no-assert" false
  "don't check assertions"
  "don't check assertions"

(* --------------- Error Reporting --------------- *)
let debug s_thk = Trace.debug "compiler" (fun () -> Format.eprintf "@[%s@\n%!@]" (s_thk ()))
  
let parse_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "@[%s: Parse error @\n" (Info.string_of_t i);
              msg_thk ();
              Format.printf "@]"))
      
let sort_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> 
              Format.printf "@[%s: Sort checking error@\n" (Info.string_of_t i);
              msg_thk ();
              Format.printf "@]"))
    
let test_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "@[%s: Unit test failed @ " (Info.string_of_t i); 
              msg_thk ();
              Format.printf "@]"))
    
let run_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "@[%s: Unexpected run-time error @\n" 
              (Info.string_of_t i);
              msg_thk ();
              Format.printf "@]"))
          
(* --------------- Environments --------------- *)
module type CommonEnvSig = sig
  type t 
  val empty : unit -> t
  val get_ev : t -> Registry.REnv.t
  val set_ev : t -> Registry.REnv.t -> t
  val get_ctx : t -> Syntax.qid list
  val set_ctx : t -> Syntax.qid list -> t
end

(* compilation environments *)
module type CEnvSig = sig
  include CommonEnvSig
  val lookup : t -> Syntax.qid -> Registry.rv option
  val update : t -> Syntax.qid -> Registry.rv -> t
  val overwrite : t -> Syntax.qid -> Registry.rv -> unit
end 

(* sort checking environments *)
module type SCEnvSig = sig
  include CommonEnvSig
  val lookup : t -> Syntax.qid -> Syntax.sort option
  val update : t -> Syntax.qid -> Syntax.sort -> t
end

module CEnv : CEnvSig = struct
  type t = Syntax.qid list * (Registry.REnv.t)
      
  let empty () = ([], (Registry.REnv.empty ()))    

  (* getters and setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_ctx cev = let (os,_) = cev in os
  let set_ctx cev os = let (_,ev) = cev in (os,ev)
				
  (* lookup from a cev, then from the library *)
  let lookup cev q = 
    match Registry.REnv.lookup (get_ev cev) q with
	None -> Registry.lookup_library_ctx (get_ctx cev) q
      | Some rv -> Some rv
	  
  (* update a cev with a new rv *)
  let update cev q rv = 
    set_ev cev (Registry.REnv.update (get_ev cev) q rv)

  (* overwrite a cev's entry for q with a new rv *)
  let overwrite cev q rv = 
    Registry.REnv.overwrite (get_ev cev) q rv
end
	
module SCEnv : SCEnvSig = struct
  type t = CEnv.t
  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx

  let lookup sev q = 
    match CEnv.lookup sev q with
	None -> None
      | Some rv -> Some (s_of_rv rv)
	  
  let update sev q s = 
    CEnv.update sev q (mk_rv s (Value.dummy s (Value.parse_qid "NONE")))
end

let check_schemas_cell = ref true

(* --------------- Sort checker --------------- *)

(* helper function to convert a list of parameters [ps] with sorts
 * [s1,..,sk] and a sort [s] to the arrow sort [s1 -> ... -> sk -> s] *)
let sort_of_params i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(sort_of_param p,ts))  ps s

(* check if one sort is a subsort of another *)
let rec subsort u v = match u,v with	
    SArrow(s11,s12),SArrow(s21,s22) -> 
      let b1 = subsort s21 s11  in
      let b2 = subsort s12 s22 in 
	b1 && b2
  | STree,SSchema  -> true
  | _ when u = v -> true
  | _            -> false

let rec expect_sort_exp msg sev expected_sort e =
  let i = info_of_exp e in
  let e_sort, new_e = check_exp sev e in
    if subsort e_sort expected_sort then 
      (e_sort,new_e)
    else
      sort_error i
	(fun () -> 
           Format.printf "@[in %s:@ " msg;
           Syntax.format_sort expected_sort;
           Format.printf " expected@ but ";
           Syntax.format_sort e_sort;
           Format.printf "@ found@]")
	
(* EXPRESSIONS *)    
and check_exp sev e0 = match e0 with
    (* applications *)
    EApp(i,e1,e2) -> begin
      match check_exp sev e1 with
	  SArrow(param_sort,return_sort),new_e1 -> 
	    let e2_sort,new_e2 = expect_sort_exp "application" sev param_sort e2 in
	    let new_e0 = EApp(i, new_e1, new_e2) in 
	      (return_sort, new_e0)
	| e1_sort,_ -> 
	    sort_error i 
	      (fun () -> 
                 Format.printf
		   "@[expected@ arrow@ sort@ in@ left-hand@ side@ of@ application@ but@ found";
                 Syntax.format_sort e1_sort;
                 Format.printf "@]")
    end
      
  (* assertions *)
  | EAssert(i,e1) -> 
      let _,new_e1 = expect_sort_exp "assert schema" sev SSchema e1 in 
      let new_e0 = EAssert(i, new_e1) in
        (SLens, new_e0)

  | ECheckLens(i,e1,e2,e3) ->
      let _,new_e1 = expect_sort_exp "check lens lens" sev SLens e1 in 
      let _,new_e2 = expect_sort_exp "check lens schema" sev SSchema e2 in  
      let _,new_e3 = expect_sort_exp "check lens schema" sev SSchema e3 in 
      let new_e0 = ECheckLens(i,new_e1,new_e2,new_e3) in
	(SLens,new_e0)

    (* type atoms *)
    | EAtom(i,e1,e2) ->
	let _,new_e1 = expect_sort_exp "atom name" sev SName e1 in
	let e2_sort, new_e2 = expect_sort_exp "atom schema" sev SSchema e2 in
	let new_e0 = EAtom(i, new_e1, new_e2) in
	  (e2_sort, new_e0)
	    
    | ECat(i,es) ->
        let merge_sorts s1 s2 = match s1,s2 with
            STree,STree -> STree
          | _           -> SSchema in
        let rec check_lift_cats es = Safelist.fold_left
          (fun (es_sort,new_es_rev) ei -> match ei with
               ECat(_,fs) -> 
                 let fs_sort, new_fs_rev = check_lift_cats fs in
                   (merge_sorts es_sort fs_sort, new_fs_rev@new_es_rev)
             | _ -> 
	         let ei_sort, new_ei = 
	           expect_sort_exp "concatenation (tree or schema)" sev SSchema ei in
	           (merge_sorts es_sort ei_sort,new_ei::new_es_rev))
	  (STree,[])
	  es in
        let e0_sort,new_es_rev = check_lift_cats es in
	let new_e0 = ECat(i, Safelist.rev new_es_rev) in
	  (e0_sort, new_e0)

    | ECons(i,e1,e2) -> 
	let e1_sort, new_e1 = expect_sort_exp "cons (tree or schema)" sev SSchema e1 in
	let e2_sort, new_e2 = expect_sort_exp "cons (tree or schema)" sev SSchema e2 in
	let e0_sort = 
	  match e1_sort,e2_sort with
	      STree,STree -> STree
	    | _           -> SSchema in
	let new_e0 = ECons(i, new_e1, new_e2) in
	  (e0_sort, new_e0)
	    
    | ESpineCons(i,e1,e2) -> 
	let e1_sort, new_e1 = expect_sort_exp "spine cons (tree or schema)" sev SSchema e1 in
	let e2_sort, new_e2 = expect_sort_exp "spine cons (tree or schema)" sev SSchema e2 in
	let e0_sort = 
	  match e1_sort,e2_sort with
	      STree,STree -> STree
	    | _           -> SSchema in
	let new_e0 = ESpineCons(i, new_e1, new_e2) in
	  (e0_sort, new_e0)
	    
    | EFun(i,[],_,_) -> 
	run_error i (fun () -> Format.printf "@[function without parameters]")
	
  | EFun(i,p1::p2::ps,return_sort_opt,body) ->
      (* multi-parameter function; rewrite to simple lambda *)
      let new_body = EFun(i,p2::ps,return_sort_opt,body) in	    
	check_exp sev (EFun(i,[p1],None,new_body))	    
	  
  | EFun(i,[p],return_sort_opt,body) ->
      let p_sort = sort_of_param p in
      let p_id = id_of_param p in	  
      let body_sev = SCEnv.update sev (qid_of_id p_id) p_sort in
      let body_sort, new_body = 
	match return_sort_opt with
	    Some s -> expect_sort_exp "function body" body_sev s body
	  | None   -> check_exp body_sev body
      in
      let e0_sort = SArrow(p_sort, body_sort) in
      let new_e0 = EFun(i,[p], Some body_sort, new_body) in
	(e0_sort, new_e0)
	  
  | ELet(i,bs,e) ->
      let bsev,_,new_bs = check_bindings sev bs in
      let e0_sort, new_e = check_exp bsev e in
      let new_e0 = ELet(i, new_bs, new_e) in
	(e0_sort, new_e0)
	 	  
  | EName(i,x) -> (SName, EName(i, x))
      
  | ENil(i) -> (STree, ENil(i))
      
  | EProtect(i,e) -> 
      let _, new_e = expect_sort_exp "protect" sev SLens e in
        (SLens, EProtect(i,new_e))
          
  | ESchema(i,ss,e) ->
      let ssev,_,new_ss = check_schema_bindings sev ss in
      let e0_sort, new_e = check_exp ssev e in 
      let new_e0 = ESchema(i,new_ss,new_e) in
        (e0_sort, new_e0)
          
  | EUnion(i,es) ->
      let rec check_lift_unions es = Safelist.fold_left 
        (fun acc ei -> match ei with 
             EUnion(_,fs) -> (check_lift_unions fs)@acc
           | _ -> snd (expect_sort_exp "union schema" sev SSchema ei)::acc)
        [] es in 
      let new_e0 = EUnion(i, Safelist.rev (check_lift_unions es)) in
        (SSchema, new_e0)

  | EInter(i,es) ->
      let rec check_lift_inters es = Safelist.fold_left 
        (fun acc ei -> match ei with 
             EInter(_,fs) -> (check_lift_inters fs)@acc
           | _ -> snd (expect_sort_exp "intersection schema" sev SSchema ei)::acc)
        [] es in 
      let new_e0 = EInter(i, Safelist.rev (check_lift_inters es)) in
        (SSchema, new_e0)

  | EMinus(i,e1,e2) -> 
      let _,new_e1 = expect_sort_exp "negation schema" sev SSchema e1 in
      let _,new_e2 = expect_sort_exp "negation schema" sev SSchema e2 in
        (SSchema,EMinus(i,new_e1,new_e2))

  | EVar(i,q,_) ->
      begin match SCEnv.lookup sev q with
          Some s -> (s,e0)
	| None -> 
            sort_error i 
	      (fun () -> 
                 Format.printf "@[%s is not bound@]"
		   (string_of_qid q))
      end
        
  | EWild(i,es,l,u,e) ->
      let new_es = Safelist.map 
	(fun ei -> snd (expect_sort_exp "wildcard exception list" sev SName ei))
	es 
      in
      let _,new_e = expect_sort_exp "wildcard schema" sev SSchema e in
      let new_e0 = EWild(i,new_es,l,u,new_e) in
	(SSchema, new_e0)
          
and check_bindings sev bs = 	    
  let bsev =
    Safelist.fold_left
      (fun bsev (BDef(i,f,xs,s,e)) ->
	 SCEnv.update bsev 
           (qid_of_id f) 
           (sort_of_params i xs s))
      sev
      bs in
  let rec check_binding sev bi = match bi with 
      Syntax.BDef(i,f,[],s,e) -> 
	let f_qid = qid_of_id f in
	let e_sort, new_e = expect_sort_exp "let binding" sev s e in
	let new_bi = Syntax.BDef(i, f, [], e_sort, new_e) in
	  (SCEnv.update bsev f_qid e_sort, f_qid, new_bi)
    | Syntax.BDef(i,f,ps,s,e) -> 
        (* rewrite bindings with parameters to plain ol' lambdas *)          
	let new_e = EFun(i,ps,Some s,e) in
	let new_s = sort_of_params i ps s in
          check_binding sev (Syntax.BDef(i,f,[],new_s,new_e)) in
  let bsev,names_rev,new_bs_rev = Safelist.fold_left 
    (fun (bsev, names_rev,new_bs_rev) bi ->  
       let bsev, f_qid, new_bi = check_binding bsev bi in
	 bsev, f_qid::names_rev, new_bi::new_bs_rev)
    (bsev,[],[])
    bs
  in
    (bsev, Safelist.rev names_rev, Safelist.rev new_bs_rev)
      
and check_schema_bindings sev ss = 	    
  let ssev = 
    Safelist.fold_left
      (fun ssev (SDef(i,x,_)) -> SCEnv.update ssev (qid_of_id x) SSchema)
      sev
      ss in
  let rec check_schema_binding sev si = match si with 
      Syntax.SDef(i,x,e) -> 
	let x_qid = qid_of_id x in
        let _, new_e = expect_sort_exp "schema binding" sev SSchema e in
	let new_si = Syntax.SDef(i, x, new_e) in
	  (SCEnv.update ssev x_qid SSchema, x_qid, new_si)
  in
  let ssev,names_rev,new_ss_rev = Safelist.fold_left 
    (fun (ssev, names_rev,new_ss_rev) si ->  
       let ssev, x_qid, new_si = check_schema_binding ssev si in
	 ssev, x_qid::names_rev, new_si::new_ss_rev)
    (ssev,[],[])
    ss
  in
    (ssev, Safelist.rev names_rev, Safelist.rev new_ss_rev)
      
(* type check a single declaration *)
let rec check_decl sev m d0 = match d0 with
  | DLet(i,bs) -> 
      let new_sev, names, new_bs = check_bindings sev bs in
      let new_d0 = DLet(i, new_bs) in
	(new_sev, names, new_d0)
          
  | DMod(i,n,ds) ->
      let n_qid = qid_of_id n in	
      let mn = Syntax.dot m n_qid in
      let m_sev,names,new_ds = check_module_aux sev mn ds in
      let new_sev, names_rev = Safelist.fold_left 
	(fun (new_sev, names) q -> 
	   match Registry.REnv.lookup (SCEnv.get_ev m_sev) q with
	       None -> run_error i 
		   (fun () -> 
		      Format.printf "@[the compiled declaration for %s went missing@]"
			(string_of_qid q))
	       | Some q_rv ->
		   let nq_dot_q = dot n_qid q in
		     (SCEnv.update sev nq_dot_q (s_of_rv q_rv), nq_dot_q::names))
	  (sev,[])
	  names
	in
	let new_d0 = DMod(i,n,new_ds) in
	  (new_sev, Safelist.rev names_rev, new_d0)

    | DSchema(i,ss) ->
        let new_sev, names, new_ss = check_schema_bindings sev ss in
        let new_d0 = DSchema(i,ss) in
          (new_sev, names, new_d0)

    | DTest(i,e,res) ->	
	if not (check_test m) then (sev, [], d0)
	else begin
	  let _,new_e = expect_sort_exp "test expression" sev STree e in
	  let new_res = 
	    match res with 
		ErrorResult -> ErrorResult
  	      | PrintResult -> PrintResult
	      | Result res -> 
		  let _,new_res = expect_sort_exp "test result" sev STree res in
		    Result(new_res) in
	  let new_d0 = DTest(i,new_e, new_res) in
	    (sev, [], new_d0)
	end
	  
and check_module_aux sev m ds = 
  let new_sev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sev, names, new_ds_rev) di ->
	 let new_sev, new_names, new_di = check_decl sev m di in
	   new_sev, names@new_names, new_di::new_ds_rev)
      (sev,[],[])
      ds
  in
    (new_sev, names, Safelist.rev new_ds_rev)
      
let check_module m0 = 
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let sev = SCEnv.set_ctx (SCEnv.empty ()) (nctx@Registry.pre_ctx) in
  let _,_,new_ds = check_module_aux sev (Syntax.qid_of_id m) ds in 
  let new_m0 = Syntax.MDef(i,m,nctx,new_ds) in
    new_m0

(* --------------- Compiler --------------- *)

(* --------------- expressions ------------- *)
let rec compile_exp cev e0 = match e0 with
  | EApp(i,e1,e2) -> 	
      let e1_rv = compile_exp cev e1 in
      let e2_rv = compile_exp cev e2 in
      let v1 = v_of_rv e1_rv in
      let v2 = v_of_rv e2_rv in
	begin 
	  match s_of_rv e1_rv with
	    | SArrow(_,return_sort) -> 
		begin match v1 with
		    Value.F(_,f) -> mk_rv return_sort (f v2)
		  | _   -> 
                      run_error 
                        i 
                        (fun () -> 
                           Format.printf
                             "@[expected function at left-hand side of application but found ";
                           Value.format_t v2;
                           Format.printf "@]")
		end
	    | s -> 
                run_error i 
		  (fun () -> 
                     Format.printf
                       "@[expected function sort at left-hand side of application but found ";
                     Syntax.format_sort s;
                     Format.printf "@]")
	end

  | EAssert(i,e1) ->
      let t = compile_exp_schema cev e1 in 
      let check_assert dir v = 
        if (not (Prefs.read no_assert)) then
          match Schema.member v t with
	      true -> ()
            | false -> 
	          Lens.error [`String (Info.string_of_t i); `Space;
                              `String (sprintf "assert(%s): tree" dir); `Space;
		              `Tree v; `Space;
		              `String "is not a member of";  `Space;
		              `Prim (fun () -> Schema.format_t t);
		              (* `Space; `String "because"; `Space;
		              `Tree v0; `Space;
		              `String "is not a member of";  `Space;
		              `Prim (fun () -> Schema.format_t t0) *)] in                    
      let checker = Value.BIJ ((fun c -> c),(fun a -> a)) in 
        mk_rv 
          SLens
          (Value.L (Lens.native 
                      (fun c -> check_assert "get" c; c)
                      (fun a co -> 
                         check_assert "put" a; 
                         (match co with None -> () | Some c -> check_assert "put" c);
                         a),
                    checker))
  | ECheckLens(i,e1,e2,e3) ->
      let l,ck = compile_exp_lens cev e1 in 
      let c = compile_exp_schema cev e2 in 
      let ck_c = match ck with Value.BIJ(c2a,_) | Value.VWB(c2a) | Value.WB(c2a) -> c2a c in
      let a = compile_exp_schema cev e3 in
        if not (Schema.equivalent ck_c a) then 
	  Lens.error [`String (Info.string_of_t i); `Space
                     ; `String "static lens type assertion failed; expected:"; `Space
		     ; `Prim (fun () -> Schema.format_t a); `Space
                     ; `String "but found:"; `Space
		     ; `Prim (fun () -> Schema.format_t ck_c)]
        else (mk_rv SLens (Value.L (l,ck)))

  | EAtom(i,e1,e2) ->
      let n = compile_exp_name cev e1 in
      let e2_rv = compile_exp cev e2 in
	begin match v_of_rv e2_rv with
	    Value.V v -> 
	      mk_rv
		STree
		(Value.V (V.set V.empty n (Some v)))
	  | Value.S s ->
              let s = Schema.mk_atom n s in
		mk_rv SSchema (Value.S s)
	  | v -> run_error i
              (fun () -> 
                 Format.printf 
                   "@[expected schema or tree in atom but found ";
                 Value.format_t v;
                 Format.printf "@]")
	end
	  
    | ECat(i, es) ->
	let e0_sort, vs_rev = 
	  Safelist.fold_left 
	    (fun (s,vs_rev) ei -> 		 
               let ei_rv = compile_exp cev ei in 
	       let v = v_of_rv ei_rv in
		 match s, v with
		     STree, (Value.V _) -> 
		       (s, v::vs_rev)
		   | STree, (Value.S _) ->
		       (SSchema, 
			v::(Safelist.map (fun v -> Value.S (Value.get_schema i v)) vs_rev))
		   | SSchema, (Value.V _) 
		   | SSchema, (Value.S _) ->
		       (s, (Value.S (Value.get_schema i v))::vs_rev)
		   | _ -> run_error i 
                       (fun () -> 
			  Format.printf "@[bogus value in ECat:";
                          Registry.format_rv ei_rv;
                          Format.printf "@]"))
	    (STree,[])
	    es in 
	  begin
	    match e0_sort with
		STree -> 
		  let vi = Safelist.fold_left
		    (fun vacc v -> V.concat vacc (Value.get_tree i v))
		    V.empty 
		    (Safelist.rev vs_rev)
		  in
		    mk_rv STree (Value.V(vi))		      
	      | SSchema ->                   
		  let ts = Safelist.rev_map (Value.get_schema i) vs_rev in                    
                  let s = Schema.mk_cat ts in
 		    mk_rv SSchema (Value.S s)
	      | _ -> run_error i 
                  (fun () -> 
                     Format.printf "@[unexpected sort in ECat: ";
                     Syntax.format_sort e0_sort;
                     Format.printf "@]")
	  end
    | ECons(i,e1,e2) ->         
	let e1_v = v_of_rv (compile_exp cev e1) in 
	let e2_v = v_of_rv (compile_exp cev e2) in    
	  begin match e1_v, e2_v with
	      Value.V v1, Value.V v2     -> 
		mk_rv STree (Value.V (V.cons v1 v2))
	    | Value.V _, Value.S t ->
                let s = Schema.mk_cons (Value.get_schema i e1_v) t in
	      	  mk_rv SSchema (Value.S s)
	    | Value.S t, Value.V _ ->
                let s = Schema.mk_cons t (Value.get_schema i e2_v) in
	      	  mk_rv SSchema (Value.S s)
	    | Value.S t1, Value.S t2 -> 
		let s = Schema.mk_cons t1 t2 in 
	      	  mk_rv SSchema (Value.S s)
	    | _ -> run_error i (fun () -> Format.printf "@[expected tree or type in atom@]")
	  end

    | ESpineCons(i,e1,e2) ->         
	let e1_v = v_of_rv (compile_exp cev e1) in 
	let e2_v = v_of_rv (compile_exp cev e2) in    
	  begin match e1_v, e2_v with
	      Value.V v1, Value.V v2     -> 
		mk_rv STree (Value.V (V.spined_cons v1 v2))
	    | Value.V _, Value.S t ->
                let s = Schema.mk_spine_cons (Value.get_schema i e1_v) t in
	      	  mk_rv SSchema (Value.S s)
	    | Value.S t, Value.V _ ->
                let s = Schema.mk_spine_cons t (Value.get_schema i e2_v) in
	      	  mk_rv SSchema (Value.S s)
	    | Value.S t1, Value.S t2 -> 
		let s = Schema.mk_spine_cons t1 t2 in 
	      	  mk_rv SSchema (Value.S s)
	    | _ -> run_error i (fun () -> Format.printf "@[expected tree or type in atom@]")
	  end

    | EFun(i,[p],Some ret_sort,e) ->
	(* fully-annotated, simple lambda *)
	(* the actual implementation of f *)
	let param_qid = qid_of_id (id_of_param p) in
	let param_sort = sort_of_param p in
	let f_sort = SArrow(param_sort, ret_sort) in
	let f_impl v =
	  let body_cev = 
	    CEnv.update cev param_qid (mk_rv param_sort v) in
	    v_of_rv (compile_exp body_cev e)
	in
	  mk_rv 
	    f_sort
	    (Value.F (f_sort, f_impl))

    | EFun(i,_,_,_) -> 
	run_error i (fun () -> Format.printf "@[unflattened or unannotated function@]")
	  
    | ELet(i, bs,e) ->
	let bcev,_ = compile_bindings cev bs in
	  compile_exp bcev e	    

    | EName(i, x) -> 
	mk_rv 
	  SName 
	  (Value.N (name_of_id x))
	  
    | ENil(i) -> 
	mk_rv
	  STree
	  (Value.V (V.empty_list))

    | EProtect(i,e) -> 
        (* call by need lenses: evaluate the lens the first time it is
           needed, then backpatch the ref cell so that subsequent
           applications of the lens require no further evaluation.  *)
        let lensro = ref None in
        let fetch_lens () = match !lensro with
            None -> 
              let lc = compile_exp_lens cev e in
                lensro := Some lc;
                lc
          | Some lc -> lc
        in
        let get c = Lens.get (fst (fetch_lens ())) c in
        let put a co = Lens.put (fst (fetch_lens ())) a co in
        let checker = Value.WB(fun c -> run_error i (fun () -> "protect lenses are unchecked")) in
          mk_rv 
            SLens
            (Value.L (Lens.native get put, checker))
            
    | ESchema(i,ss,e) ->         
        let scev,_ = compile_schema_bindings cev ss in 
          compile_exp scev e 

    | EUnion(i, es) ->
	let ts = Safelist.map (fun ei -> compile_exp_schema cev ei) es in
        let s = Schema.mk_union ts in 
	  mk_rv SSchema (Value.S s)

    | EInter(i, es) ->
	let ts = Safelist.map (fun ei -> compile_exp_schema cev ei) es in
        let s = Schema.mk_isect ts in 
	  mk_rv SSchema (Value.S s)

    | EMinus(i,e1,e2) -> 
        let s1 = compile_exp_schema cev e1 in
        let s2 = compile_exp_schema cev e2 in
        let s = Schema.mk_diff s1 s2 in
	  mk_rv SSchema (Value.S s)

    | EVar(i,q,_) -> begin 
        match CEnv.lookup cev q with
	    Some rv -> 
              if Value.is_dummy (v_of_rv rv) then 
                (* for agreement with the paper semantics, we should
                   uncomment out the next line so that we diverge here
                   instead of throwing an exception; it seems more
                   helpful to throw the exception. *)
                (* let rec diverge () = diverge () in diverge () *)
                run_error (info_of_exp e0)
                  (fun () -> 
                     Format.printf "@[variable %s may not be used recursively@]"
                       (Syntax.string_of_qid q))
              else rv
	  | None -> run_error 
	      (info_of_exp e0)
	        (fun () -> 
		   Format.printf "@[unbound variable %s@]"
		     (string_of_qid q))
      end

    | EWild(i,es,l,u,e) ->
	let f = 
          Safelist.fold_left 
            (fun f ei -> Name.Set.add (compile_exp_name cev ei) f)
            Name.Set.empty 
            es in
	let t = compile_exp_schema cev e in 
        let s = Schema.mk_wild f l u t in
	  mk_rv SSchema (Value.S s)
	   
(* compilation helpers when we know the sort *)	  	  	    
and compile_exp_name cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_name i (v_of_rv e_rv)
      
and compile_exp_tree cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_tree i (v_of_rv e_rv)
      
and compile_exp_schema cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_schema i (v_of_rv e_rv)
      
and compile_exp_lens cev e = 
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_lens i (v_of_rv e_rv)

and compile_bindings cev bs =
  (* collect up a CEnv that includes mappings for 
     the name of each recursive binding *)
  let bcev =
    Safelist.fold_left
      (fun bcev (BDef(i,f,xs,s,e)) ->
	 let f_qid = qid_of_id f in
	 let b_sort = (sort_of_params i xs s) in
	 let dummy = Value.dummy b_sort (Syntax.qid_of_id f) in
	   CEnv.update bcev f_qid (mk_rv b_sort dummy))
      cev
      bs in

  (* backpactch bcev with compiled value for each recursive binding *)
  let names_rev = Safelist.fold_left
    (fun names_rev bi -> match bi with 
         Syntax.BDef(i,f,[],s,e) ->
	   let f_qid = qid_of_id f in
	   let e_rv = compile_exp bcev e in
	   let memoized_v = Value.memoize (v_of_rv e_rv) in
	   let memoized_rv = mk_rv (s_of_rv e_rv) memoized_v in
             CEnv.overwrite bcev f_qid memoized_rv;
             f_qid::names_rev
       | Syntax.BDef(i,_,_,_,_) -> run_error i 
           (fun () -> Format.printf "@[unflattened binding@]"))
    []
    bs
  in
    bcev, Safelist.rev names_rev
      
and compile_schema_bindings cev sbinds =
  (* pass 1: get a fresh schema variable for each bound name and update
     the compilation environment maps each bound name to the 
     corresponding schema variable *)
  let scev,i_rev,fresh_xs_rev,names_rev = Safelist.fold_left
    (fun (scev,i_rev,fresh_xs_rev,names_rev) (SDef(i,x,e)) ->
       let q_x = qid_of_id x in
       let fresh_x = Schema.fresh (Syntax.string_of_id x) in
       let scev' = CEnv.update scev q_x
         (mk_rv SSchema (Value.S (Schema.mk_var fresh_x))) in            
         (scev', i::i_rev,fresh_x::fresh_xs_rev, q_x::names_rev))
    (cev,[],[],[])
    sbinds in    
  let both_names = Safelist.rev (Safelist.combine fresh_xs_rev names_rev) in
  let annot_sbinds = Safelist.combine both_names sbinds in

  (* set some state: 
     - mark the recursive variables
     - don't finish delayed work in compiling (we'll do that ourselves if needed) *)
  let old_check_schemas = !check_schemas_cell in 
    check_schemas_cell := false;
    Schema.mark_tvars (Safelist.rev (Safelist.combine fresh_xs_rev i_rev));
   
  (* pass 2: compile each schema expression and update the compilation
     environment so that every q_x points to the actual compiled
     schema not just a schema variable. *)
    Safelist.iter
      (fun ((fresh_x,q_x),SDef(_,_,e)) ->
         let s = compile_exp_schema scev e in 
           Schema.update fresh_x s;
           CEnv.overwrite scev q_x (mk_rv SSchema (Value.S s)))
      annot_sbinds;
  
    (* finalize schemas, if flag set *)
    check_schemas_cell := old_check_schemas;
    if (!check_schemas_cell) then Schema.finalize ();
    
    (* result *)
    (scev, Safelist.rev names_rev)
      
(* type check a single declaration *)
let rec compile_decl cev m di = 
  let _ = Trace.debug "decl" (fun () -> 
                                Format.printf "@\n";
                                Syntax.format_decl di; 
                                Format.print_newline ()) in
    match di with
  | DLet(i,bs) -> compile_bindings cev bs 
  | DMod(i,n,ds) ->
      let nq = qid_of_id n in
      let mn = dot m nq in
      let m_cev, names = compile_module_aux cev mn ds in
      (* insert all the just-compiled names into the environment *)
      let new_cev, names_rev =
	Safelist.fold_left
	  (fun (cev, names) q ->
	     match Registry.REnv.lookup (CEnv.get_ev m_cev) q with
		 Some rv ->
		   let nq_dot_q = dot nq q in
		     (CEnv.update cev nq_dot_q rv, nq_dot_q::names)
	       | None -> run_error i
		   (fun () -> Format.printf "@[the compiled declaration for %s went missing@]"
		      (string_of_qid q)))
	  (cev,[])
	  names
      in
	new_cev, Safelist.rev names_rev
  | DSchema(i,ss) -> compile_schema_bindings cev ss
  | DTest(i,e,res) ->
      if check_test m then
	begin
	  let vo = 
	    try
	      OK (compile_exp_tree cev e)
	    with (Error.Harmony_error(m)) -> Error m
	  in
	    match vo, res with 
	      | OK v, PrintResult -> 
		  V.format_msg [`Newline; `String "Test result:"; `Space; `Tree v];
	      | Error m, PrintResult -> 
		  V.format_msg [`Newline; `String "Test result: error"];
                  m()
	      | Error _, ErrorResult -> ()
	      | OK v, Result res -> 
		  let resv = compile_exp_tree cev res in
		    if not (V.equal v resv) then
		      test_error i 
			(fun () -> 
			   V.format_msg
			     [`String "expected:"; `Space;
			      `Tree resv;`Space;
			      `String "found:"; `Space;
			      `Tree v])
	      | Error m, Result res -> 
		  let resv = compile_exp_tree cev res in
		    test_error i 
		      (fun () -> 
			 V.format_msg
			   [`String "expected:"; `Space;
 			    `Tree resv; `Space;
			    `String "found error:"; `Space];
			 m ())
	
	      | OK v, ErrorResult -> 
		  test_error i 
		    (fun () -> 
		       V.format_msg
			 [`String "expected error, found"; `Break;
 			  `Tree v;])
	end;
      (cev, [])	
	  
and compile_module_aux cev m ds = Safelist.fold_left
  (fun (cev, names) di ->
     let new_cev, new_names = compile_decl cev m di in
       new_cev, names@new_names)
  (cev,[])
  ds

let compile_module m0 =
  (* let _ = debug (sprintf "compiling module %s" (string_of_module m0)) in *)
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let mq = qid_of_id m in
  let cev = CEnv.set_ctx (CEnv.empty ()) (nctx@Registry.pre_ctx) in
  let new_cev,_ = compile_module_aux cev mq ds in
    Registry.register_env (CEnv.get_ev new_cev) mq
	      
(* --------------- Exported functions --------------- *)

(* parse an AST from a lexbuf *)
let parse_lexbuf lexbuf = 
  try Parser.modl Lexer.main lexbuf 
  with Parsing.Parse_error ->
    parse_error (Lexer.info lexbuf) 
      (fun () -> Format.printf "@[syntax error@]")
      
let m_check n m_str ast= 
  if n = m_str then ()
  else
    sort_error 
      (info_of_module ast)
      (fun () -> Format.printf "@[module %s must appear in a file named %s.src or %s.fcl.@]"
	 m_str (String.uncapitalize m_str))
      
(* end-to-end compilation of files *)
let compile_lexbuf lexbuf n = 
  let ast = parse_lexbuf lexbuf in
  let m_str = string_of_id (id_of_modl ast) in 
  let _ = m_check n m_str ast in
  let ast' = check_module ast in 
  let _ = compile_module ast' in 
    ()

let compile_fcl fn n = 
  let fcl_buf = Src2fcl.fcl_of_src fn in 
  let _ = Lexer.setup fn in          
  let lexbuf = Lexing.from_string fcl_buf in 
  let _ = compile_lexbuf lexbuf n in
    Lexer.finish ()

let compile_fcl_str s n = 
  let _ = Lexer.setup "<string constant>" in    
  let lexbuf = Lexing.from_string (Src2fcl.fcl_of_src_str s) in
  let _ = compile_lexbuf lexbuf n in 
    Lexer.finish ()

let compile_src_str s n = compile_fcl_str (Src2fcl.fcl_of_src_str s) n

let compile_file fn n = compile_fcl fn n 
      
(* ugly hack!! this thunk forces loading of this module when used via
   harmony.cmxa (which is occasionally dynamically linked) *)
let init () = 
  Registry.compile_file_impl := compile_file;
  Registry.compile_fcl_str_impl := compile_fcl_str;
