(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007-2008                                                     *)
(* J. Nathan Foster and Benjamin C. Pierce                                     *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/compiler.ml                                                  *)
(* Boomerang type checker                                                      *)
(* $Id$ *)
(*******************************************************************************)

(* --------------- Imports --------------- *)
open Bsyntax
open Bident
open Benv
open Bprint
open Bsubst
open Berror
module L = Blenses.DLens
module C = Blenses.Canonizer
module V = Bvalue
module G = Bregistry
module H = Bheap

(* abbreviations *)
let sprintf = Printf.sprintf  
let msg = Util.format
let (@) = Safelist.append
let v_of_rv = G.value_of_rv 

(* helpers for constructing AST nodes *)
let mk_var i q = 
  EVar(i,q)

let mk_core_var i s = 
  mk_var i (Qid.mk_core_t s)

let mk_unit i = 
  EUnit(i)

let mk_int i n = 
  EInteger(i,n)

let mk_app i e1 e2 = 
  EApp(i,e1,e2)

let mk_app3 i e1 e2 e3 = 
  mk_app i (mk_app i e1 e2) e3

let mk_tyapp i e1 s2 = 
  ETyApp(i,e1,s2)

(* --------------------------------------------------------------------------- *)
(* SORT CHECKING *)

(* Generate an [Id.t] from [x] that is fresh for [s] by adding primes. *)
let rec fresh_id x s = 
  if Id.Set.mem x s then 
  fresh_id (Id.mk (Id.info_of_t x) (Id.string_of_t x ^ "'")) s
  else x

(* Generate an [Qid.t] from [x] that is fresh for [s] by adding primes. *)
let rec fresh_qid x s = 
  let q = Qid.t_of_id x in 
  if Qid.Set.mem q s then 
    fresh_qid (Id.mk (Id.info_of_t x) (Id.string_of_t x ^ "'")) s
  else q

(* Helpers for resolving types. Both return returns the
   fully-qualified name of the type (e.g., [List.t]), its sort
   variables (e.g., ['a]), and its constructors (e.g., [(List.Nil,
   None); (List.Cons, (a * List.t))]).

   o get_con lookups a type from a data type constructor (e.g.,
     [Nil]).

   o get_type is similar, but looks up the type from its name (e.g.,
     [List.t]).
*)
let get_con i sev li = 
  match SCEnv.lookup_con sev li with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ constructor@ %s@]" (Qid.string_of_t li))
    | Some r -> r

let get_type lookup_type i qx = 
  match lookup_type qx with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ type@ %s@]" (Qid.string_of_t qx))
    | Some r -> r

(* helper: substitute for variables in data type constructors *)
let inst_cases subst cl = 
  Safelist.map 
    (fun (li,so) -> (li,Misc.map_option (subst_sort subst) so)) cl

(* --------------------------------------------------------------------------- *)
(* COMPATIBILITY / CASTING *)

let no_alias = Prefs.createBool "no-alias" false
  "disable aliasing type checking/compilation (may degrade performance)"
  "disable aliasing type checking/compilation (may degrade performance)"

let ignore_refinements = Prefs.createBool "ignore-refinements" false
  "disable refinement type checking (dangerous if run with -no-type-check!)"
  "disable refinement type checking (dangerous if run with -no-type-check!)"

(* determine whether a term is (i.e., immediately compiles to) a value *)
let rec is_value e0 = match e0 with
  (* | EVar _ *) (* !!! this should be true, but our substitution based overloading uses variables and demands aliasing ... *)
  | EFun _ 
  | ETyFun _
  | ELoc _ 
  | EUnit _
  | EBoolean _
  | EInteger _
  | EChar _ 
  | EString _ 
  | ECSet _ -> true
  | EPair(_,e1,e2) -> is_value e1 && is_value e2
  | _ -> false

(* generate a new location, return a location binding and a reference to it *)
let mk_alias =
  let loc_ctr = ref 0 in
    fun i e0 ->
      if is_value e0 || Prefs.read no_alias
      then ([],e0)
      else 
	let l = !loc_ctr in
	incr loc_ctr;
	([(l,e0)],ELoc(i,l))

(* generate an allocation node from a list of location bindings *)
let mk_alloc i ls e0 =
  if ls = []
  then e0
  else EAlloc(i,ls,e0)

let rec compatible f t = match f,t with
  (* identity at base types *)
  | SUnit,SUnit       
  | SInteger,SInteger 
  | SBool,SBool       
  | SChar,SChar
  | SString,SString 
  | SRegexp,SRegexp 
  | SLens,SLens 
  | SCanonizer,SCanonizer ->
      true
  (* bool/cex coercions *)
  | SBool,SData([],q) when q = V.cex_qid -> true
  (* char/string/regexp/lens coercions *)
  | SChar,SString 
  | SChar,SRegexp
  | SChar,SLens
  | SString,SRegexp 
  | SString,SLens 
  | SRegexp,SLens -> 
      true
  | SFunction(_,s11,_,s12),SFunction(_,s21,_,s22) -> 
           (compatible s21 s11) (* contravariant in argument *)
        && (compatible s12 s22)
  | SProduct(s11,s12),SProduct(s21,s22) -> 
         compatible s11 s21
      && compatible s12 s22
  | SData(sl1,qx),SData(sl2,qy) -> 
      if Qid.equal qx qy then 
        (* and check that the sl1 and sl2 pairwise compatible *)
        let ok,sl12 = 
          try true, Safelist.combine sl1 sl2 
          with Invalid_argument _ -> (false,[]) in 
        Safelist.fold_left
          (fun b (s1i,s2i) -> b && compatible s1i s2i)
          ok sl12
      else 
        false
  | SVar x, SVar y -> 
      Id.equal x y
  | SForall(x,s1),SForall(y,s2) -> 
      if Id.equal x y then compatible s1 s2
      else   
        (* alpha vary f and t if needed *) 
        let f_fvs = free_sort_vars s1 in 
        let t_fvs = free_sort_vars s2 in 
        let z = fresh_id x (Id.Set.union f_fvs t_fvs) in
        let f_subst = [x,SVar z] in 
        let t_subst = [y,SVar z] in 
          compatible (subst_sort f_subst s1) (subst_sort t_subst s2)
  | SRefine(_,s11,_),s2 -> compatible s11 s2
  | s1,SRefine(_,s21,_) -> compatible s1 s21
  | _ -> false

let rec may_coerce f t =
  match f,t with
    | SBool,SData([],q)
    | SData([],q),SBool when q = V.cex_qid -> true
    | SChar,SString
    | SChar,SRegexp
    | SChar,SLens
    | SString,SRegexp
    | SString,SLens
    | SRegexp,SLens -> true
    | SProduct(f1,f2), SProduct(t1,t2) -> may_coerce f1 t1 || may_coerce f2 t2
    | SData(fl,x),SData(tl,y) when Qid.equal x y ->
	let rec aux acc l1 l2 = acc && match l1,l2 with
	  | [],[] -> false
	  | f::fl',t::tl' -> aux (may_coerce f t) fl' tl'
	  | _ -> false in
	aux false fl tl
    | SRefine(_,f1,_),_ -> may_coerce f1 t
    | _,SRefine(_,t1,_) -> may_coerce f t1
    | _,_ -> false

let rec trivial_cast f t =
  f == t ||
  syneq_sort f t ||
    match f,t with
      | SRefine(_,f',_),_ -> trivial_cast f' t (* out of a refinement *)
      | SFunction(x,f1,[],f2), SFunction(y,t1,[],t2) ->
	  trivial_cast t1 f1 &&
	  let f_fvs = free_exp_vars_in_sort f2 in
	  let t_fvs = free_exp_vars_in_sort t2 in
	  let qz = fresh_qid x (Qid.Set.union f_fvs t_fvs) in
	  let e_z = mk_var (Info.M "dummy info from trivial_cast") qz in
	  let f_subst = [Qid.t_of_id x,e_z] in
	  let t_subst = [Qid.t_of_id y,e_z] in
          trivial_cast (subst_exp_in_sort f_subst f2) (subst_exp_in_sort t_subst t2)
      | SProduct(f1,f2),SProduct(t1,t2) ->
	  trivial_cast f1 t1 && trivial_cast f2 t2
      | SForall(a,f'),SForall(b,t') -> 
          let f_fvs = free_sort_vars f in 
          let t_fvs = free_sort_vars t in 
          let z = fresh_id a (Id.Set.union f_fvs t_fvs) in
          let f_subst = [a,SVar z] in 
          let t_subst = [b,SVar z] in 
	  trivial_cast (subst_sort f_subst f') (subst_sort t_subst t')
      | SData(fl,x),SData(tl,y) ->
	  let rec all_trivial acc fl tl =
	    acc && match fl,tl with
		f::fl',t::tl' -> all_trivial (trivial_cast f t) fl' tl'
	      | [],[] -> true
	      | _,_ -> false
	  in
	    all_trivial (Qid.equal x y) fl tl
      | _ -> false
        
let rec mk_cast s i f t e = 
  if Prefs.read ignore_refinements
  then 
    if may_coerce f t
    then ECast(i,f,t,mk_blame i,e)
    else e
  else
    if trivial_cast f t
    then e
    else ECast(i,f,t,mk_blame i,e)

(* static_match: determine if a value with a given sort *could* match
   a pattern; annotate PVars with their sorts, return the list of sort
   bindings for variables. *)
let rec static_match i sev p0 s = 
(*   msg "STATIC_MATCH: %s # %s@\n" (string_of_pat p0) (string_of_sort s); *)
(*   msg "LOOKING UP %s@\n" (Qid.string_of_t li);         *)
(*   msg "QX: %s SVL: %t@\n" (Qid.string_of_t qx) (fun _ -> Misc.format_list "," (format_svar false) svl); *)
(*   msg "INSTANTIATED DATA: %s@\n" (string_of_sort s_expect); *)
  let err p str s = sort_error i 
    (fun () -> msg "@[in@ pattern@ %s:@ expected %s,@ but@ found@ %s@]"
       (string_of_pat p) str (string_of_sort s)) in 
    match p0 with 
      | PWld _ -> 
          Some (p0,[])
      | PVar(i,x,_) -> 
          Some (PVar(i,x,Some s),[(x,s)])
      | PUnt(i) ->
          if not (compatible s SUnit) then err p0 "unit" s;
          Some (p0,[])
      | PInt _ -> 
          if not (compatible s SInteger) then err p0 "int" s;
          Some (p0,[])
      | PBol _ -> 
          if not (compatible s SBool) then err p0 "bool" s;
          Some (p0,[])
      | PStr _ -> 
          if not (compatible s SString) then err p0 "string" s;
          Some (p0,[])
      | PVnt(pi,li,ptio) -> 
          begin match expose_sort s with 
            | SData(sl1,qy) -> 
                (* lookup the constructor from the environment *)
                let qx,(svl,cl) = get_con i sev li in 
                let cl_inst = 
                  if not (Qid.equal qx qy) then     
                    let s_expect = SData(sl_of_svl svl,qx) in 
                    err p0 (string_of_sort s_expect) s
                  else 
                    let subst = Safelist.combine svl sl1 in 
                    inst_cases subst cl in 
                let rec resolve_label li lj = function
                  | [] -> None
                  | o::os -> 
                      let qi = Qid.t_dot_t o li in 
                        if Qid.equal qi lj then Some qi
                        else resolve_label li lj os in
                let rec find_label = function
                  | [] -> None
                  | (lj,sjo)::rest ->                            
                      let go qi = 
                        match ptio,sjo with 
                          | None,None -> 
                              Some(PVnt(pi,qi,ptio),[])
                          | Some pti,Some sj -> 
                              begin 
                                match static_match i sev pti sj with
                                  | Some (new_pti,binds) -> 
                                      Some (PVnt(pi,qi,Some new_pti),binds)
                                  | None -> find_label rest
                              end
                          | _ -> sort_error i 
                              (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                                 (Qid.string_of_t lj)) in 
                        if Qid.equal li lj then go li
                        else match resolve_label li lj (SCEnv.get_ctx sev) with
                          | None -> find_label rest
                          | Some qi -> go qi in 
                  find_label cl_inst                
            | _ -> err p0 "data type" s
          end
            
      | PPar(pi,p1,p2) -> 
          begin match s with 
            | SProduct(s1,s2) -> 
                begin match
                  static_match i sev p1 s1, 
                  static_match i sev p2 s2 
                with 
                  | Some (new_p1,l1),Some(new_p2,l2) -> 
                      Some (PPar(pi,new_p1,new_p2),l1 @ l2)
                  | _ -> None 
                end
            | _ -> err p0 "product" s
          end

let s_cex = SData([], V.cex_qid)

(* ------ sort resolution and compilation ----- *)
(* check_sort: resolves QIds in SData, compiles terms in SRefine *)
let rec check_sort i sev s0 = 
  let rec go s0 = match s0 with 
    | SUnit | SBool | SInteger | SChar | SString 
    | SRegexp | SLens | SCanonizer | SVar _ -> 
        s0
    | SFunction(x,s1,[],s2) -> 
        let new_s1 = go s1 in 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort new_s1) in 
        let new_s2 = check_sort i sev1 s2 in  
          SFunction(x,new_s1,[],new_s2)
    | SFunction(_,_,ls,_) ->
	sort_error i
	  (fun () ->
	     msg "@[unexpected@ non-empty@ locations@ in@ source@ sort@ ";
	     format_sort s0;
	     msg "@]")
    | SProduct(s1,s2)  -> 
        SProduct(go s1,go s2)
    | SData(sl,qx) ->        
        let qx',_ = match SCEnv.lookup_type sev qx with 
          | None -> 
	      sort_error i  
                (fun () -> msg "@[cannot@ resolve@ sort@ %s@]" 
                   (Qid.string_of_t qx))
          | Some q -> q in 
          SData(Safelist.fold_left (fun acc si -> go si::acc) [] sl,qx')
    | SForall(x,s1) -> SForall(x,go s1)
    | SRefine(x,s1,e1) -> 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort (erase_sort s1)) in 
        let s1' = check_sort i sev1 s1 in 
        let sev2 = SCEnv.update sev (Qid.t_of_id x) (G.Sort s1') in 
        let e1_sort,new_e1,e1_ls = check_exp sev2 e1 in  
        if not (compatible e1_sort s_cex) then
          sort_error i 
	    (fun () -> msg "@[in@ refinement: expected@ %s@ but@ found@ %s@]"
               (string_of_sort s_cex)
               (string_of_sort e1_sort));
	let alloc_e1 = mk_alloc (info_of_exp new_e1) e1_ls new_e1 in
	let cexed_e1 = mk_cast "refinement" i e1_sort s_cex alloc_e1 in
          SRefine(x,s1',cexed_e1) in 
    go s0

(* ------ sort checking and compiling expressions ----- *)
and check_exp_app i sev (e1_sort,new_e1,e1_ls) (e2_sort,new_e2,e2_ls) =
  match expose_sort e1_sort with 
    | SFunction(x,param_sort,ls,return_sort) -> 
        let i2 = info_of_exp new_e2 in 
          if not (compatible e2_sort param_sort) then 
            sort_error i2
	      (fun () ->                    
                 msg "@[in@ application:@ expected@ %s@ but@ found@ %s@]"
                   (string_of_sort param_sort)
                   (string_of_sort e2_sort));
          (* construct cast *)
          let cast_e2 = mk_cast "application argument" i2 e2_sort param_sort new_e2 in
            (* two cases: simple arrows (represented by Id.wild) and dependent arrows *)
            if Id.equal x Id.wild then
	      let new_e0 = EApp(i,new_e1,cast_e2) in 
	      let e0_sort = return_sort in
		(Trace.debug "alias+" (fun () -> msg "@[UNALIASED:@ %a : %a@]@\n" (fun _ -> format_exp) new_e0 (fun _ -> format_sort) e0_sort));
		(e0_sort,new_e0,e1_ls@e2_ls@ls)
            else
	      let e2_loc,aliased_e2 = mk_alias i2 cast_e2 in
	      let new_e0 = EApp(i,new_e1,aliased_e2) in
	      let e0_sort = subst_exp_in_sort [(Qid.t_of_id x,aliased_e2)] return_sort in
		(Trace.debug "alias" (fun () -> msg "@[ALIASED:@ %a : %a@]@\n" (fun _ -> format_exp) new_e0 (fun _ -> format_sort) e0_sort));
		(*                           msg "@[IN APP: "; format_exp e0; msg "@]@\n"; *)
		(*                           msg "@[E1_SORT: %s@\n@]" (string_of_sort e1_sort); *)
		(*                           msg "@[E2_SORT: %s@\n@]" (string_of_sort e2_sort); *)
		(*                           msg "@[RESULT: %s@\n@]" (string_of_sort e0_sort); *)
		(e0_sort,new_e0,e1_ls@e2_ls@e2_loc@ls)
    | _ -> 
        sort_error (info_of_exp new_e1)
          (fun () ->              
             msg "@[in@ application:@ expected@ function@ sort@ but@ found@ %s.@]"
	       (string_of_sort e1_sort))

and check_exp sev e0 = 
  match e0 with
    | EVar(i,q) ->
        (* lookup q in the environment *)
        let e0_sort = match SCEnv.lookup sev q with
          | Some (G.Sort s) -> 
	      (* if it is bound, return the sort *)
	      s

	      (* otherwise raise an appropriate exception *)
	  | Some (G.Unknown) ->
	      sort_error i
                (fun () -> msg "@[%s is bound but of unknown type; checking failed@]" 
                   (Qid.string_of_t q))
	      
          | None -> 
	      sort_error i
                (fun () -> msg "@[%s is not bound@]" 
                   (Qid.string_of_t q)) in 
          (e0_sort,e0,[])

    | EOver(i,op,es) -> begin 
        let err () = sort_error i 
          (fun () -> msg "@[could@ not@ resolve@ %s@]" (string_of_op op)) in 
        (* rules for overloaded symbols *)
	let bin_rules =
	  [ ODot, 
	    [ SString, "string_concat";
	      SRegexp, "regexp_concat";
	      SLens, "lens_concat";
	      SCanonizer, "canonizer_concat" ]
	  ; OTilde,
	    [ SLens, "lens_swap";
	      SCanonizer, "canonizer_swap" ] 
	  ; OMinus,
	    [ SRegexp, "diff";
	      SInteger, "minus" ]
	  ; OBar,
	    [ SRegexp, "regexp_union";
	      SLens, "lens_disjoint_union";
	      SCanonizer, "canonizer_union" ]
	  ; OAmp,    [ SRegexp, "inter" ]
	  ; OBarBar, [ SLens, "lens_union";
		       SBool, "lor";
		       s_cex, "cor";
		     ]
	  ; OAmpAmp, [ SBool, "land";
		       s_cex, "cand"
		     ]
	  ; ODarrow, [ SLens, "set" ]
	  ; ODeqarrow, [ SLens, "rewrite" ]
	  ; OLt, [ SInteger, "blt" ] 
	  ; OLeq, [ SInteger, "bleq" ] 
	  ; OGt, [ SInteger, "bgt" ] 
	  ; OGeq, [ SInteger, "bgeq" ] ] in
        (* helper to find rule *)
        let rec find_rule r rs = match r with 
          | [] -> None
          | (s,op_name)::t -> 
	      if Safelist.for_all (fun (si,_,_) -> compatible si s) rs then 
                Some op_name
	      else 
                find_rule t rs in 
        (* type check and instrument es *)
        let rs = 
          Safelist.fold_right
            (fun ei rs -> 
	       let ri = check_exp sev ei in 
                 ri::rs)
            es [] in
        (* rewrite the overloaded symbol using the rules above; iter is treated specially *)
        let (op_s,op_e,op_ls) = match op,rs with
            | OIter(min,max),[r1] ->
		let (e1_sort,_,_) = r1 in
		let mk_iter iter_id =
		  let r_min  = check_exp sev (mk_int i min) in
		  let r_max  = check_exp sev (mk_int i max) in
		  let r_iter = check_exp sev (mk_core_var i iter_id) in
		    check_exp_app i sev (check_exp_app i sev (check_exp_app i sev r_iter r1) r_min) r_max
		in
		if compatible e1_sort SRegexp
		then mk_iter "regexp_iter"
		else if compatible e1_sort SLens
		then
		  let checked_app f_name =
		    let r_f = check_exp sev (EVar(i,Qid.mk_core_t f_name)) in
		      check_exp_app i sev r_f r1
		  in
		  match min,max with
		      (* l{0,0} = copy [] *)
		      (0,0) -> check_exp sev (EApp(i,mk_core_var i "copy",mk_core_var i "EPSILON"))
		    | (0,1) -> checked_app "lens_option"
		    | (0,-1) -> checked_app "lens_star"
		    | (1,-1) -> checked_app "lens_plus"
		    | _ -> mk_iter "lens_iter"
		else if compatible e1_sort SCanonizer
		then mk_iter "canonizer_iter"
		else err ()
            | OEqual,[e1_sort,e1,e1_ls; _,e2,e2_ls] ->
                (SBool,
                 mk_app3 i 
                   (mk_tyapp i (mk_var i (Qid.mk_core_t "equals")) e1_sort) 
                   e1 e2,
		 e1_ls@e2_ls)
            | op,[r1; r2] -> begin
                let rules = try Safelist.assoc op bin_rules with _ -> err () in 
                  match find_rule rules rs with 
                    | Some op_id ->
			let r_op = check_exp sev (mk_core_var i op_id) in
			  check_exp_app i sev (check_exp_app i sev r_op r1) r2
		    | None -> err ()
	      end
            | _ -> err ()
	in
	  Trace.debug "over+" (fun () -> msg "@[@ %a@\n\t~~>@\n %a@]@\n" (fun _ -> format_exp) e0 (fun _ -> format_exp) (mk_alloc i op_ls op_e));
	  (op_s,op_e,op_ls)
      end

    | EFun(i,Param(p_i,p_x,p_s),ret_sorto,body) ->
        (* resolve the parameter sort *)
        let new_p_s = check_sort p_i sev p_s in 
          (* create the environment for the body *)
        let body_sev = SCEnv.update sev (Qid.t_of_id p_x) (G.Sort new_p_s) in
        let new_ret_sorto,(body_sort,new_body,body_ls) = 
          match ret_sorto with 
            | None -> 
                (* if no return sort declared, just check the body *)
                None,(check_exp body_sev body)
            | Some ret_sort ->
                (* otherwise, resolve the declared return sort *)
                let new_ret_sort = check_sort i body_sev ret_sort in 
                  (* then check the body *)
                let body_sort,new_body,body_ls = check_exp body_sev body in
                  (* and check that the declared return sort is a subsort of the body sort *)
                  if not (compatible body_sort new_ret_sort) then
                    sort_error i
		      (fun () ->
                         msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                           (string_of_sort new_ret_sort)
                           (string_of_sort body_sort));
                  let cast_body = mk_cast "function body" (info_of_exp body) body_sort new_ret_sort new_body in
                    (Some new_ret_sort,(new_ret_sort,cast_body,body_ls)) in 
	  (* make the return type dependent if we need to *)
        let dep_x = 
          if Qid.Set.mem (Qid.t_of_id p_x) (free_exp_vars_in_sort body_sort) 
	  then p_x
          else Id.wild in
        (* cull unnecessary locs before allocation -- note that we must take the transitive closure to be safe 

	   TODO this should be converted to sets -- it'll be faster; we need to convert free_locs_*, too, though
	*)
	let rec close needed ls =
	  if needed = []
	  then []
	  else
	    let needed' = Safelist.concat (Safelist.map (fun (li,ei) -> free_locs_exp ei) ls) in
	    let all = needed' @ needed in
	    let novel = Safelist.filter (fun l -> not (Safelist.mem l all)) needed in
	    let closure = close novel ls in
	      all @ closure
	in
	let needed_ls = close (free_locs_sort body_sort) body_ls in
	let alloc_ls = Safelist.filter (fun (li,ei) -> Safelist.mem li needed_ls) body_ls in
        let e0_sort = 
            SFunction(dep_x,new_p_s,alloc_ls (* MK_ALLOC *),body_sort) in
          (*       Trace.debug "dep" *)
          (*         (fun () -> *)
          (*            msg "FVS: {%s}@\n" (Misc.concat_list "," (Safelist.map Qid.string_of_t (Qid.Set.elements (free_exp_vars_in_sort body_sort)))); *)
          (*            msg "P_X: %s@\n" (Id.string_of_t p_x); *)
          (*            msg "RES: %s@\n" (string_of_sort e0_sort)); *)
        let new_p = Param(p_i,p_x,new_p_s) in
	let b_i = info_of_exp new_body in
        let new_e0 = EFun(i,new_p,new_ret_sorto,mk_alloc b_i body_ls new_body) in
          (e0_sort,new_e0,[])

    | ELet(i,b,e) ->
        (* for let-expressions, check the bindings *)
        let bevs,xs,Bind(new_bi,new_bp,new_bso,new_be),b_ls = check_binding sev b in
          (* alias the argument term, since it will get put in the type, as well *)
	let (b_loc,aliased_be) = mk_alias new_bi new_be in
          (* use the resulting environment to check the exp *)
        let e_sort,new_e,e_ls = check_exp bevs e in
	  (* allocate for e, simulating lets-as-lambdas *)
	let alloc_e = mk_alloc (info_of_exp new_e) e_ls new_e in
          (* put in the aliased bound-term and allocated inner term *)
        let new_e0 = ELet(i,Bind(new_bi,new_bp,new_bso,aliased_be),alloc_e) in 
          (* substitute in the sort *)
        let e_sort_subst = match new_bp with 
          | PVar(_,x,_) -> subst_exp_in_sort [(Qid.t_of_id x,aliased_be)] e_sort 
          | _ -> 
	      let e_sort_fvs = free_exp_vars_in_sort e_sort in 
	      let () = Safelist.iter 
                (fun qi -> 
                   if Qid.Set.mem qi e_sort_fvs then 
                     sort_error i (fun () -> msg "@[illegal@ dependent@ let-binding@]"))
                xs in 
		e_sort in
          (e_sort_subst,new_e0,b_ls@b_loc)

    | EPair(i,e1,e2) -> 
        (* for pairs, recursively check e1 and e2 *)
        let e1_sort,new_e1,e1_ls = check_exp sev e1 in 
        let e2_sort,new_e2,e2_ls = check_exp sev e2 in 
        let e0_sort = SProduct(e1_sort,e2_sort) in 
        let new_e0 = EPair(i,new_e1,new_e2) in 
          (e0_sort,new_e0,e1_ls@e2_ls)

    | EUnit(_) -> 
        (* units have sort SUnit *)
        (SUnit,e0,[])

    | EBoolean(_) -> 
        (* boolean constants have sort SBool *)
        (SBool,e0,[])

    | EInteger(_) -> 
        (* integer constants have sort SInteger *)
        (SInteger,e0,[])

    | EChar(_) -> 
        (SChar,e0,[])

    | EString(_) -> 
        (* string constants have sort SString *)
        (SString,e0,[])

    | ECSet(_) -> 
        (* character sets have sort SRegexp *)
        (SRegexp,e0,[])

    | EApp(i,e1,e2) ->
        let r1 = check_exp sev e1 in 
	let (e1_sort,_,_) = r1 in
	(Trace.debug "alias+" (fun () -> msg "@[EApp arg1:@ %a : %a@]@\n" (fun _ -> format_exp) e1 (fun _ -> format_sort) e1_sort));
        let r2 = check_exp sev e2 in 
	  check_exp_app i sev r1 r2

    | ECase(i,e1,pl,ps) -> 
        (* helper function for printing error messages *)
        let err2 i p s1 s2 = sort_error i (fun () -> msg p s1 s2) in 
          (* resolve the sort *)
        let new_ps = check_sort i sev ps in 
          (* check the expression being matched *)
        let e1_sort,new_e1,e1_ls = check_exp sev e1 in 
          (* fold over the list of patterns and expressions *)
        let new_pl_rev,new_p_ls = Safelist.fold_left 
          (fun (new_pl_rev,new_p_ls) (pi,ei) -> 
             match static_match i sev pi e1_sort with 
	       | None -> 
                   (* if the branch is useless, raise an exception *)
                   err2 i "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
                     (string_of_pat pi) 
                     (string_of_sort e1_sort)
	       | Some (new_pi,binds) ->                
                   (* otherwise, extend the environment with bindings for pattern vars *)
                   let ei_sev = Safelist.fold_left 
                     (fun ei_sev (qj,sj) -> SCEnv.update ei_sev (Qid.t_of_id qj) (G.Sort sj))
                     sev binds in 
                     (* sort check the expression *) 
                   let ei_sort,new_ei,ei_ls = check_exp ei_sev ei in
                     (* and check that it is compatible with the sorts of the other branches *)
                   let ii = info_of_exp ei in 
                     if not (compatible ei_sort new_ps) then 
		       err2 ii                     
                         "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                         (string_of_sort new_ps)
                         (string_of_sort ei_sort);
                     let cast_ei = mk_cast "match branch" ii ei_sort new_ps new_ei in
		       ((new_pi,cast_ei)::new_pl_rev,ei_ls@new_p_ls))
          ([],[]) pl in 
          if Safelist.length new_pl_rev = 0 then 
            sort_error i (fun () -> msg "@[empty@ match@ expression@]");
          let new_e0 = ECase(i,new_e1,Safelist.rev new_pl_rev,new_ps) in 
            (new_ps,new_e0,e1_ls@new_p_ls)

    | ETyFun(i,x,e1) -> 
        let e1_sort,new_e1,e1_ls = check_exp sev e1 in 
        let new_e0 = ETyFun(i,x,new_e1) in 
        let e0_sort = SForall(x,e1_sort) in 
          (e0_sort,new_e0,e1_ls)

    | ETyApp(i,e1,s2) -> 
        let e1_sort,new_e1,e1_ls = check_exp sev e1 in 
        let new_s2 = check_sort i sev s2 in 
        let e0_sort = match e1_sort with 
          | SForall(x,s11) -> subst_sort [x,new_s2] s11
          | _ -> sort_error i
	      (fun () -> msg "@[in@ type@ application:@ expected@ universal@ type@ but@ %s@ found.@]"
                 (string_of_sort e1_sort)) in 
        let new_e0 = ETyApp(i,new_e1,new_s2) in 
          (e0_sort,new_e0,e1_ls)

    | ECast(i,f,t,b,e) -> sort_error i (fun () -> msg "@[unexpected@ cast@ expression@ in@ source@ term@]")

    | ELoc(i,l) -> sort_error i (fun () -> msg "@[unexpected@ location@ reference@ in@ source@ term@]")

    | EAlloc(i,ls,e1) -> sort_error i (fun () -> msg "@[unexpected@ allocation@ in@ source@ term@]")

and check_binding sev b0 = match b0 with
  | Bind(i,p,so,e) ->
      let e_sort,new_e,e_ls = check_exp sev e in        
      let new_s,cast_e = match so with 
        | None -> (e_sort,new_e)
        | Some s -> 
            let new_s = check_sort i sev s in
	      if not (compatible e_sort new_s) then 
                sort_error i
                  (fun () ->
                     msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
		       (string_of_sort new_s)
		       (string_of_sort e_sort));
	      let cast_e = mk_cast "let binding" (info_of_exp e) e_sort new_s new_e in 
                (new_s,cast_e) in 
      let new_p,xs,bsev = match static_match i sev p new_s with 
        | None -> 
            sort_error i 
	      (fun () -> 
                 msg "@[in@ let-binding:@ %s@ does not match@ %s@]"
                   (string_of_pat p)
                   (string_of_sort new_s))
        | Some(new_p,binds) ->             
            let xs_rev,bsev = Safelist.fold_left 
	      (fun (xsi,sevi) (xj,sj) -> 
                 let qj = Qid.t_of_id xj in 
                   (qj::xsi,SCEnv.update sevi qj (G.Sort sj)))
	      ([],sev) binds in
	      (new_p,Safelist.rev xs_rev,bsev) in 
      let new_b = Bind(i,new_p,Some new_s,cast_e) in 
        (bsev,xs,new_b,e_ls)

(* type check a single declaration *)
let rec check_decl sev ms d0 = 
  let res = match d0 with
    | DLet(i,b) ->
	let bsev,xs,Bind(b_i,b_p,b_so,b_e),b_ls = check_binding sev b in
	let alloc_be = mk_alloc b_i b_ls b_e in
	let new_b = Bind(b_i,b_p,b_so,alloc_be) in
	let new_d = DLet(i,new_b) in
	  (bsev,xs,new_d)
    | DMod(i,n,ds) ->
	let qmn = Qid.t_dot_id (SCEnv.get_mod sev) n in 
	let msev = SCEnv.set_mod sev qmn in 
	let ms = ms @ [n] in 
	  (* check the module *)
	let msev,names,new_ds = check_module_aux msev ms ds in
	let nsev, names_rev = Safelist.fold_left 
          (fun (nsev, names) q -> 
             match SCEnv.lookup msev q with
		 None -> run_error i
                   (fun () -> msg "@[declaration@ for@ %s@ missing@]"
		      (Qid.string_of_t q))
	       | Some s ->
                   (* prefix the qualifiers in each name with n *)
                   let nq = Qid.splice_id_dot n q in
                     (SCEnv.update nsev nq s, nq::names))
          (msev,[]) names in 
	let new_d = DMod(i,n,new_ds) in 
	  (nsev,Safelist.rev names_rev,new_d)

    | DType(i,svl,qx,cl) -> 
	(* get module prefix *)
	let qm = SCEnv.get_mod sev in       
	let new_qx = Qid.t_dot_t qm qx in      
	  (* install a dummy for qx in environment *)
	let sev2 = SCEnv.update_type sev svl new_qx [] in 
	  (* then resolve sorts in cl *)
	let new_cl_rev = 
          Safelist.fold_left  
            (fun acc (x,so) -> 
	       let x_so' = match so with 
		 | None -> (x,so)
		 | Some s -> (x,Some (check_sort i sev2 s)) in 
		 x_so'::acc)
            [] cl in 
	let new_cl = Safelist.rev new_cl_rev in 
	let new_qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) new_cl in
	  (* put the real qx in environment -- note! must discard dummy sev2 *)
	let sev3 = SCEnv.update_type sev svl new_qx new_qcl in
	  (* build the datatype *)
	let sx = SData(sl_of_svl svl,new_qx) in
	let mk_univ s = Safelist.fold_right (fun svi acc -> SForall(svi,acc)) svl s in 
	  (* add constructors to sev *)
	let sev4,xs_rev = Safelist.fold_left 
          (fun (sevi,acc) (li,sio) ->            
             let li_sort = match sio with 
	       | None -> mk_univ sx
	       | Some si -> mk_univ (SFunction(Id.wild,si,[],sx)) in 
             let qli = Qid.t_of_id li in 
	       (SCEnv.update sevi qli (G.Sort li_sort),qli::acc))
          (sev3,[]) new_cl_rev in
	let new_d = DType(i,svl,new_qx,Safelist.rev new_cl_rev) in 
	  (sev4,Safelist.rev xs_rev,new_d)
            
    | DTest(i,e1,tr) -> 
	(* check the expression *)
	let e1_sort,new_e1,e1_ls = check_exp sev e1 in
	let alloc_e1 = mk_alloc (info_of_exp new_e1) e1_ls new_e1 in
	let new_tr = match tr with 
          | TestError | TestPrint -> tr
          | TestEqual e2 -> 
	      (* for values, check that the exps have compatible types *)
	      let e2_sort,new_e2,e2_ls = check_exp sev e2 in 
	      let e2_i = info_of_exp e2 in
		if not (compatible e2_sort e1_sort) then
		  sort_error e2_i
                    (fun () -> 
		       msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
			 (string_of_sort e1_sort)
			 (string_of_sort e2_sort));
		TestEqual (mk_alloc e2_i e2_ls new_e2)
          | TestSortPrint _ -> TestSortPrint (Some e1_sort)
          | TestSortEqual s -> 
	      let s' = check_sort i sev s in 
		TestSortEqual s' in            
	let new_d = DTest(i,alloc_e1,new_tr) in 
	  (sev,[],new_d) in 
  let _,_,new_d0 = res in 
    Trace.debug "check"
      (fun () -> 
	 msg "@[";
	 format_decl d0;
	 msg "@\n~~>@\n";
	 format_decl new_d0;
	 msg "@]@\n");
    res

      
and check_module_aux sev m ds = 
  let msev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sevi, names, new_ds_rev) di -> 
         let dsev,new_names,new_di = check_decl sevi m di in
           dsev,names@new_names,new_di::new_ds_rev)
      (sev,[],[]) ds in 
    (msev, names, Safelist.rev new_ds_rev)

(* entry point to static analysis / instrumentation *)
let check_module m0 = match m0 with 
  | Mod(i,m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let sev = SCEnv.set_ctx (SCEnv.empty qm) (qm::nctx@G.pre_ctx) in
      let checked_sev,_,checked_ds = check_module_aux sev [m] ds in 
      let res = Mod(i,m,nctx,checked_ds) in 
	Trace.debug "instr+" (fun () -> format_module res; msg "@\n");
	res

