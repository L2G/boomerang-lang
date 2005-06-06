(***********)
(* PRELUDE *)
(***********)

open Lens
open Registry
open Value
open Syntax

(* helper function for constructing a Focal type error *)
let focal_type_error msg = 
  raise (Error.Fatal_error (Printf.sprintf "run-time sort error in expression %s" msg))

(* standard tags for cons-cell encoded lists *)
let hd_tag = Value.N (V.hd_tag)
let _ = register_native "Native.Prelude.hd_tag" "name" hd_tag
let tl_tag = Value.N (V.tl_tag)
let _ = register_native "Native.Prelude.tl_tag" "name" tl_tag
let nil_tag = Value.N (V.nil_tag)
let _ = register_native "Native.Prelude.nil_tag" "name" nil_tag

(* these need to be baked in here, because the parser uses them *)
let cons =   
  let i = Info.bogus in    
    Value.T(Type.TT(Type.mk_ptype (Type.Fun(i,fun h -> Type.Fun(i,fun t -> Type.cons_it h t)))))
let _ = register_native "Native.Prelude.Cons" "type => type => type" cons
let nil = 
  let i = Info.bogus in
    Value.T (Type.TT(Type.mk_ptype (Type.nil_it)))
let _ = register_native "Native.Prelude.Nil" "type" nil

(*************)
(* UTILITIES *)
(*************)
let read fn = 
  let fn = match find_filename fn with
      None -> error [`String ("Native.Prelude.read: cannot locate " ^ fn)]
    | Some fn -> fn in
    try 
      Misc.read fn 
    with 
	_ -> raise (Error.Native_error ("Native.prelude.read: error reading " ^ fn))
    
(* read *)
let read_lib =
  F(function 
      | N fn -> N (read fn)
      | _ -> focal_type_error "Native.Prelude.read")
    
let _ = register_native "Native.Prelude.read" "name -> name" read_lib

(* load *)  
let load viewer s = 
  let ekey = Surveyor.get_ekey (Some viewer) "" (Some s) in
    (Surveyor.get_reader ekey) s

let load_lib =
  F (function
       | N s -> F (function 
		     | N viewer -> V (load viewer s)
		     | _ -> focal_type_error "Native.Prelude.load")
       | _ -> focal_type_error "Native.Prelude.load")

let _ = register_native "Native.Prelude.load" "name -> name -> view" load_lib

(* load_file *)
let load_file fn = 
  let (fn,ekeyo) = Surveyor.parse_filename fn in
  let contents = read fn in
  let ekey = 
    try 
      Surveyor.get_ekey ekeyo fn (Some contents) 
    with Error.Fatal_error m -> raise (Error.Native_error m)
  in     
    load ekey (contents)
      
let load_file_lib =
  F(function 
      | N fn -> V (load_file fn)
      | _ -> focal_type_error "Native.Prelude.load_file")
    
let _ = register_native "Native.Prelude.load_file" "name -> view" load_file_lib
  

(*************)
(* DEBUGGING *)
(*************)

(* PROBE *)
let probe msg = 
  { get = (fun c ->
     Format.printf "@,@[<v0>%s (get) @,  " msg;
     V.format c;
     Format.printf "@,@]";
     Format.print_flush ();
     c);
    put = (fun a co ->     
	     Format.printf "@,@[<v0>%s (put) @,  " msg;
	     V.format a;
     Format.printf "@,  ";
     begin
       match co with
           None -> Format.printf "MISSING";
         | Some c -> V.format c
     end;
     Format.printf "@,@]";
     Format.print_flush ();
     a)}
  
let probe_lib = 
  F(function 
      | N n -> L (probe n)
      | _ -> focal_type_error "Native.Prelude.probe")
    
let _ = register_native "Native.Prelude.probe" "name -> lens" probe_lib

(* TRACE *)
let trace msg = 
  { get = (fun c ->
	     Format.printf "%s (get)" msg;
	     Format.print_flush ();
	     c);
    put = (fun a co ->     
	     Format.printf "%s (put)" msg;
	     Format.print_flush ();
	     a)
  }
  
let trace_lib = 
  F(function 
      | N n -> L (trace n)
      | _ -> focal_type_error "Native.Prelude.trace")
    
let _ = register_native "Native.Prelude.trace" "name -> lens" trace_lib
	
(* TRACEPOINT *)
let tracepoint = Lens.tracepoint
let tracepoint_lib = 
  F(function 
      | N n -> F(function 
		   | L l -> L (tracepoint n l)
		   | _ -> focal_type_error "Native.Prelude.tracepoint")
      | _ -> focal_type_error "Native.Prelude.tracepoint")
    
let _ = register_native "Native.Prelude.tracepoint" "name -> lens -> lens" tracepoint_lib


(* INVERT *)
(* flip the direction -- only for bijective lenses! *)
let invert l = 
  { get = (fun c -> l.put c None);
    put = (fun a _ -> l.get a) }
    
let invert_lib = 
  F(function 
      | L l -> L (invert l)
      | _ -> focal_type_error "Native.Prelude.invert")
    
let _ = register_native "Native.Prelude.invert" "lens -> lens" invert_lib

let assert_native t = 
  let check_assert dir v t = 
    if not (Type.member v t) then
      error [`String ("Native.Prelude.assert(" ^ dir ^ "): view");
	     `View v;
	     `String "is not a member of "; `String (Type.string_of_t t)] 
  in          
    { get = ( fun c -> check_assert "get" c t; c);
      put = ( fun a _ -> check_assert "put" a t; a) }

let assert_lib = 
  F(function
	T t -> L (assert_native t) 
      | _ -> focal_type_error "Native.Prelude.assert")

let _ = register_native "Native.Prelude.assert" "type -> lens" assert_lib
      
(******************)
(* Generic Lenses *)
(******************)

(*** ID ***)
let id =
  { get = (fun c -> c);
    put = (fun a co -> a)}
let id_lib = L id
let _ = register_native "Native.Prelude.id" "lens" id_lib
	  
(*** CONST ***)
let const v d =
  { get = (fun c -> v);
    put = (fun a co ->
	     if V.equal a v then
	       match co with
		 | None -> d
		 | Some(c) -> c
	     else error [`String "Native.Prelude.const(put): abstract view";
			 `View a;
			 `String "is not equal to"; `View (v)]) }
let const_lib = F (function
		     | V v -> F (function 
				   | V d -> L (const v d)
				   | _ -> focal_type_error "Native.Prelude.const")
		     | _ -> focal_type_error "Native.Prelude.const")
		  
let _ = register_native "Native.Prelude.const" "view -> view -> lens" const_lib
	  
(*** COMPOSE2 ***)
let compose2 l1 l2 = 
  let l1 = memoize_lens l1 in
    { get = (fun c -> (l2.get (l1.get c)));
      put = (fun a co -> 
	       match co with
		 | None -> l1.put (l2.put a None) None
		 | Some c -> l1.put (l2.put a (Some (l1.get c))) co)}
      
let compose2_lib = 
  F (function
       | L l1 -> F (function
		      | L l2 -> L (compose2 l1 l2)
		      | _ -> focal_type_error "Native.Prelude.compose2")
       | _ -> focal_type_error "Native.Prelude.compose2")
    
let _ = register_native "Native.Prelude.compose2" "lens -> lens -> lens" compose2_lib

(********************)
(* Lenses for trees *)
(********************)
(*** MAP ***)
(* map - native interface *)
(* map can be implemented in terms of wmap, but it delays evaluation,
   of the sub-lens which is bad for memoization and causes too 
   much consing... *)
let map l = 
  { get = (fun c ->
	     let binds =
	       V.fold
		 (fun k ck bindacc ->
		    let ck' = (l.get ck) in
		    ((k,Some ck')::bindacc))
		 c [] in
	     V.create_star binds);
    put =
      (fun a co ->
         let c = match co with None -> V.empty | Some c -> c in
 	 let cbinds =
	   V.fold
	     (fun k vk bindacc ->
		(k,
		 begin match (V.get c k),(V.get a k) with
		   | Some ck, Some ak -> Some ((l.put ak (Some ck)))
		   | Some ck, None -> None
		   | None, Some ak -> Some ((l.put ak None))
		   | _ -> assert false 
		 end)::bindacc)
  	     a [] in
	   V.create_star cbinds)}
    
(* map - library interface *)
let map_lib = 
  F (function
       | L l -> L (map l)
       | _ -> focal_type_error "Native.Prelude.map")

let _ = register_native "Native.Prelude.map" "lens -> lens" map_lib
	  
(*** WMAP ***)
(* wmap - native interface *)
let wmap (l0 : Value.t -> Value.t) : (V.t, V.t) Lens.t = 
  let l =
    let memo = Hashtbl.create 11 in
      (fun k -> try Hashtbl.find memo k with
	   Not_found -> 
	     let res = match l0 (N k) with L l -> l | _ -> focal_type_error "Native.Prelude.wmap" in 
	       Hashtbl.add memo k res; res)
  in
    { get = (fun c ->
	       let binds =
		 V.fold
		   (fun k ck bindacc ->
		      let ck' = ((l k).get ck) in
			((k,Some ck')::bindacc))
		   c [] in
		 V.create_star binds);
      put =
	(fun a co ->
	   let c = match co with None -> V.empty | Some c -> c in
 	   let cbinds =
	     V.fold
	       (fun k vk bindacc ->
		  (k,
		   begin match (V.get c k),(V.get a k) with
		     | Some ck, Some ak -> Some ((l k).put ak (Some ck))
		     | Some ck, None -> None
		     | None, Some ak -> Some ((l k).put ak None)
		     | _ -> assert false
		   end)::bindacc)
  	       a [] in
	     V.create_star cbinds)}
      
(* wmap - library interface *)
let wmap_lib = 
  F (function
       | F m -> L (wmap m)
       | _ -> focal_type_error "Native.Prelude.wmap")
    
(* (\* wmap - unit tests *\) *)
(* let wmap_unit_tests =  *)
(*   let m = M (fun n -> if n = "y" then const a b else id) in *)
(*     [ test_get_eq [m] "{}" (\*=*\) "{}" *)
(*     ; test_get_eq [m] "{x={} y=[1 2 3]}" (\*=*\) "{x={} y={a={}}}" *)
(*     ; test_put_eq [m] "{}" None (\*=*\) "{}" *)
(*     ; test_put_eq [m] "{}" (Some "{x={} y=[1 2 3]}") (\*=*\) "{}" *)
(*     ; test_put_eq [m] "{x={} y={a={}}}" None (\*=*\) "{x={} y={b={}}}" *)
(*     ; test_put_eq [m] "{x={a={}}}" (Some "{x={} y=[1 2 3]}") (\*=*\) "{x={a={}}}" *)
(*     ; test_put_eq [m] "{y={a={}}}" (Some"{x={} y=[1 2 3]}") (\*=*\) "{y=[1 2 3]}" *)
(*     ; test_put_eq [m] "{y={a={}} z={c={}}}" (Some "{x={} y=[1 2 3]}") (\*=*\) "{y=[1 2 3] z={c={}}}" *)
(*     ] *)
      
let _ = register_native "Native.Prelude.wmap" "(name -> lens) -> lens" wmap_lib
  
(* XFORK *)
let xfork pcv pav l1 l2 =
  (* FIXME: check that pcv and pca have height <= 1? *)
  let dom_pcv = V.dom pcv in
  let dom_pav = V.dom pav in
  let pc = fun n -> Name.Set.mem n dom_pcv in
  let pa = fun n -> Name.Set.mem n dom_pav in
  { get = 
      (fun c ->
	 let c1,c2 = V.split pc c in
	 let a1 = l1.get c1 in
	 let a2 = l2.get c2 in
	   if not(Name.Set.for_all pa (V.dom a1)) then
	     error [`String "Native.Prelude.xfork(get): l1 yielded a child not ";
         	    `String "satisfying pa"; 
		    `View a1];
	   if not(Name.Set.for_all (fun k -> not (pa k)) (V.dom a2)) then
	     error [`String "Native.Prelude.xfork(get): l2 yielded a child satisfying pa";
		    `View a2];
	   V.concat a1 a2);
    put = (fun a co -> 
	     let co1,co2 =
	       match co with None -> None,None
		 | Some c -> let c1,c2 = V.split pc c in (Some c1,Some c2) in
	     let a1, a2 = V.split pa a in
	     let c1' = l1.put a1 co1 in
	     let c2' = l2.put a2 co2 in
	       if not(Name.Set.for_all pc (V.dom c1')) then
		 error [`String "Native.Prelude.xfork(put): l1 yielded a child ";
			`String "not satisfying pc"; 
			`View c1'];
	       if not(Name.Set.for_all (fun k -> not (pc k)) (V.dom c2')) then
		 error [`String "Native.Prelude.xfork(put): l2 yielded a child ";
			`String "satisfying pc"; 
			`View c2'];
	       V.concat c1' c2')}

let xfork_lib = 
  F(function V pcv -> F (
      function V pav -> F (
	function L l1 -> F (
	  function L l2 -> L (xfork pcv pav l1 l2)
	    | _ -> focal_type_error "Native.Prelude.xfork")
	  | _ -> focal_type_error "Native.Prelude.xfork")
	| _ -> focal_type_error "Native.Prelude.xfork")
      | _ -> focal_type_error "Native.Prelude.xfork")
          
let _ = register_native "Native.Prelude.xfork" "view -> view -> lens -> lens -> lens" xfork_lib
	  
(* HOIST *)
let hoist k =
  { get = 
      (fun c ->
	 if   (Name.Set.cardinal (V.dom c)) <> 1 
	   or (Name.Set.choose (V.dom c)) <> k then
	     error [`String "Native.Prelude.hoist (get): expecting exactly one child (named ";
		    `Name k; 
		    `String ")"; 
		    `View c];
	 V.get_required c k);
    put = 
      (fun a _ -> 
	 V.set V.empty k (Some a)) }

let hoist_lib = 
  F (function 
       |  N k -> L (hoist k)
       | _ -> focal_type_error "Native.Prelude.hoist")
    
let _ = register_native "Native.Prelude.hoist" "name -> lens" hoist_lib

(* PLUNGE *)
let plunge k =
  { get = (fun c -> V.set V.empty k (Some c));
    put = (fun a _ -> 
	     if   (Name.Set.cardinal (V.dom a)) <> 1 
	       or (Name.Set.choose (V.dom a)) <> k then
	     error [`String "Native.Prelude.plunge (put): expecting exactly one child (named ";
		    `Name k; 
		    `String ")"; 
		    `View a];
	     V.get_required a k)}
    
let plunge_lib = 
  F (function
       | N k -> L (plunge k)
       | _ -> focal_type_error "Native.Prelude.plunge")
    
let _ = register_native "Native.Prelude.plunge" "name -> lens" plunge_lib
	  
(******************)
(* Copy and Merge *)
(******************)
(* COPY *)
let copy m n =
  { get = 
      (fun c -> 
	 let child =
	   try V.get_required c m
	   with Not_found ->
	     error [`String "Native.Prelude.copy(get): expecting one child named ";
		    `Name m; 
		    `String ")"; 
		    `View c] in
	   V.set c n (Some child)) ;
    put = 
      (fun a _ -> 
	 if (try V.equal (V.get_required a m) (V.get_required a n)
	     with Not_found -> 
	       error [`String "Native.Prelude.copy(put): expecting two children named ";
		      `Name m; 
		      `String "and";
		      `Name n; 
		      `View a])
	 then V.set a n None
	 else 
	   error [`String "Native.Prelude.copy(put): expecting two equal children named ";
		  `Name m; 
		  `String " and ";
		  `Name n;
		  `View a]) }

let copy_lib =
  F (function 
       | N m -> F (function 
		       N n -> L (copy m n)
		     | _ -> focal_type_error "Native.Prelude.copy")
       | _ -> focal_type_error "Native.Prelude.copy")

let _ = register_native "Native.Prelude.copy" "name -> name -> lens" copy_lib

(* MERGE *)
let merge m n =
  { get = (fun c -> V.set c n None) ;
    put = 
      (fun a ->
	 function
	   | None -> 
	       (try V.set a n (Some (V.get_required a m))
		with Not_found -> 
		  error
		  [`String "Native.Prelude.merge(put): expecting a child named ";
		   `Name m])
	   | Some c ->
	       let cmo,cno = (V.get c m), (V.get c n) in
	       let eqCmCn = 
		 match cmo,cno with
		   | None, None -> true
		   | None, Some _ -> false
		   | Some _, None -> false
		   | Some cm, Some cn -> V.equal cm cn
	       in
		 if (eqCmCn) 
		 then V.set a n (V.get a m)
		 else V.set a n cno)
  }

let merge_lib =
  F (function 
       | N m -> F (function 
		       N n -> L (merge m n)
		     | _ -> focal_type_error "Native.Prelude.merge")
       | _ -> focal_type_error "Native.Prelude.merge")
    
let _ = register_native "Native.Prelude.merge" "name -> name -> lens" merge_lib

(****************)
(* Conditionals *)
(****************)
	    
(* COND *)
let cond_impl c a1 a2 f21o f12o lt lf =
  { get = 
      (fun cv ->
	 if Type.member cv c then lt.get cv
	 else lf.get cv);
    put = 
      (fun a co ->
	 if Type.member a a1 then
	   if Type.member a a2 then
	     match co with
	       | None -> lf.put a co
	       | Some cv ->
		   if Type.member cv c
		   then lt.put a co
		   else lf.put a co
	   else
	     match co with
	       | None -> lt.put a co
	       | Some cv ->
		   if Type.member cv c
		   then lt.put a co
		   else lt.put a (match f21o with 
				    | Some l -> (Some (l.get cv))
				    | None   -> None)
	 else
	   if Type.member a a2 then
	     match co with
	       | None -> lf.put a co
	       | Some cv ->
		   if Type.member cv c
		   then lf.put a (match f12o with 
				    | Some l -> (Some (l.get cv))
				    | None   -> None)
		   else lf.put a co
	   else error [
	     `String "Native.Prelude.cond (put): the abstract view does not satisfy a1 or a2:";
	     `View a ]
      )}

let cond_ff c a1 a2 f21 f12 lt lf = cond_impl c a1 a2 (Some f21) (Some f12) lt lf 
let cond_ww c a1 a2 lt lf = cond_impl c a1 a2 None None lt lf
let cond_fw c a1 a2 f21 lt lf = cond_impl c a1 a2 (Some f21) None lt lf
let cond_wf c a1 a2 f12 lt lf = cond_impl c a1 a2 None (Some f12) lt lf

let cond_ff_lib =
  F (function T c -> F (
       function T a1 -> F (
	 function T a2 -> F (
	   function L f21 -> F (
	     function L f12 -> F (
	       function L lt -> F (
		 function L lf -> L (cond_ff c a1 a2 f21 f12 lt lf)
		   | _ -> focal_type_error "Native.Prelude.cond")
		 | _ -> focal_type_error "Native.Prelude.cond")
	       | _ -> focal_type_error "Native.Prelude.cond")
	     | _ -> focal_type_error "Native.Prelude.cond")
	   | _ -> focal_type_error "Native.Prelude.cond")
	 | _ -> focal_type_error "Native.Prelude.cond")
       | _ -> focal_type_error "Native.Prelude.cond")


let _ = register_native
  "Native.Prelude.cond_ff"
  "type -> type -> type -> lens -> lens -> lens -> lens -> lens"
  cond_ff_lib

let cond_ww_lib =
  F (function T c -> F (
       function T a1 -> F (
	 function T a2 -> F (
	   function L lt -> F (
	     function L lf -> L (cond_ww c a1 a2 lt lf)
	       | _ -> focal_type_error "Native.Prelude.cond")
	     | _ -> focal_type_error "Native.Prelude.cond")
	   | _ -> focal_type_error "Native.Prelude.cond")
	 | _ -> focal_type_error "Native.Prelude.cond")
       | _ -> focal_type_error "Native.Prelude.cond")

let _ = register_native
  "Native.Prelude.cond_ww"
  "type -> type -> type -> lens -> lens -> lens"
  cond_ww_lib

let cond_fw_lib =
  F (function T c -> F (
       function T a1 -> F (
	 function T a2 -> F (
	   function L f21 -> F(
	     function L lt -> F (
	       function L lf -> L (cond_fw c a1 a2 f21 lt lf)
		 | _ -> focal_type_error "Native.Prelude.cond")
	       | _ -> focal_type_error "Native.Prelude.cond")
	     | _ -> focal_type_error "Native.Prelude.cond")
	   | _ -> focal_type_error "Native.Prelude.cond")
	 | _ -> focal_type_error "Native.Prelude.cond")
       | _ -> focal_type_error "Native.Prelude.cond")
    
let _ = register_native
  "Native.Prelude.cond_fw"
  "type -> type -> type -> lens -> lens -> lens -> lens"
  cond_fw_lib

let cond_wf_lib =
  F (function T c -> F (
       function T a1 -> F (
	 function T a2 -> F (
	   function L f12 -> F(
	     function L lt -> F (
	       function L lf -> L (cond_wf c a1 a2 f12 lt lf)
		 | _ -> focal_type_error "Native.Prelude.cond")
	       | _ -> focal_type_error "Native.Prelude.cond")
	     | _ -> focal_type_error "Native.Prelude.cond")
	   | _ -> focal_type_error "Native.Prelude.cond")
	 | _ -> focal_type_error "Native.Prelude.cond")
       | _ -> focal_type_error "Native.Prelude.cond")
    
let _ = register_native
  "Native.Prelude.cond_wf"
  "type -> type -> type -> lens -> lens -> lens -> lens"
  cond_wf_lib
          	  
(*************)
(* DATABASES *)
(*************)

(* PIVOT *)
let pivot k =
  { get = 
      (fun c ->
	   try 
	     let ck = V.get_required c k in
	     let ckv = V.get_value ck in
	       V.set V.empty ckv (Some (V.set c k None))
	   with Not_found -> 
	     error [`String "Native.Prelude.pivot(get): the following view should have ";
		    `String "exactly one child named "; 
		    `Name k; 
		    `String ", leading to a value ";
		    `View c]);
    put = 
      (fun a _ ->
	 if (Name.Set.cardinal (V.dom a)) <> 1 then
	   error [`String "Native.Prelude.pivot(get): the following view should have ";
		  `String "exactly one child"; 
		  `View a]
	 else
	   let ak = Name.Set.choose (V.dom a) in
	   let w = try V.get_required a ak with Not_found -> assert false in
	     if V.get w k <> None then
	       error [`String "Native.Prelude.pivot(put): child ";
		      `Name k;
		      `String "of this view should not exist: "; 
		      `View w]
	     else
	       V.set w k (Some (V.new_value ak))
      )} 

let pivot_lib = 
  F (function
       | N k -> L (pivot k)
       | _ -> focal_type_error "Native.Prelude.pivot")
    
let _ = register_native "Native.Prelude.pivot" "name -> lens" pivot_lib
  
(* JOIN *)
(* Dan Spoonhower's outer join *)
(* disclaimer: written down very quickly and directly.  trying to get
   it correct so using lots of lets and explicit match statements for
   readability.  let's clean it up later -nate
*)
let join m1 m2 = 
  { get = 
      (fun c ->
	 try
	   let rec compute_join b1 b2 acc = 
	     let bo = 
	       if (not (V.is_empty b1)) then (Some b1)
	       else if (not (V.is_empty b2)) then (Some b2)
	       else None 
	     in
	       match bo with
		 | None -> acc
		 | Some b -> 
		     let k = Name.Set.choose (V.dom b) in
		     let b1', b2' = (V.set b1 k None, V.set b2 k None) in
		     let tk1, tk2 = (V.get b1 k, V.get b2 k) in
		     let tk = 
		       match tk1, tk2 with
			 | None, None       -> assert false			     
			 | Some t1, None    -> V.set V.empty m1 (Some t1)
			 | None, Some t2    -> V.set V.empty m2 (Some t2)
			 | Some t1, Some t2 -> V.set (V.set V.empty m2 (Some t2)) m1 (Some t1)
		     in
		       compute_join b1' b2' (V.set acc k (Some tk))
	   in
	   let tm1, tm2 = (V.get_required c m1, V.get_required c m2) in	 
	     compute_join tm1 tm2 V.empty
	 with Not_found -> 
	   error [`String "Native.Prelude.join(get): expected view with children: "; 
		  `Name m1; 
		  `String " and "; 
		  `Name m2]
      );
    put = 
      (fun a co ->
	 let init = V.set (V.set V.empty m2 (Some V.empty)) m1 (Some V.empty) in
	 let rec compute_unjoin b acc =
	   if (V.is_empty b) then acc
	   else
	     try
	       let k = Name.Set.choose (V.dom b) in
	       let tk = V.get_required b k in
	       let b' = (V.set b k None) in
	       let cm1,cm2 = (V.get_required acc m1,V.get_required acc m2) in
	       let acc' = 
		 match (V.get tk m1, V.get tk m2) with
		   | None, None       -> 
		       error [`String "Native.Prelude.join(put): illformed abstract view"]
		   | Some t1, None    -> 
		       V.set acc m1 (Some (V.set cm1 k (Some t1)))
		   | None, Some t2    -> 
		       V.set acc m2 (Some (V.set cm2 k (Some t2)))
		   | Some t1, Some t2 -> 
		       V.set 
			 (V.set acc m2 (Some (V.set cm2 k (Some t2)))) 
			 m1 (Some (V.set cm1 k (Some t1)))
	       in
		 compute_unjoin b' acc'
	     with Not_found ->
	       error [`String "Native.Prelude.join(put): the impossible happened"] in
	   compute_unjoin a init 
      )
  }

let join_lib = 
  F (function 
       | N n1 -> 
	   F (function 
		| N n2 -> L (join n1 n2)
		| _ -> focal_type_error "Native.Prelude.join")
       | _ -> focal_type_error "Native.Prelude.join")
    
let _ = register_native "Native.Prelude.join" "name -> name -> lens" join_lib

(* FLATTEN *)
let flatten =   
  let rec get = function 
      c -> 
	if V.is_empty_list c then V.empty
	else 
	  (* Error handling in case of ill-formed list *)
	  let head = V.get_required c V.hd_tag in 
	  let c' = V.get_required c V.tl_tag in
	    (* List of labels pointing toward trees *)
	    if Name.Set.cardinal (V.dom head) = 1 then 
	      let c_list = V.to_list head in
	      let (k,d) = Safelist.hd c_list in
	      let a_rec = get c' in		
		(* removes spurious *nil labels in output *)
	      let a = if (V.is_empty_list a_rec) then V.empty else a_rec in 
	      let childk = V.get a k in
		begin
		  match childk with
		      None -> 
			V.set a k (Some (V.cons d V.empty_list))
		    | Some s -> 
			let a' = V.set a k None in
			  V.set a' k (Some (V.cons d s))
		end 
	    else error [`String "Native.Prelude.flatten(get): expected a view with exactly one child: "; 
			`View head]
  in
  let listify v =
    let v_list = V.to_list v in
    let listifystep (k,child) =
      List.map (function x -> (k,x)) (V.list_from_structure child)
    in
      Safelist.map (function (k,x) -> 
		      V.set V.empty k (Some x)) 
	(Safelist.flatten (Safelist.map listifystep v_list)) in
  let rec put a = function
      None -> V.structure_from_list (listify a)
    | Some c -> 
	if V.is_empty_list c then V.structure_from_list (listify a)
	else 
	  let head = V.get_required c V.hd_tag in
	    (* Error handling in case of ill-formed list *)
	  let c' = V.get_required c V.tl_tag in
	    (* List of labels pointing toward trees *)
	    if Name.Set.cardinal (V.dom head) = 1 then 
	      let c_list = V.to_list head in
	      let (k,d) = Safelist.hd c_list in
		match V.get a k with
		    None -> put a (Some c')
		  | Some ds -> 
		      (* Error handling in case of ill-formed list *)
		      let d' = V.get_required ds V.hd_tag in 
		      let s = V.get_required ds V.tl_tag in
			if V.is_empty_list s then
			  V.cons 
			    (V.from_list [k,d']) 
			    (put (V.set a k None) (Some c'))
			else 
			  V.cons 
			    (V.from_list [k,d']) 
			    (put (V.set (V.set a k None) k (Some s)) (Some c'))
	    else error [`String "Native.Prelude.flatten(put): expected a view with exactly one child: "; 
			`View head]
  in
    {get = get ;
     put = put }
      
let flatten_lib =  L flatten

let _ = register_native "Native.Prelude.flatten" "lens" flatten_lib

(************)
(* EXPLODE  *)
(************)
let explode =
  let rec tree_of_string msg = function
      "" -> []
    | s -> 
	let sh = String.sub s 0 1 and st = String.sub s 1 (String.length s - 1) in
	(V.new_value sh)::(tree_of_string msg st)
  and string_of_tree msg = function
      [] -> ""
    | a::q -> 
	if( Name.Set.cardinal (V.dom a)) <> 1 then
	  error [`String ("Native.Prelude.explode ("^msg^") : expecting exactly one child :");
		 `View a
		];
	let ch = Name.Set.choose (V.dom a) in
	  if String.length ch <> 1 then
	    error [`String ("Native.Prelude.explode (" ^ msg ^ ") : expecting child with a one character name");
		   `View a
		  ];
	  ch^(string_of_tree msg q)
  in
    { get =
	(fun c ->
	   if( Name.Set.cardinal (V.dom c)) <> 1 then
	     error [`String " Native.Prelude.explode (get) : expecting exactly one child :";
		    `View c];
	   let k = Name.Set.choose (V.dom c) in
	     (* here is the string we have to 'explode' *)
	     V.structure_from_list (tree_of_string "get" k)
	);
      put = (fun a _ -> V.new_value (string_of_tree "put" (V.list_from_structure a)))
    }
      
let explode_lib = L (explode)
    
let _ = register_native "Native.Prelude.explode" "lens" explode_lib
    
(*********)
(* SPLIT *)
(*********)
let split =
   { get = (fun c ->
              if not (V.is_value c) then
                error [`String ("Native.Prelude.split (get) : expecting exactly one child :"); `View c];
              V.structure_from_list (Safelist.map V.new_value (Misc.split_nonescape '\n' (V.get_value c))));
     put = (fun a c ->
             let lines = Safelist.map V.get_value (V.list_from_structure a) in
             if lines=[] then
               error [`String ("Native.Prelude.split (put) : abstract argument must be non-empty :"); `View a];
             Safelist.iter
               (fun s -> if String.contains s '\n' then
                           error [`String ("Native.Prelude.split (put): abstract view contains '\\n'"); `View a])
               lines;
             V.new_value (String.concat "\n" lines)) }
      
let split_lib = L (split)
    
let _ = register_native "Native.Prelude.split" "lens" split_lib
    
(* PAD *)
(* pad a list to a power of two *)
let pad n = 
  let nextPowerOf2 n = 
    let lg = (log (float_of_int n)) /. (log 2.0) in
      int_of_float (2.0 ** (ceil lg))
  in
  let pad_view = V.set V.empty n (Some V.empty) in
    { get = 
	(fun c -> 
	   let len = V.list_length c in
	   let rec add_pad v = function
	       0 -> v
	     | x -> add_pad (V.cons pad_view v) (x-1)
	   in
	     add_pad c (nextPowerOf2 len)
	);
      put = 
	(* FIXME: all the pads should be at the front of the list, 
	   but this primitive doesn't test that *)
	(fun a co ->
	   V.structure_from_list 
	     (Safelist.filter 
		(fun v -> not (V.equal pad_view v))
		(V.list_from_structure a))
	)
    }

let pad_lib = 
  F(function 
      | N n -> L (pad n)
      | _ -> focal_type_error "Native.Prelude.pad")
    
let _ = register_native "Native.Prelude.pad" "name -> lens" pad_lib
  
