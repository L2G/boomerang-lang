(***********)
(* PRELUDE *)
(***********)

open Lens
open Registry
open Value
open Syntax

let (@) = Safelist.append

(* standard tags for cons-cell encoded lists *)
let hd_tag = V.hd_tag
let hd_tag_lib = Value.N (hd_tag)
let _ = register_native "Native.Prelude.hd_tag" "name" hd_tag_lib
let tl_tag = V.tl_tag
let tl_tag_lib = Value.N (tl_tag)
let _ = register_native "Native.Prelude.tl_tag" "name" tl_tag_lib
let nil_tag = V.nil_tag
let nil_tag_lib = Value.N (nil_tag)
let _ = register_native "Native.Prelude.nil_tag" "name" nil_tag_lib

(* these need to be baked in here, because the parser uses them *)
let cons_qid = "Native.Prelude.Cons" 
let cons h t = Schema.mk_cons (Info.M cons_qid) h t
let cons_lib = 
  mk_tfun "schema -> schema" cons_qid 
    (fun h -> mk_tfun "schema" cons_qid 
       (fun t -> Value.T(cons h t)))
let _ = register_native cons_qid "schema -> schema -> schema" cons_lib

let nil_qid = "Native.Prelude.Nil"
let nil = Schema.mk_nil (Info.M nil_qid)
let nil_lib = Value.T (nil)
let _ = register_native nil_qid "schema" nil_lib

let any_qid = "Native.Prelude.Any"
let any = Value.Any(Info.M any_qid)
let any_lib = Value.T(any)
let _ = register_native any_qid "schema" any_lib

let empty_qid = "Native.Prelude.Empty"
let empty = Value.Empty(Info.M empty_qid) 
let empty_lib = Value.T (empty)
let _ = register_native empty_qid "schema" empty_lib

(*************)
(* UTILITIES *)
(*************)

(* read *)
let read_qid = "Native.Prelude.read"
let read fn = 
  let fn = match find_filename fn with
      None -> error [`String (read_qid^": cannot locate " ^ fn)]
    | Some fn -> fn in
    try 
      Misc.read fn 
    with 
	_ -> raise (Error.Harmony_error 
		      (fun () -> Format.printf "%s : error reading %s" read_qid fn))
let read_lib =
  mk_nfun "name" read_qid (fun fn -> Value.N (read fn))
let _ = register_native read_qid "name -> name" read_lib

(* load *)  
let load_qid = "Native.Prelude.load"
let load ekey blob = 
  let ekey = Surveyor.get_ekey (Some ekey) "" (Some blob) in
    (Surveyor.get_reader ekey) blob
let load_lib =
  mk_nfun "name -> tree" load_qid
  (fun ekey -> mk_nfun "tree" load_qid
              (fun blob -> Value.V (load ekey blob)))
let _ = register_native load_qid "name -> name -> tree" load_lib

(* load_file *)
let load_file_qid = "Native.Prelude.load_file"
let load_file fn = 
  let (fn,ekeyo) = Surveyor.parse_filename fn in
  let contents = read fn in
  let ekey = Surveyor.get_ekey ekeyo fn (Some contents) in
    load ekey (contents)
let load_file_lib =
  mk_nfun "tree" load_file_qid (fun fn -> Value.V (load_file fn))
let _ = register_native load_file_qid "name -> tree" load_file_lib

(* get *)
let get_qid = "Native.Prelude.get"
let get l c = Lens.get l c
let get_lib =
  mk_lfun "tree -> tree" get_qid
    (fun l ->
       mk_vfun "tree" get_qid (fun c -> Value.V (get l c)))
let _ = register_native get_qid "lens -> tree -> tree" get_lib

(* put *)
let put_qid = "Native.Prelude.put"
let put l a c = Lens.put l a (Some c)
let put_lib =
  mk_lfun "tree -> tree -> tree" put_qid
    (fun l ->
       mk_vfun "tree -> tree" put_qid
         (fun a ->
            mk_vfun "tree" put_qid (fun c -> Value.V (put l a c))))
let _ = register_native put_qid "lens -> tree -> tree -> tree" put_lib

(* create *)
let create_qid = "Native.Prelude.create"
let create l a = Lens.put l a None
let create_lib =
  mk_lfun "tree -> tree" create_qid
    (fun l -> mk_vfun "tree" create_qid
                (fun a -> Value.V (create l a)))
let _ = register_native create_qid "lens -> tree -> tree" create_lib

(* sync *)
let sync_qid = "Native.Prelude._sync"
let sync lo la lb typ orig = 
  let co,ca,cb =
    try
      (V.get_required ~msg:"sync1" orig "O", 
       V.get_required ~msg:"sync2 " orig "A", 
       V.get_required ~msg:"sync3 "orig "B")
    with
        (Error.Harmony_error _) -> 
	  error [`String sync_qid
		; `String ":the initial tree,"
		; `Tree orig
		; `String "should have children 'O', 'A', and 'B'"
		] in
  let ao,aa,ab = (Lens.get lo co, Lens.get la ca, Lens.get lb cb) in
  let _,ao',aa',ab' = Sync.sync typ (Some ao) (Some aa) (Some ab) in
  let (ao',aa',ab') = match (ao',aa',ab') with 
      Some(ao'),Some(aa'),Some(ab') -> (ao',aa',ab') 
    | _ -> assert false in
  let co',ca',cb' = (Lens.put lo ao' (Some co), Lens.put la aa' (Some ca), Lens.put lb ab' (Some cb)) in	
    V.from_list [("O", co'); ("A",ca'); ("B",cb')]
      
let sync_lib = mk_lfun "lens -> lens -> schema -> tree -> tree" sync_qid 
  (fun lo -> mk_lfun "lens -> schema -> tree -> tree" sync_qid
     (fun la -> mk_lfun "schema -> tree -> tree" sync_qid
	(fun lb -> mk_tfun "tree -> tree" sync_qid 
	   (fun typ -> mk_vfun "tree" sync_qid 
	      (fun orig -> V (sync lo la lb typ orig))))))

let _ = register_native sync_qid "lens -> lens -> lens -> schema -> tree -> tree" sync_lib   

(*************)
(* DEBUGGING *)
(*************)

(* PROBE *)
let probe_qid = "Native.Prelude.probe"
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
  mk_nfun "lens" probe_qid
    (fun n -> Value.L (probe n))
let _ = register_native probe_qid "name -> lens" probe_lib
	
(* TRACEPOINT *)
let tracepoint_qid = "Native.Prelude.tracepoint"
let tracepoint = Lens.tracepoint
let tracepoint_lib = 
  mk_nfun "lens -> lens" tracepoint_qid
    (fun n ->
       mk_lfun "lens" tracepoint_qid (fun l -> Value.L (tracepoint n l)))
let _ = register_native tracepoint_qid "name -> lens -> lens" tracepoint_lib

(* INVERT *)
(* flip the direction -- only for bijective lenses! *)
let invert_qid = "Native.Prelude.invert"
let invert l = 
  { get = (fun c -> l.put c None);
    put = (fun a _ -> l.get a) }
let invert_lib = 
  mk_lfun "lens" invert_qid (fun l -> Value.L (invert l))
let _ = register_native invert_qid "lens -> lens" invert_lib

(* ASSERT *)
let no_assert = Prefs.createBool "no-assert" false
  "don't check assertions"
  "don't check assertions"

let assert_qid = "Native.Prelude.assert" 
let assert_native t = 
  let check_assert dir v t = 
    if (not (Prefs.read no_assert))
      && (not (Schema.member v t)) 
    then
      error [`String (assert_qid^"(" ^ dir ^ "): tree");
	     `Tree v;
	     `String "is not a member of "; 
	     `String (Schema.string_of_t t)] 
  in          
    { get = ( fun c -> check_assert "get" c t; c);
      put = ( fun a _ -> check_assert "put" a t; a) }
let assert_lib = 
  mk_tfun "lens" assert_qid 
    (fun t -> L (assert_native t))    
let _ = register_native assert_qid "schema -> lens" assert_lib

(******************)
(* Generic Lenses *)
(******************)

(*** ID ***)
let id_qid = "Native.Prelude.id"
let id =
  { get = (fun c -> c);
    put = (fun a co -> a)}
let id_lib = L id
let _ = register_native id_qid "lens" id_lib
	  
(*** CONST ***)
let const_qid = "Native.Prelude.const"
let const v d =
  { get = (fun c -> v);
    put = (fun a co ->
	     if V.equal a v then
	       match co with
		 | None -> d
		 | Some(c) -> c
	     else error [`String (const_qid ^ "(put): abstract tree");
			 `Tree a;
			 `String "is not equal to"; `Tree (v)]) }
let const_lib =
  mk_vfun "tree -> lens" const_qid
    (fun v ->
       mk_vfun "lens" const_qid (fun d -> Value.L (const v d)))
let _ = register_native const_qid "tree -> tree -> lens" const_lib
	  
(*** COMPOSE2 ***)
let compose2_qid = "Native.Prelude.compose2"
let compose2 l1 l2 = 
  let l1 = memoize_lens l1 in
    { get = (fun c -> (l2.get (l1.get c)));
      put = (fun a co -> 
	       match co with
		 | None -> l1.put (l2.put a None) None
		 | Some c -> l1.put (l2.put a (Some (l1.get c))) co)}      
let compose2_lib = 
  mk_lfun "lens -> lens" compose2_qid
    (fun l1 ->
       mk_lfun "lens" compose2_qid (fun l2 -> Value.L (compose2 l1 l2)))
let _ = register_native compose2_qid "lens -> lens -> lens" compose2_lib

(********************)
(* Lenses for trees *)
(********************)
(*** MAP ***)
(* map - native interface *)
(* map can be implemented in terms of wmap, but it delays evaluation,
   of the sub-lens which is bad for memoization and causes too 
   much consing... *)
let map_qid = "Native.Prelude.map"
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
  mk_lfun "lens" map_qid (fun l -> Value.L (map l))
let _ = register_native map_qid "lens -> lens" map_lib
	  
(*** WMAP ***)
(* wmap - native interface *)
let wmap_qid = "Native.Prelude.wmap"
let wmap (l0 : Value.t -> Value.t) : (V.t, V.t) Lens.t = 
  let l =
    let memo = Hashtbl.create 11 in
      (fun k -> try Hashtbl.find memo k with
	   Not_found -> 
	     let res = match l0 (N k) with L l -> l | v -> focal_type_error (Info.M wmap_qid) SLens v in 
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
let wmap_lib = 
  mk_ffun "name -> lens" "lens" wmap_qid (fun m -> Value.L (wmap m))      
let _ = register_native wmap_qid "(name -> lens) -> lens" wmap_lib
  
(* XFORK *)
let xfork_qid = "Native.Prelude.xfork"
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
	     error [`String xfork_qid; `String "(get): l1 yielded a child not ";
         	    `String "satisfying pa"; 
		    `Tree a1];
	   if not(Name.Set.for_all (fun k -> not (pa k)) (V.dom a2)) then
	     error [`String xfork_qid; `String "(get): l2 yielded a child satisfying pa";
		    `Tree a2];
	   V.concat a1 a2);
    put = (fun a co -> 
	     let co1,co2 =
	       match co with None -> None,None
		 | Some c -> let c1,c2 = V.split pc c in (Some c1,Some c2) in
	     let a1, a2 = V.split pa a in
	     let c1' = l1.put a1 co1 in
	     let c2' = l2.put a2 co2 in
	       if not(Name.Set.for_all pc (V.dom c1')) then
		 error [`String xfork_qid; `String "(put): l1 yielded a child ";
			`String "not satisfying pc"; 
			`Tree c1'];
	       if not(Name.Set.for_all (fun k -> not (pc k)) (V.dom c2')) then
		 error [`String xfork_qid; `String "(put): l2 yielded a child ";
			`String "satisfying pc"; 
			`Tree c2'];
	       V.concat c1' c2')}
let xfork_lib = 
  mk_vfun "tree -> lens -> lens -> lens" xfork_qid
    (fun pcv ->
       mk_vfun "lens -> lens -> lens" xfork_qid
         (fun pav ->
            mk_lfun "lens -> lens" xfork_qid
              (fun l1 ->
                 mk_lfun "lens" xfork_qid (fun l2 -> Value.L (xfork pcv pav l1 l2)))))
let _ = register_native xfork_qid "tree -> tree -> lens -> lens -> lens" xfork_lib
	  
(* HOIST *)
let hoist_qid = "Native.Prelude.hoist"
let hoist k =
  { get = 
      (fun c ->
	 if   (Name.Set.cardinal (V.dom c)) <> 1 
	   or (Name.Set.choose (V.dom c)) <> k then
	     error [`String hoist_qid
		   ; `String "(get): expecting exactly one child named "
		   ; `Name k
		   ; `Break
		   ; `Tree c];
	 V.get_required ~msg:"hoist" c k);
    put = 
      (fun a _ -> 
	 V.set V.empty k (Some a)) }
let hoist_lib = 
  mk_nfun "lens" hoist_qid (fun k -> Value.L (hoist k))    
let _ = register_native hoist_qid "name -> lens" hoist_lib

(* PLUNGE *)
let plunge_qid = "Native.Prelude.plunge"
let plunge k =
  { get = (fun c -> V.set V.empty k (Some c));
    put = (fun a _ -> 
	     if   (Name.Set.cardinal (V.dom a)) <> 1 
	       or (Name.Set.choose (V.dom a)) <> k then
		 error [`String plunge_qid
		       ; `String "(put): expecting exactly one child named "
		       ; `Name k
		       ; `Break
		       ; `Tree a];
	     V.get_required ~msg:"plunge" a k)}    
let plunge_lib = 
  mk_nfun "lens" plunge_qid (fun k -> Value.L (plunge k))    
let _ = register_native plunge_qid "name -> lens" plunge_lib
	  
(******************)
(* Copy and Merge *)
(******************)
(* COPY *)
let copy_qid = "Native.Prelude.copy"
let copy m n =
  { get = 
      (fun c -> 
	 let child =
	   try V.get_required ~msg:"copy" c m
	   with (Error.Harmony_error _) ->
	     error [`String copy_qid
		   ; `String "(get): expecting one child named "
		   ; `Name m 
		   ; `String ")" 
		   ; `Tree c] in
	   V.set c n (Some child)) ;
    put = 
      (fun a _ -> 
	 if (try V.equal (V.get_required  ~msg:"copy "a m) (V.get_required ~msg:"copy" a n)
	     with (Error.Harmony_error _) -> 
	       error [`String copy_qid
		     ; `String "(put): expecting two children named "
		     ; `Name m
		     ; `String "and"
		     ; `Name n
		     ; `Tree a])
	 then V.set a n None
	 else 
	   error [`String copy_qid; `String "(put): expecting two equal children named ";
		  `Name m; 
		  `String " and ";
		  `Name n;
		  `Tree a]) }
let copy_lib =
  mk_nfun "name -> lens" copy_qid
    (fun m ->
       mk_nfun "lens" copy_qid (fun n -> Value.L (copy m n)))
let _ = register_native copy_qid "name -> name -> lens" copy_lib

(* MERGE *)
let merge_qid = "Native.Prelude.merge"                  
let merge m n =
  { get = (fun c -> V.set c n None) ;
    put = 
      (fun a ->
	 function
	   | None -> 
	       (try V.set a n (Some (V.get_required ~msg:"merge" a m))
		with (Error.Harmony_error _) -> 
		  error
		    [ `String merge_qid
		    ; `String "(put): expecting a child named "
		    ; `Name m])
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
		 else V.set a n cno)}
let merge_lib =
  mk_nfun "name -> lens" merge_qid
    (fun m ->
       mk_nfun "lens" merge_qid (fun n -> Value.L (merge m n)))
let _ = register_native merge_qid "name -> name -> lens" merge_lib

(****************)
(* Conditionals *)
(****************)
	    
(* COND *)
let cond_qid = "Native.Prelude.cond"
let cond_impl c ?(b1=fun x -> x) a1 ?(b2=fun x -> x) a2 f21o f12o lt lf =
  { get = 
      (fun cv ->
	 if Schema.member cv c then lt.get cv
	 else lf.get cv);
    put = 
      (fun a co ->
	 if b1 (Schema.member a a1) then
	   if b2 (Schema.member a a2) then
	     match co with
	       | None -> lf.put a co
	       | Some cv ->
		   if Schema.member cv c
		   then lt.put a co
		   else lf.put a co
	   else
	     match co with
	       | None -> lt.put a co
	       | Some cv ->
		   if Schema.member cv c
		   then lt.put a co
		   else lt.put a (match f21o with 
				    | Some l -> (Some (l.get cv))
				    | None   -> None)
	 else
	   if b2 (Schema.member a a2) then
	     match co with
	       | None -> lf.put a co
	       | Some cv ->
		   if Schema.member cv c
		   then lf.put a (match f12o with 
				    | Some l -> (Some (l.get cv))
				    | None   -> None)
		   else lf.put a co
	   else error [
             `String cond_qid;
	     `String "(put): the abstract tree does not satisfy a1 or a2:";
	     `Tree a ]
      )}
let cond_ff c a1 a2 f21 f12 lt lf = cond_impl c a1 a2 (Some f21) (Some f12) lt lf 
let cond_ww c a1 a2 lt lf = cond_impl c a1 a2 None None lt lf
let cond_fw c a1 a2 f21 lt lf = cond_impl c a1 a2 (Some f21) None lt lf
let cond_wf c a1 a2 f12 lt lf = cond_impl c a1 a2 None (Some f12) lt lf
let cond_ff_lib =
  mk_tfun "schema -> schema -> lens -> lens -> lens -> lens -> lens" cond_qid
    (fun c ->
       mk_tfun "schema -> lens -> lens -> lens -> lens -> lens" cond_qid
         (fun a1 ->
            mk_tfun "lens -> lens -> lens -> lens -> lens" cond_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens -> lens" cond_qid
                   (fun f21 ->
                      mk_lfun "lens -> lens -> lens" cond_qid
                        (fun f12 ->
                           mk_lfun "lens -> lens" cond_qid
                             (fun lt -> mk_lfun "lens" cond_qid 
                                          (fun lf -> Value.L (cond_ff c a1 a2 f21 f12 lt lf))))))))
let _ = register_native
  cond_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens -> lens"
  cond_ff_lib
let cond_ww_qid = "Native.Prelude.cond_ww"
let cond_ww_lib =
  mk_tfun "schema -> schema -> lens -> lens -> lens" cond_ww_qid
    (fun c ->
       mk_tfun "schema -> lens -> lens -> lens" cond_ww_qid
         (fun a1 ->
            mk_tfun "lens -> lens -> lens" cond_ww_qid
              (fun a2 ->
                 mk_lfun "lens -> lens" cond_ww_qid
                   (fun lt -> mk_lfun "lens" cond_ww_qid 
                      (fun lf -> Value.L (cond_ww c a1 a2 lt lf))))))
let _ = register_native
  cond_ww_qid
  "schema -> schema -> schema -> lens -> lens -> lens"
  cond_ww_lib
let cond_fw_qid = "Native.Prelude.cond_fw"
let cond_fw_lib =
  mk_tfun "schema -> schema -> lens -> lens -> lens -> lens" cond_fw_qid
    (fun c ->
       mk_tfun "schema -> lens -> lens -> lens -> lens" cond_fw_qid
         (fun a1 ->
            mk_tfun "lens -> lens -> lens -> lens" cond_fw_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens" cond_fw_qid
                   (fun f21 ->
                      mk_lfun "lens -> lens" cond_fw_qid
                        (fun lt -> mk_lfun "lens" cond_fw_qid 
                           (fun lf -> Value.L (cond_fw c a1 a2 f21 lt lf)))))))    
let _ = register_native
  cond_fw_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens"
  cond_fw_lib
let cond_wf_qid = "Native.Prelude.cond_wf"
let cond_wf_lib =
  mk_tfun "schema -> schema -> lens -> lens -> lens -> lens" cond_wf_qid
    (fun c ->
       mk_tfun "schema -> lens -> lens -> lens -> lens" cond_wf_qid
         (fun a1 ->
            mk_tfun "lens -> lens -> lens -> lens" cond_wf_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens" cond_wf_qid
                   (fun f12 ->
                      mk_lfun "lens -> lens" cond_wf_qid
                        (fun lt -> mk_lfun "lens" cond_wf_qid 
                           (fun lf -> Value.L (cond_wf c a1 a2 f12 lt lf)))))))
let _ = register_native
  cond_wf_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens"
  cond_wf_lib

(* ACOND *)
let acond c a lt lf = cond_impl c a ~b2:(fun x -> not x) a None None lt lf
let acond_qid = "Native.Prelude.acond"
let acond_lib =
  mk_tfun "schema -> lens -> lens -> lens" acond_qid
    (fun c ->
       mk_tfun "lens -> lens -> lens" acond_qid
         (fun a ->
            mk_lfun "lens -> lens" acond_qid
              (fun lt ->
                 mk_lfun "lens" acond_qid (fun lf -> Value.L (acond c a lt lf)))))
let _ = register_native
  acond_qid
  "schema -> schema -> lens -> lens -> lens"
  acond_lib
  
(*************)
(* DATABASES *)
(*************)

(* PIVOT *)
let pivot_qid = "Native.Prelude.pivot"
let pivot k =
  { get = 
      (fun c ->
	   try 
	     let ck = V.get_required  ~msg:"pivot" c k in
	     let ckv = V.get_value ck in
	       V.set V.empty ckv (Some (V.set c k None))
	   with (Error.Harmony_error _) -> 
	     error [`String "Native.Prelude.pivot(get): the following tree should have ";
		    `String "exactly one child named "; 
		    `Name k; 
		    `String ", leading to a value ";
		    `Tree c]);
    put = 
      (fun a _ ->
	 if (Name.Set.cardinal (V.dom a)) <> 1 then
	   error [`String "Native.Prelude.pivot(get): the following tree should have ";
		  `String "exactly one child"; 
		  `Tree a]
	 else
	   let ak = Name.Set.choose (V.dom a) in
	   let w = try V.get_required ~msg:"pivot" a ak with (Error.Harmony_error f) -> f (); assert false in
	     if V.get w k <> None then
	       error [`String "Native.Prelude.pivot(put): child ";
		      `Name k;
		      `String "of this tree should not exist: "; 
		      `Tree w]
	     else
	       V.set w k (Some (V.new_value ak))
      )} 
let pivot_lib = 
  mk_nfun "lens" pivot_qid (fun k -> L(pivot k))    
let _ = register_native 
  pivot_qid 
  "name -> lens" 
  pivot_lib
  
(* JOIN *)
(* Dan Spoonhower's outer join *)
(* disclaimer: written down very quickly and directly.  trying to get
   it correct so using lots of lets and explicit match statements for
   readability.  let's clean it up later -nate
*)
let join_qid = "Native.Prelude.join"
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
	   let tm1, tm2 = (V.get_required ~msg:"join" c m1, V.get_required ~msg:"join" c m2) in	 
	     compute_join tm1 tm2 V.empty
	 with (Error.Harmony_error _) -> 
	   error [`String "Native.Prelude.join(get): expected tree with children: "; 
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
	       let tk = V.get_required ~msg:"join" b k in
	       let b' = (V.set b k None) in
	       let cm1,cm2 = (V.get_required ~msg:"join" acc m1,V.get_required ~msg:"join" acc m2) in
	       let acc' = 
		 match (V.get tk m1, V.get tk m2) with
		   | None, None       -> 
		       error [`String "Native.Prelude.join(put): illformed abstract tree"]
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
	     with (Error.Harmony_error _) ->
	       error [`String "Native.Prelude.join(put): the impossible happened"] in
	   compute_unjoin a init 
      )
  }
let join_lib = 
  mk_nfun "name -> lens" join_qid 
    (fun n1 -> mk_nfun "lens" join_qid 
       (fun n2 -> L (join n1 n2)))
let _ = register_native join_qid "name -> name -> lens" join_lib

(* FLATTEN *)
let flatten_qid = "Native.Prelude.flatten"
let flatten =   
  let rec get = function 
      c -> 	
	if V.is_empty_list c then V.empty
	else 
	  (* Error handling in case of ill-formed list *)
	  let head = V.get_required ~msg:"flatten" c V.hd_tag in 
	  let c' = V.get_required ~msg:"flatten" c V.tl_tag in
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
	    else error [`String "Native.Prelude.flatten(get): expected a tree with exactly one child: "; 
			`Tree head]
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
	  let head = V.get_required ~msg:"flatten" c V.hd_tag in
          (* Error handling in case of ill-formed list *)
	  let c' = V.get_required  ~msg:"flatten" c V.tl_tag in
          (* List of labels pointing toward trees *)
          if Name.Set.cardinal (V.dom head) = 1 then 
            let c_list = V.to_list head in
            let (k,d) = Safelist.hd c_list in
            match V.get a k with
                None -> put a (Some c')
              | Some ds -> 
                  (* Error handling in case of ill-formed list *)
                  let d' = V.get_required  ~msg:"flatten" ds V.hd_tag in 
                  let s = V.get_required  ~msg:"flatten" ds V.tl_tag in
                  if V.is_empty_list s then
                    V.cons 
                      (V.from_list [k,d']) 
                      (put (V.set a k None) (Some c'))
                  else 
                    V.cons 
                      (V.from_list [k,d']) 
                      (put (V.set (V.set a k None) k (Some s)) (Some c'))
          else error [`String "Native.Prelude.flatten(put): expected a tree with exactly one child: "; 
                      `Tree head]
  in
    {get = get ;
     put = put }
let flatten_lib =  L flatten
let _ = register_native flatten_qid "lens" flatten_lib

(************)
(* EXPLODE  *)
(************)
let explode_qid = "Native.Prelude.explode"
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
		 `Tree a
		];
	let ch = Name.Set.choose (V.dom a) in
	  if String.length ch <> 1 then
	    error [`String ("Native.Prelude.explode (" ^ msg ^ ") : expecting child with a one character name");
		   `Tree a
		  ];
	  ch^(string_of_tree msg q)
  in
    { get =
	(fun c ->
	   if( Name.Set.cardinal (V.dom c)) <> 1 then
	     error [`String " Native.Prelude.explode (get) : expecting exactly one child :";
		    `Tree c];
	   let k = Name.Set.choose (V.dom c) in
	     (* here is the string we have to 'explode' *)
	     V.structure_from_list (tree_of_string "get" k)
	);
      put = (fun a _ -> V.new_value (string_of_tree "put" (V.list_from_structure a)))
    }
      
let explode_lib = L (explode)
let _ = register_native explode_qid "lens" explode_lib
    
(*********)
(* LINES *)
(*********)
let lines_qid = "Native.Prelude.lines"
let lines =
   { get = (fun c ->
              if not (V.is_value c) then
                error [`String ("Native.Prelude.lines (get) : expecting exactly one child :"); `Tree c];
              V.structure_from_list (Safelist.map V.new_value (Misc.split_nonescape '\n' (V.get_value c))));
     put = (fun a c ->
             let lines = Safelist.map V.get_value (V.list_from_structure a) in
             if lines=[] then
               error [`String ("Native.Prelude.lines (put) : abstract argument must be non-empty :"); `Tree a];
             Safelist.iter
               (fun s -> if String.contains s '\n' then
                           error [`String ("Native.Prelude.lines (put): abstract tree contains '\\n'"); `Tree a])
               lines;
             V.new_value (String.concat "\n" lines)) }
      
let lines_lib = L (lines)
let _ = register_native lines_qid "lens" lines_lib
    
(* PAD *)
(* pad a list to a power of two *)
let pad_qid = "Native.Prelude.pad"
let pad n = 
  let nextPowerOf2 n = 
    let lg = (log (float_of_int n)) /. (log 2.0) in
      int_of_float (2.0 ** (ceil lg))
  in
  let pad_tree = V.set V.empty n (Some V.empty) in
    { get = 
	(fun c -> 
	   let len = V.list_length c in
	   let rec add_pad v = function
	       0 -> v
	     | x -> add_pad (V.cons pad_tree v) (x-1)
	   in
	     add_pad c (nextPowerOf2 len)
	);
      put = 
	(* FIXME: all the pads should be at the front of the list, 
	   but this primitive doesn't test that *)
	(fun a co ->
	   V.structure_from_list 
	     (Safelist.filter 
		(fun v -> not (V.equal pad_tree v))
		(V.list_from_structure a)))}
let pad_lib = mk_nfun "lens" pad_qid (fun n -> L (pad n))    
let _ = register_native pad_qid "name -> lens" pad_lib
  
(* split an even list in half *)
let even_split_qid = "Native.Prelude.even_split"
let even_split = 
  let check_list v dir = 
    if not (V.is_list v) then 
      error [`String (Printf.sprintf "Native.Prelude.even_split (%s): " dir); `Tree v; `String "is not a list"]
  in
    {
      get = 
	(fun c -> 
	   check_list c "get";
	   let l = V.list_from_structure c in
	     match l with 
		 [] -> c
	       | _ -> begin
		   let n = Safelist.length l in
		   let rec loop l acc i = 
		   if i = 0 then (Safelist.rev acc, l)
		   else match l with 
		       h::t -> loop t (h::acc) (i-1)
		     | [] -> error 
			 [`String "Native.Prelude.even_split (get): ";
			  `Tree c;
			  `String "does not have even length"]
		 in
		   let (l1,l2) = loop l [] (n/2) in
		   let vl1 = V.structure_from_list l1 in
		   let vl2 = V.structure_from_list l2 in
		     V.structure_from_list [vl1;vl2] 
		 end);    
      put = 
	(fun a co -> 
	   check_list a "put";
	   match V.list_from_structure a with
	       [] -> a
	     | [l1;l2] -> 
		 check_list l1 "put";
		 check_list l2 "put";
		 V.structure_from_list ((V.list_from_structure l1) @ (V.list_from_structure l2))
	     | _ -> 
		 error 
		   [`String "Native.Prelude.even_split (get): "
		   ; `Space
		   ; `Tree a
		   ; `Space
		   ; `String "is not a list of length two"])}
let even_split_lib = L (even_split)
let _ = register_native even_split_qid "lens" even_split_lib
