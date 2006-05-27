(***********)
(* PRELUDE *)
(***********)

open Lens
open Registry
open Value
open Syntax

let (@) = Safelist.append

(* some common checkers *)
let id_checker = 
  let c s = s in 
    BIJ(c,c)

let unchecked q = 
  WB(fun s -> error [`String q; `Space; `String "is unchecked"])

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
let cons h t = Schema.mk_cons h t
let cons_lib = 
  mk_sfun "schema -> schema" cons_qid 
    (fun h -> mk_sfun "schema" cons_qid 
       (fun t -> Value.S(cons h t)))
let _ = register_native cons_qid "schema -> schema -> schema" cons_lib

let nil_qid = "Native.Prelude.Nil"
let nil = Schema.mk_nil
let nil_lib = Value.S (nil)
let _ = register_native nil_qid "schema" nil_lib

let any_qid = "Native.Prelude.Any"
let any = Schema.mk_any
let any_lib = Value.S(any)
let _ = register_native any_qid "schema" any_lib

(*************)
(* UTILITIES *)
(*************)

(* read *)
let read_qid = "Native.Prelude.read"
let read fn = 
  let fn = match find_filename fn [] with
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
    (Surveyor.get_reader ekey) (Surveyor.FromString blob)
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
let get (l,_) c = Lens.get l c
let get_lib =
  mk_lfun "tree -> tree" get_qid
    (fun lc -> mk_vfun "tree" get_qid (fun c -> Value.V (get lc c)))
let _ = register_native get_qid "lens -> tree -> tree" get_lib

(* put *)
let put_qid = "Native.Prelude.put"
let put l a c = Lens.put l a (Some c)
let put_lib =
  mk_lfun "tree -> tree -> tree" put_qid
    (fun (l,_) -> 
       mk_vfun "tree -> tree" put_qid 
         (fun a -> mk_vfun "tree" put_qid 
            (fun c -> Value.V (put l a c))))
let _ = register_native put_qid "lens -> tree -> tree -> tree" put_lib

(* create *)
let create_qid = "Native.Prelude.create"
let create l a = Lens.put l a None
let create_lib =
  mk_lfun "tree -> tree" create_qid
    (fun (l,_) -> mk_vfun "tree" create_qid
       (fun a -> Value.V (create l a)))
let _ = register_native create_qid "lens -> tree -> tree" create_lib

(* sync *)
let sync_qid = "Native.Prelude._sync"
let sync lo la lb typ orig = 
  let co,ca,cb =
    try
      (V.get_required ~msg:"sync1" orig "O", 
       V.get_required ~msg:"sync2" orig "A", 
       V.get_required ~msg:"sync3" orig "B")
    with
        (Error.Harmony_error _) -> 
	  error [`String sync_qid
		; `String ":the initial tree,"
		; `Tree orig
		; `String "should have children 'O', 'A', and 'B'"
		] in
  let log = (V.get orig "log" <> None) in
  let ao,aa,ab = (Lens.get lo co, Lens.get la ca, Lens.get lb cb) in
  let a,ao',aa',ab' = Sync.sync typ (Some ao, Some aa, Some ab) in
  let (ao',aa',ab') = match (ao',aa',ab') with 
      Some(ao'),Some(aa'),Some(ab') -> (ao',aa',ab') 
    | _ -> assert false in
  if log then begin Sync.format_action a; Format.printf "@\n" end;
  let co',ca',cb' = (Lens.put lo ao' (Some co), Lens.put la aa' (Some ca), Lens.put lb ab' (Some cb)) in	
    V.from_list [("O", co'); ("A",ca'); ("B",cb')]
      
let sync_lib = mk_lfun "lens -> lens -> schema -> tree -> tree" sync_qid 
  (fun (lo,_) -> mk_lfun "lens -> schema -> tree -> tree" sync_qid
     (fun (la,_) -> mk_lfun "schema -> tree -> tree" sync_qid
	(fun (lb,_) -> mk_sfun "tree -> tree" sync_qid 
	   (fun typ -> mk_vfun "tree" sync_qid 
	      (fun orig -> V (sync lo la lb typ orig))))))

let _ = register_native sync_qid "lens -> lens -> lens -> schema -> tree -> tree" sync_lib   

(*************)
(* DEBUGGING *)
(*************)

(* PROBE *)
let probe_qid = "Native.Prelude.probe"
let probe msg = 
  let lens = 
    { get = (fun c ->
               Format.printf "@,@[<v0>%s (get) @,  " msg;
               V.format_t c;
               Format.printf "@,@]";
               Format.print_flush ();
               c);
      put = (fun a co ->     
	       Format.printf "@,@[<v0>%s (put) @,  " msg;
	       V.format_t a;
               Format.printf "@ into@   ";
               begin
                 match co with
                     None -> Format.printf "MISSING";
                   | Some c -> V.format_t c
               end;
               Format.printf "@,@]";
               Format.print_flush ();
               a)} in
    (lens,id_checker)

let probe_lib = mk_nfun "lens" probe_qid 
  (fun n -> 
     let l,ck = probe n in 
       Value.L(l,ck))
      
let _ = register_native probe_qid "name -> lens" probe_lib
	
(* PROGRESS *)
let progress_qid = "Native.Prelude.progress"
let time() =
  let times = Unix.times() in
  let total = times.Unix.tms_utime +. times.Unix.tms_stime in
  Printf.sprintf "%.1f" total
let progress msg = 
  let lens = 
    { get = (fun c -> Format.printf "@,[->%s %s]@," (time()) msg; c);
      put = (fun a co -> Format.printf "@,[<-%s %s]@," (time()) msg; a)} in
    (lens,id_checker)
let progress_lib = mk_nfun "lens" progress_qid 
  (fun n -> 
     let l,ck = progress n in 
       Value.L (l,ck))
let _ = register_native progress_qid "name -> lens" progress_lib
	
(* TRACEPOINT *)
let tracepoint_qid = "Native.Prelude.tracepoint"
let tracepoint n lc = 
  let lc_lens, lc_checker = lc in 
  let lens = Lens.tracepoint n lc_lens in
    (lens,lc_checker)
      
let tracepoint_lib = 
  mk_nfun "lens -> lens" tracepoint_qid
    (fun n ->
       mk_lfun "lens" tracepoint_qid 
         (fun lc -> 
            let (l,ck) = tracepoint n lc in 
              Value.L (l,ck)))
let _ = register_native tracepoint_qid "name -> lens -> lens" tracepoint_lib

(* INVERT *)
(* flip the direction -- only for bijective lenses! *)
let invert_qid = "Native.Prelude.invert"
let invert lc = 
  let switch l = 
    { get = (fun c -> l.put c None);
      put = (fun a _ -> l.get a) } in
    match lc with 
        l,BIJ(c2a,a2c) -> 
          (switch l,BIJ(a2c,c2a))
      | l,_ -> 
          (* stub until finished; eventually, this dynamic error can be made static! *)
          (switch l, WB(fun c -> error [`String "invert used with non-bijective lens"]))

let invert_lib = mk_lfun "lens" invert_qid 
  (fun lc -> 
     let l,ck = invert lc in 
       Value.L (l,ck))
  
let _ = register_native invert_qid "lens -> lens" invert_lib

(******************)
(* Generic Lenses *)
(******************)

(*** ID ***)
let id_qid = "Native.Prelude.id"
let id =
  let lens = 
    { get = (fun c -> c);
      put = (fun a co -> a)} in  
    (lens, id_checker)    
let id_lib = 
  let l,ck = id in 
    Value.L(l,ck) 
let _ = register_native id_qid "lens" id_lib
	  
(*** CONST ***)
let const_qid = "Native.Prelude.const"
let const v d =
  let lens = 
    { get = (fun c -> v);
      put = (fun a co ->
	       if V.equal a v then
	         match co with
		   | None -> d
		   | Some(c) -> c
	       else error [`String (const_qid ^ "(put): abstract tree"); 
			   `Space; `Tree a; `Space;
			   `String "is not equal to"; `Tree (v)]) } in
  let checker = 
    VWB (fun c -> 
           if not (Schema.member d c) then
             error [`String const_qid; `Space;
                    `Tree d; `Space; 
                    `String "is not a member of"; `Space;
                    `Prim (fun () -> Schema.format_t c)]
           else Schema.t_of_tree v) in 
    (lens,checker)
      
let const_lib =
  mk_vfun "tree -> lens" const_qid
    (fun v ->
       mk_vfun "lens" const_qid 
         (fun d -> 
            let l,ck = const v d in 
              Value.L (l,ck)))
let _ = register_native const_qid "tree -> tree -> lens" const_lib

(*** COMPOSE2 ***)
let compose2_qid = "Native.Prelude.compose2"
let compose2 lc1 lc2 =
  let (l1,c1),(l2,c2) = lc1,lc2 in 
  (* helper to make memoized put function *)
  let mk_memoized_put l1 = 
    let l1 = memoize_lens l1 in     
      (fun a co -> match co with 
           None -> l1.put (l2.put a None) None
         | Some c -> l1.put (l2.put a (Some (l1.get c))) co) in 
  let compose2_get = (fun c -> l2.get (l1.get c)) in       
  let compose2_put,checker = match c2 with
      BIJ(b2a,a2b) -> 
        (* type-based optimization: if l2 is bijective we don't need
           to compute get during putback, hence no memoizing :) *)
        let put = (fun a co -> l1.put (l2.put a None) co) in 
        let checker = match c1 with
            BIJ(c2b,b2c) -> BIJ ((fun c -> b2a (c2b c)), (fun a -> b2c (a2b a)))
          | VWB(c2b)     -> VWB (fun c -> b2a (c2b c))
          | WB(c2b)      -> WB  (fun c -> b2a (c2b c)) in 
          (put, checker)
    | VWB(b2a) -> 
        let checker = match c1 with 
            BIJ(c2b,_) 
          | VWB(c2b) -> VWB (fun c -> b2a (c2b c))
          | WB(c2b)  -> WB (fun c -> b2a (c2b c)) in
          (mk_memoized_put l1, checker)
    | WB(b2a) ->           
        let checker = match c1 with BIJ(c2b,_) | VWB(c2b) | WB(c2b) -> 
          WB(fun c-> b2a (c2b c)) in 
          (mk_memoized_put l1,checker) in 
  let lens = { get=compose2_get; put=compose2_put} in 
    (lens, checker)
      
let compose2_lib = 
  mk_lfun "lens -> lens" compose2_qid
    (fun lc1 ->
       mk_lfun "lens" compose2_qid 
         (fun lc2 -> 
            let l,ck = compose2 lc1 lc2 in 
              Value.L (l,ck)))
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
let map (l,c) = 
  let lens = 
    { get = (fun c ->
	       let binds =
	         V.fold
		   (fun k ck bindacc ->
		      let ck' = (l.get ck) in
		        ((k,Some ck')::bindacc))
		   c [] in
	         V.create_star binds);
      put =
        (* we can do a type-based optimization here, but i haven't done it -JNF *)
        (fun a co ->
           let c = match co with None -> V.empty | Some c -> c in
 	   let cbinds = V.fold
	     (fun k vk bindacc ->
	        (k,
	         begin match (V.get c k),(V.get a k) with
		   | Some ck, Some ak -> Some ((l.put ak (Some ck)))
		   | Some ck, None -> None
		   | None, Some ak -> Some ((l.put ak None))
		   | _ -> assert false 
	         end)::bindacc)
  	     a [] in
	     V.create_star cbinds)} in 
  let checker = 
    let mk_map_checker ck c = 
      match Schema.project_all c with
          None -> error [`String map_qid; `Space;
                         `String "expected schema with consistent subschema"; `Space;
                         `String "found:"; `Space;
                         `Prim (fun () -> Schema.format_t c)]
        | Some c1 -> 
            let a1 = ck c1 in 
              Schema.inject c a1 in  
      match c with 
          BIJ(c2a,a2c)       -> BIJ(mk_map_checker c2a, mk_map_checker a2c)
        | VWB(c2a) | WB(c2a) -> WB(mk_map_checker c2a) in 
    (lens, checker)
    
(* map - library interface *)
let map_lib = 
  mk_lfun "lens" map_qid 
    (fun lc -> 
       let l,ck = map lc in 
         Value.L (l,ck))
let _ = register_native map_qid "lens -> lens" map_lib
  
(* XFORK *)
let xfork_qid = "Native.Prelude.xfork"
let xfork pcv pav (l1,c1) (l2,c2) = 
  let dom_pcv = V.dom pcv in (* FIXME: check that pcv and pca have height <= 1? *)
  let dom_pav = V.dom pav in
  let pc = fun n -> Name.Set.mem n dom_pcv in
  let pa = fun n -> Name.Set.mem n dom_pav in
  let lens = 
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
	         V.concat c1' c2')} in 
  let checker = 
    let aux pc ck1 ck2 = 
      fun s -> 
        if Schema.empty s then s 
        else let s_pc,s_neg_pc = Schema.restrict pc s in 
          Schema.mk_cat[ck1 s_pc; ck2 s_neg_pc] in
      match c1,c2 with
          BIJ(c2a,a2c),BIJ(c2a',a2c') -> 
            BIJ(aux dom_pcv c2a c2a', aux dom_pav a2c a2c')
        | BIJ(c2a,_),VWB(c2a') | VWB(c2a),BIJ(c2a',_) | VWB(c2a),VWB(c2a') -> 
            VWB(aux dom_pcv c2a c2a')
        | BIJ(c2a,_),WB(c2a') | WB(c2a),BIJ(c2a',_) | VWB(c2a),WB(c2a') | WB(c2a),VWB(c2a') | WB(c2a),WB(c2a') -> 
            WB(aux dom_pcv c2a c2a') in
    (lens, checker)
      
let xfork_lib = 
  mk_vfun "tree -> lens -> lens -> lens" xfork_qid
    (fun pcv ->
       mk_vfun "lens -> lens -> lens" xfork_qid
         (fun pav ->
            mk_lfun "lens -> lens" xfork_qid
              (fun l1 ->
                 mk_lfun "lens" xfork_qid 
                   (fun l2 -> 
                      let l,ck = xfork pcv pav l1 l2 in 
                        Value.L (l,ck)))))
let _ = register_native xfork_qid "tree -> tree -> lens -> lens -> lens" xfork_lib

(* BFORK *)
let bfork_qid = "Native.Prelude.bfork"
let bfork sc pca lc1 lc2 =
  let lens = 
    let (l1,_),(l2,_) = lc1,lc2 in 
    let dom_pav = V.dom pca in
    let pc = fun v -> Schema.member v sc in
    let pa = fun n -> Name.Set.mem n dom_pav in
      { get = 
          (fun c ->
             let c1l, c2l = Safelist.partition (fun (n,v) -> pc v) (V.to_list c) in
             let c1,c2 = (V.from_list c1l, V.from_list c2l) in
             let a1 = l1.get c1 in
             let a2 = l2.get c2 in
               if not(Name.Set.for_all pa (V.dom a1)) then
	         error [`String bfork_qid; `String "(get): l1 yielded a child not ";
         	        `String "satisfying pa"; 
		        `Tree a1];
               if not(Name.Set.for_all (fun k -> not (pa k)) (V.dom a2)) then
	         error [`String bfork_qid; `String "(get): l2 yielded a child satisfying pa";
		        `Tree a2];
               V.concat a1 a2);
        put = (fun a co -> 
                 let co1,co2 =
	           match co with None -> None,None
	             | Some c -> 
	                 let c1l, c2l = Safelist.partition (fun (n,v) -> pc v) (V.to_list c) in
	                   (Some (V.from_list c1l), Some(V.from_list c2l)) in
                 let a1, a2 = V.split pa a in
                 let c1' = l1.put a1 co1 in
                 let c2' = l2.put a2 co2 in
                   if not(Safelist.for_all (fun (_,v) -> pc v) (V.to_list c1')) then
	             error [`String bfork_qid; `String "(put): l1 yielded a child ";
		            `String "not satisfying sc"; 
		            `Tree c1'];
                   if not(Safelist.for_all (fun (_,v) -> not (pc v)) (V.to_list c2')) then
	             error [`String xfork_qid; `String "(put): l2 yielded a child ";
		            `String "satisfying pc"; 
		            `Tree c2'];
                   V.concat c1' c2')} in 
    (lens, unchecked bfork_qid)
      
let bfork_lib = 
  mk_sfun "tree -> lens -> lens -> lens" bfork_qid
    (fun c ->
       mk_vfun "lens -> lens -> lens" bfork_qid
         (fun pav ->
            mk_lfun "lens -> lens" bfork_qid
              (fun l1 ->
                 mk_lfun "lens" bfork_qid 
                   (fun l2 -> 
                      let l,ck = bfork c pav l1 l2 in 
                        Value.L(l,ck)))))
let _ = register_native bfork_qid "schema -> tree -> lens -> lens -> lens" bfork_lib

(* HOIST *)
let hoist_qid = "Native.Prelude.hoist"
let hoist k =
  let lens = 
    { get = 
        (fun c ->
	   if   (Name.Set.cardinal (V.dom c)) <> 1 
	     or (Name.Set.choose (V.dom c)) <> k then
	       error [`String hoist_qid
		     ; `String "(get): expecting exactly one child named"
                     ; `Break
		     ; `Name k
		     ; `Break
		     ; `Tree c];
	   V.get_required ~msg:"hoist" c k);
      put = 
        (fun a _ -> 
	   V.set V.empty k (Some a)) } in 
  let c2a c = 
    match 
      Schema.project k c, 
      Schema.subschema c 
        (Schema.mk_wild (Name.Set.empty) 1 false Schema.mk_any) 
    with     
        None,_ | _,false -> 
          error [`String hoist_qid;`Space;
                 `String "may only be used with schemas of the form {n=A}, for some A"]
      | Some a,true -> a in 
  let a2c a = Schema.mk_atom k a in
    (lens, BIJ(c2a,a2c))

let hoist_lib = 
  mk_nfun "lens" hoist_qid 
    (fun k -> 
       let l,ck = hoist k in 
         Value.L (l,ck))    
let _ = register_native hoist_qid "name -> lens" hoist_lib
	  
(******************)
(* Copy and Merge *)
(******************)
(* COPY *)
let copy_qid = "Native.Prelude.copy"
let copy m n =
  let lens = 
    { get = 
        (fun c -> 
	   let child =
	     try V.get_required ~msg:"copy" c m
	     with (Error.Harmony_error _) ->
	       error [`String copy_qid
		     ; `String "(get): expecting one child named"
                     ; `Break
		     ; `Name m 
		     ; `String ")" 
		     ; `Tree c] in
	     V.set c n (Some child)) ;
      put = 
        (fun a _ -> 
	   if (try V.equal 
                 (V.get_required  ~msg:"copy "a m) 
                 (V.get_required ~msg:"copy" a n)
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
		    `Tree a]) } in  
    (lens, unchecked copy_qid)

let copy_lib =
  mk_nfun "name -> lens" copy_qid
    (fun m ->
       mk_nfun "lens" copy_qid 
         (fun n -> 
            let l,ck = copy m n in 
              Value.L (l,ck)))
let _ = register_native copy_qid "name -> name -> lens" copy_lib

(* MERGE *)
let merge_qid = "Native.Prelude.merge"
let merge m n =
  let lens = 
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
		   else V.set a n cno)} in 
  let checker =
    VWB(fun c -> 
          let mn = Name.Set.add m (Name.Set.singleton n) in 
          let c_mn,c_rest = Schema.restrict mn c in 
          let d = match Schema.project_all c_mn with 
              None -> error [`String merge_qid; `Space
                            ; `String "may only be used with a schema of the form (C1 + { ?m=D, ?n=D })"]
            | Some d -> d in 
            Schema.mk_cat [c_rest; Schema.mk_atom m d]) in    
    (lens, checker)

let merge_lib =
  mk_nfun "name -> lens" merge_qid
    (fun m ->
       mk_nfun "lens" merge_qid 
         (fun n -> 
            let l,ck = merge m n in 
              Value.L (l,ck)))
let _ = register_native merge_qid "name -> name -> lens" merge_lib

(****************)
(* Conditionals *)
(****************)
	    
(* COND *)
let cond_qid = "Native.Prelude.cond"
let cond_impl c1 a1 a2 f21o f12o (lt,ct) (lf,cf) = 
  let lens = 
    { get = 
        (fun cv ->
	   if Schema.member cv c1 then lt.get cv
	   else lf.get cv);
      put = 
        (fun a co ->
	   if Schema.member a a1 then
	     if Schema.member a a2 then
	       match co with
	         | None -> lf.put a co
	         | Some cv ->
		     if Schema.member cv c1
		     then lt.put a co
		     else lf.put a co
	     else
	       match co with
	         | None -> lt.put a co
	         | Some cv ->
		     if Schema.member cv c1
		     then lt.put a co
		     else lt.put a (match f21o with 
				      | Some (l,_) -> (Some (l.get cv))
				      | None   -> None)
	   else
	     if Schema.member a a2 then
	       match co with
	         | None -> lf.put a co
	         | Some cv ->
		     if Schema.member cv c1
		     then lf.put a (match f12o with 
				      | Some (l,_) -> (Some (l.get cv))
				      | None   -> None)
		     else lf.put a co
	     else error [
               `String cond_qid;
	       `String "(put): the abstract tree does not satisfy a1 or a2:";
	       `Tree a ]
        )} in 
  let checker = 
    let cond_checker c2a c2a' c = 
      let c_inter_c1 = Schema.mk_isect [c;c1] in 
      let c_minus_c1 = Schema.mk_diff c c1 in 
      let calc_a1 = c2a c_inter_c1 in 
      let calc_a2 = c2a' c_minus_c1 in 
      let _ = 
        (* check that abstract schemas are subschemas of the ones written down *)
        if not (Schema.subschema calc_a1 a1) then 
          error [`String cond_qid; `Space;
                 `String "the abstract schema of l1 must be a subschema of a1"];
        if not (Schema.subschema calc_a2 a2) then 
          error [`String cond_qid; `Space;
                 `String "the abstract schema of l2 must be a subschema of a1"] in
      let _ = 
        (* check fixup functions *)
        let aux msg fi2jo ci cj = match fi2jo with 
            None -> ()
          | Some (_,BIJ(i2j,_)) | Some(_,VWB(i2j)) | Some(_,WB(i2j)) -> 
              if not (Schema.subschema (i2j ci) cj) then 
                error [`String cond_qid; `Space;
                       `String msg; `Space;
                       `String "has an incorrect type"] in
          aux "f21" f21o c_minus_c1 c_inter_c1;
          aux "f12" f12o c_inter_c1 c_minus_c1 in
        Schema.mk_union [calc_a1;calc_a2] in        
      match ct,cf with
          BIJ(c2a,a2c),BIJ(c2a',a2c') ->               
            (* the cond is bijective iff a1 and a2 are disjoint *)
            if Schema.empty (Schema.mk_isect [a1;a2]) then 
              let ck_a2c a = 
                let a_inter_a1 = Schema.mk_isect [a;a1] in 
                let a_inter_a2 = Schema.mk_isect [a;a2] in 
                let calc_c1 = a2c a_inter_a1 in
                let calc_c2 = a2c a_inter_a2 in 
                  Schema.mk_union [calc_c1;calc_c2] in
                BIJ(cond_checker c2a c2a',ck_a2c)
            else
              VWB(cond_checker c2a c2a')
        | VWB(c2a),BIJ(c2a',_) | BIJ(c2a,_),VWB(c2a') | VWB(c2a),VWB(c2a') ->
            VWB(cond_checker c2a c2a')
        | WB(c2a),BIJ(c2a',_) | BIJ(c2a,_),WB(c2a') 
        | WB(c2a),VWB(c2a') | VWB(c2a),WB(c2a') 
        | WB(c2a),WB(c2a') ->
            WB(cond_checker c2a c2a') in
    (lens,checker)

let cond_ff c a1 a2 f21 f12 lt lf = cond_impl c a1 a2 (Some f21) (Some f12) lt lf 
let cond_ww c a1 a2 lt lf = cond_impl c a1 a2 None None lt lf
let cond_fw c a1 a2 f21 lt lf = cond_impl c a1 a2 (Some f21) None lt lf
let cond_wf c a1 a2 f12 lt lf = cond_impl c a1 a2 None (Some f12) lt lf
let cond_ff_lib =
  mk_sfun "schema -> schema -> lens -> lens -> lens -> lens -> lens" cond_qid
    (fun c ->
       mk_sfun "schema -> lens -> lens -> lens -> lens -> lens" cond_qid
         (fun a1 ->
            mk_sfun "lens -> lens -> lens -> lens -> lens" cond_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens -> lens" cond_qid
                   (fun f21 ->
                      mk_lfun "lens -> lens -> lens" cond_qid
                        (fun f12 ->
                           mk_lfun "lens -> lens" cond_qid
                             (fun lt -> mk_lfun "lens" cond_qid 
                                (fun lf -> 
                                   let l,ck = cond_ff c a1 a2 f21 f12 lt lf in
                                     Value.L (l,ck))))))))
let _ = register_native
  cond_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens -> lens"
  cond_ff_lib
let cond_ww_qid = "Native.Prelude.cond_ww"
let cond_ww_lib =
  mk_sfun "schema -> schema -> lens -> lens -> lens" cond_ww_qid
    (fun c ->
       mk_sfun "schema -> lens -> lens -> lens" cond_ww_qid
         (fun a1 ->
            mk_sfun "lens -> lens -> lens" cond_ww_qid
              (fun a2 ->
                 mk_lfun "lens -> lens" cond_ww_qid
                   (fun lt -> mk_lfun "lens" cond_ww_qid 
                      (fun lf -> 
                         let l,ck = cond_ww c a1 a2 lt lf in 
                           Value.L (l,ck))))))
let _ = register_native
  cond_ww_qid
  "schema -> schema -> schema -> lens -> lens -> lens"
  cond_ww_lib
let cond_fw_qid = "Native.Prelude.cond_fw"
let cond_fw_lib =
  mk_sfun "schema -> schema -> lens -> lens -> lens -> lens" cond_fw_qid
    (fun c ->
       mk_sfun "schema -> lens -> lens -> lens -> lens" cond_fw_qid
         (fun a1 ->
            mk_sfun "lens -> lens -> lens -> lens" cond_fw_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens" cond_fw_qid
                   (fun f21 ->
                      mk_lfun "lens -> lens" cond_fw_qid
                        (fun lt -> mk_lfun "lens" cond_fw_qid 
                           (fun lf -> 
                              let l,ck = cond_fw c a1 a2 f21 lt lf in 
                                Value.L (l,ck)))))))
                                
let _ = register_native
  cond_fw_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens"
  cond_fw_lib
let cond_wf_qid = "Native.Prelude.cond_wf"
let cond_wf_lib =
  mk_sfun "schema -> schema -> lens -> lens -> lens -> lens" cond_wf_qid
    (fun c ->
       mk_sfun "schema -> lens -> lens -> lens -> lens" cond_wf_qid
         (fun a1 ->
            mk_sfun "lens -> lens -> lens -> lens" cond_wf_qid
              (fun a2 ->
                 mk_lfun "lens -> lens -> lens" cond_wf_qid
                   (fun f12 ->
                      mk_lfun "lens -> lens" cond_wf_qid
                        (fun lt -> mk_lfun "lens" cond_wf_qid 
                           (fun lf -> 
                              let l,ck = cond_wf c a1 a2 f12 lt lf in
                                Value.L (l,ck)))))))

let _ = register_native
  cond_wf_qid
  "schema -> schema -> schema -> lens -> lens -> lens -> lens"
  cond_wf_lib

(* ACOND *)
let acond c a lt lf = cond_impl c a (Schema.mk_neg a) None None lt lf
let acond_qid = "Native.Prelude.acond"
let acond_lib =
  mk_sfun "schema -> lens -> lens -> lens" acond_qid
    (fun c ->
       mk_sfun "lens -> lens -> lens" acond_qid
         (fun a ->
            mk_lfun "lens -> lens" acond_qid
              (fun lt ->
                 mk_lfun "lens" acond_qid 
                   (fun lf -> 
                      let l,ck = acond c a lt lf in 
                        Value.L (l,ck)))))

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
  let lens = 
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
	         error [`String "Native.Prelude.pivot(put): child"; `Space
		       ; `Name k; `Space
		       ; `String "of this tree should not exist:"; `Space
		       ; `Tree w]
	       else
	         V.set w k (Some (V.new_value ak))
        )} in
  let checker = 
    let mk_skinny s = Schema.mk_wild Name.Set.empty 1 false s in 
    let value = mk_skinny (Schema.mk_cat []) in 
    let c2a c = 
      let c_k,c_rest = Schema.restrict (Name.Set.singleton k) c in
      let _ = 
        let err () = error [`String pivot_qid; `Space
                           ; `String "may only be used with a shema that has a child"; `Space
                           ; `Name k; `Space
                           ; `String "leading to a value"] in          
          match Schema.project k c_k with
              None -> err ()
            | Some c_k -> 
                if not (Schema.equivalent c_k value) then err () in
        mk_skinny c_rest in 
    let a2c a = 
      if not (Schema.subschema a (mk_skinny Schema.mk_any)) then 
        error [`String pivot_qid; `Space
              ; `String "may only be used with a shema of the form {!=A}"];
      match (Schema.project_all a) with 
          None -> assert false
        | Some a_bang -> Schema.mk_cat [Schema.mk_atom k value; a_bang] in
      BIJ(c2a,a2c) in
    (lens, checker)
      
let pivot_lib = mk_nfun "lens" pivot_qid 
  (fun k -> 
     let l,ck = pivot k in 
       Value.L(l,ck))
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
  let lens = 
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
	       with (Error.Harmony_error m) ->
	         m ();
	         error ([`String "Native.Prelude.join(put): the impossible happened"])in
	     compute_unjoin a init 
        )
    } in 
    (lens, unchecked join_qid)

let join_lib = 
  mk_nfun "name -> lens" join_qid 
    (fun n1 -> mk_nfun "lens" join_qid 
       (fun n2 -> 
          let l,ck = join n1 n2 in 
            Value.L(l,ck)))
let _ = register_native join_qid "name -> name -> lens" join_lib

(* FLATTEN *)
let flatten_op_qid = "Native.Prelude.flatten_op"
let flatten_op (l,ck) =   
  let rec get = function 
      c -> 	
	if V.is_empty_list c then V.empty
	else 
	  (* Error handling in case of ill-formed list *)
          (* the head that is considered is the get of l *)
	  let head = l.get (V.get_required ~msg:"flatten" c V.hd_tag) in 
	  let c' = V.get_required ~msg:"flatten" c V.tl_tag in
	    (* List of labels pointing toward trees *)
	    if Name.Set.cardinal (V.dom head) = 1 then 
	      let c_list = V.to_list head in
	      let (k,d) = Safelist.hd c_list in
	      let a = get c' in
		(* removes spurious *nil labels in output *)
                (* Alan: not needed: the initial test in get already makes sure of this *)
                (*
	      let a = if (V.is_empty_list a_rec) then V.empty else a_rec in *)
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
      None -> V.structure_from_list (Safelist.map (fun x -> l.put x None) (listify a))
    | Some c -> 
	if V.is_empty_list c then put a None
	else 
	  let head = V.get_required ~msg:"flatten" c V.hd_tag in
          let transformed_head = l.get head in
          (* Error handling in case of ill-formed list *)
	  let c' = V.get_required  ~msg:"flatten" c V.tl_tag in
          (* List of labels pointing toward trees *)
          if Name.Set.cardinal (V.dom transformed_head) = 1 then 
            let c_list = V.to_list transformed_head in
            let (k,d) = Safelist.hd c_list in
            match V.get a k with
                None -> put a (Some c')
              | Some ds -> 
                  (* Error handling in case of ill-formed list *)
                  let d' = V.get_required  ~msg:"flatten" ds V.hd_tag in 
                  let s = V.get_required  ~msg:"flatten" ds V.tl_tag in
                  if V.is_empty_list s then
                    V.cons 
                      (l.put (V.from_list [k,d']) (Some head)) 
                      (put (V.set a k None) (Some c'))
                  else 
                    V.cons 
                      (l.put (V.from_list [k,d']) (Some head))
                      (put (V.set (V.set a k None) k (Some s)) (Some c'))
          else error [`String "Native.Prelude.flatten(put): expected a tree with exactly one child: "; 
                      `Tree head]
  in
  let lens = {get = get ;
              put = put } in
    (lens, unchecked flatten_op_qid)

let flatten_op_lib =
  mk_lfun "lens" flatten_op_qid 
    (fun l -> 
       let l,ck = flatten_op l in 
         Value.L (l,ck))
let _ = register_native flatten_op_qid "lens -> lens" flatten_op_lib

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
  let lens = 
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
    } in 
    (lens, unchecked explode_qid)
    
let explode_lib = 
  let l,ck = explode in 
    Value.L (l,ck)
let _ = register_native explode_qid "lens" explode_lib
    
(*********)
(* LINES *)
(*********)
let split_qid = "Native.Prelude.split"
let split sep =
  let sepchar = if String.length sep = 1 then sep.[0]
                else error [`String "Native.Prelude.split (get) - ";
                            `String "separator should be one character:"; `Name sep] in
  let lens = 
    { get = (fun c ->
               if not (V.is_value c) then
                 error [`String ("Native.Prelude.split (get) : expecting exactly one child :"); `Tree c];
               V.structure_from_list (Safelist.map V.new_value (Misc.split_nonescape sepchar (V.get_value c))));
      put = (fun a c ->
               let split = Safelist.map V.get_value (V.list_from_structure a) in
                 if split=[] then
                   error [`String ("Native.Prelude.split (put) : abstract argument must be non-empty :"); `Tree a];
                 Safelist.iter
                   (fun s -> if String.contains s '\n' then
                      error [`String ("Native.Prelude.split (put): abstract tree contains '");
                             `String sep; `String "': ";
                             `Tree a])
                   split;
                 V.new_value (String.concat sep split)) } in 
  let checker =
    let value = Schema.mk_wild Name.Set.empty 1 false (Schema.mk_cat []) in
    let mk_value_list c =
      let x = split_qid ^ "generated value list" in
      let fresh_x = Syntax.fresh x in 
      let x_t = Schema.mk_var fresh_x in
        Schema.mark_tvars [x,Info.M x];
        Schema.update fresh_x (Schema.mk_union [nil;cons c x_t]);
        Schema.finalize ();        
        x_t in
    let c2a c = 
      if not (Schema.subschema c value) then error 
        [`String split_qid; `Space;
         `String "may only be used with concrete schema that is a subschema of {!={}}"];
      mk_value_list c in
    let a2c a = 
      if not (Schema.subschema a (mk_value_list value)) then error
        [`String split_qid; `Space;
         `String "may only be used with the abstract schema (List.T {!={}})"];
      match Schema.project hd_tag a with 
          None -> assert false;
        | Some sub_value -> sub_value in 
      BIJ(c2a,a2c) in
    (lens, checker)
      
let split_lib =
  mk_nfun "lens" split_qid
    (fun k -> 
       let l,ck = split k in 
         Value.L (l,ck))
let _ = register_native split_qid "name -> lens" split_lib
  
(* split an even list in half *)
let even_split_qid = "Native.Prelude.even_split"
let even_split = 
  let check_list v dir = 
    if not (V.is_list v) then 
      error [`String (Printf.sprintf "Native.Prelude.even_split (%s): " dir); `Tree v; `String "is not a list"]
  in
  let lens = 
    { get = 
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
		   ; `String "is not a list of length two"])} in 
    (lens, unchecked even_split_qid)
let even_split_lib = 
  let l,ck = even_split in 
    Value.L (l,ck)
let _ = register_native even_split_qid "lens" even_split_lib

(*** FCONST ***)
let output_from d =
  let readChannelTillEof c =
    let rec loop lines =
      try let l = input_line c in
      loop (l::lines)
      with End_of_file -> lines in
    String.concat "\n" (Safelist.rev (loop [])) in
  let out = Unix.open_process_in d in
  let output = readChannelTillEof out in
  match Unix.close_process_in out with
    Unix.WEXITED 0 -> Some output 
  | _ -> None

let fconst_qid = "Native.Prelude.fconst"
let fconst v d =
  let lens = 
    { get = (fun c -> v);
      put = (fun a co ->
               if V.equal a v then
	         match co with
	           | None -> 
	               (match output_from d with
	                    Some name -> V.new_value name
	                  | None -> error [`String (fconst_qid ^ "(put): cmd");
			                   `String d;
			                   `String "returned with non-zero value"])
	           | Some(c) -> c
               else error [`String (fconst_qid ^ "(put): abstract tree");
		           `Tree a;
		           `String "is not equal to"; `Tree (v)]) } in
    (lens, unchecked fconst_qid)

let fconst_lib =
  mk_vfun "name -> lens" fconst_qid
    (fun v ->
       mk_nfun "lens" fconst_qid 
         (fun n -> 
            let l,ck = fconst v n in 
              Value.L (l,ck)))
let _ = register_native fconst_qid "tree -> name -> lens" fconst_lib

(*** FMODIFY ***)
let fmodify_qid = "Native.Prelude.fmodify"
let fmodify n cmd =
  let lens = 
    { get = (fun c -> V.set c n None);
      put = (fun a co ->
               match co with 
	           Some c ->
	             let a' = V.set c n None in
	               if V.equal a a' then
	                 c
	               else begin
	                 match output_from cmd with
	                     Some value -> 
		               V.set a n (Some (V.new_value value))
	                   | None ->
		               error [`String (fmodify_qid ^ "(put): cmd");
			              `String cmd;
			              `String "returned with non-zero value"]
	               end
                 | None ->
	             match output_from cmd with
	                 Some value -> 
	                   V.set a n (Some (V.new_value value))
	               | None ->
	                   error [`String (fmodify_qid ^ "(put): cmd");
		                  `String cmd;
		                  `String "returned with non-zero value"])} in
    (lens, unchecked fmodify_qid)

let fmodify_lib =
  mk_nfun "name -> lens" fmodify_qid
    (fun n ->
       mk_nfun "lens" fmodify_qid 
         (fun cmd -> 
            let l,ck = fmodify n cmd in 
              Value.L (l,ck)))
let _ = register_native fmodify_qid "name -> name -> lens" fmodify_lib

(* force loading when compiled in a library *)
let init () = ()

