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
let hd_tag = Tree.hd_tag
let hd_tag_lib = Value.N (hd_tag)
let _ = register_native "Native.Prelude.hd_tag" SName hd_tag_lib
let tl_tag = Tree.tl_tag
let tl_tag_lib = Value.N (tl_tag)
let _ = register_native "Native.Prelude.tl_tag" SName tl_tag_lib
let nil_tag = Tree.nil_tag
let nil_tag_lib = Value.N (nil_tag)
let _ = register_native "Native.Prelude.nil_tag" SName nil_tag_lib

(* these need to be baked in here, because the parser uses them *)
let cons_qid = "Native.Prelude.Cons" 
let cons h t = Treeschema.mk_cons h t
let cons_lib = 
  mk_sfun (SSchema ^> SSchema) cons_qid 
    (fun h -> mk_sfun SSchema cons_qid 
      (fun t ->
        let i = Info.M "cons" in
        Value.S (
          Schema.treeschema (
            cons (Schema.treeschema_of i h) (Schema.treeschema_of i t)))))
let _ = register_native cons_qid (SSchema ^> SSchema ^> SSchema) cons_lib

let nil_qid = "Native.Prelude.Nil"
let nil = Treeschema.mk_nil
let nil_lib = Value.S (Schema.treeschema nil)
let _ = register_native nil_qid (SSchema) nil_lib

let any_qid = "Native.Prelude.Any"
let any = Treeschema.mk_any
let any_lib = Value.S (Schema.treeschema any)
let _ = register_native any_qid (SSchema) any_lib

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
  mk_nfun (SName) read_qid (fun fn -> Value.N (read fn))
let _ = register_native read_qid (SName ^> SName) read_lib

(* load *)  
let load_qid = "Native.Prelude.load"
let load ekey blob = 
  let ekey = Surveyor.get_ekey (Some ekey) "" (Some blob) in
    (Surveyor.get_reader ekey) (Surveyor.FromString blob)
let load_lib =
  mk_nfun (SName ^> SView) load_qid
  (fun ekey -> mk_nfun (SView) load_qid
              (fun blob -> Value.V (load ekey blob)))
let _ = register_native load_qid (SName ^> SName ^> SView) load_lib

(* load_file *)
let load_file_qid = "Native.Prelude.load_file"
let load_file fn = 
  let (fn,ekeyo) = Surveyor.parse_filename fn in
  let contents = read fn in
  let ekey = Surveyor.get_ekey ekeyo fn (Some contents) in
    load ekey (contents)
let load_file_lib =
  mk_nfun (SView) load_file_qid (fun fn -> Value.V (load_file fn))
let _ = register_native load_file_qid (SName ^> SView) load_file_lib

(* get *)
let get_qid = "Native.Prelude.get"
let get (l,_) c = Lens.get l c
let get_lib =
  mk_lfun (SView ^> SView) get_qid
    (fun lc -> mk_vfun (SView) get_qid (fun c -> Value.V (get lc c)))
let _ = register_native get_qid (SLens ^> SView ^> SView) get_lib

(* put *)
let put_qid = "Native.Prelude.put"
let put l a c = Lens.put l a (Some c)
let put_lib =
  mk_lfun (SView ^> SView ^> SView) put_qid
    (fun (l,_) -> 
       mk_vfun (SView ^> SView) put_qid 
         (fun a -> mk_vfun (SView) put_qid 
            (fun c -> Value.V (put l a c))))
let _ = register_native put_qid (SLens ^> SView ^> SView ^> SView) put_lib

(* create *)
let create_qid = "Native.Prelude.create"
let create l a = Lens.put l a None
let create_lib =
  mk_lfun (SView ^> SView) create_qid
    (fun (l,_) -> mk_vfun (SView) create_qid
       (fun a -> Value.V (create l a)))
let _ = register_native create_qid (SLens ^> SView ^> SView) create_lib

(* sync *)
let sync_qid = "Native.Prelude._sync"
let sync lo_v la_v lb_v typ_v orig_v = 
  let orig = V.tree_of (Info.M sync_qid) orig_v in
  let typ = Schema.treeschema_of (Info.M sync_qid) typ_v in
  let lo = Lens.tree_of_v lo_v in
  let la = Lens.tree_of_v la_v in
  let lb = Lens.tree_of_v lb_v in
  let co,ca,cb =
    try
      (Tree.get_required ~msg:"sync1" orig "O", 
       Tree.get_required ~msg:"sync2" orig "A", 
       Tree.get_required ~msg:"sync3" orig "B")
    with
        (Error.Harmony_error _) -> 
          error [`String sync_qid
                ; `String ":the initial tree,"
                ; `Tree orig
                ; `String "should have children 'O', 'A', and 'B'"
                ] in
  let log = (Tree.get orig "log" <> None) in
  let ao,aa,ab = (Lens.get lo co, Lens.get la ca, Lens.get lb cb) in
  let a,ao',aa',ab' = Sync.sync typ (Some ao, Some aa, Some ab) in
  let (ao',aa',ab') = match (ao',aa',ab') with 
      Some(ao'),Some(aa'),Some(ab') -> (ao',aa',ab') 
    | _ -> assert false in
  if log then begin Sync.format_action a; Format.printf "@\n" end;
  let co',ca',cb' = (Lens.put lo ao' (Some co), Lens.put la aa' (Some ca), Lens.put lb ab' (Some cb)) in
  V.Tree (Tree.from_list [("O", co'); ("A",ca'); ("B",cb')])

let sync_lib = mk_lfun (SLens ^> SLens ^> SSchema ^> SView ^> SView) sync_qid 
  (fun (lo,_) -> mk_lfun (SLens ^> SSchema ^> SView ^> SView) sync_qid
     (fun (la,_) -> mk_lfun (SSchema ^> SView ^> SView) sync_qid
        (fun (lb,_) -> mk_sfun (SView ^> SView) sync_qid 
           (fun typ -> mk_vfun (SView) sync_qid 
              (fun orig -> V (sync lo la lb typ orig))))))

let _ = register_native sync_qid (SLens ^> SLens ^> SLens ^> SSchema ^> SView
^> SView) sync_lib   

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

let probe_lib = mk_nfun (SLens) probe_qid 
  (fun n -> 
     let l,ck = probe n in 
       Value.L(l,ck))

let _ = register_native probe_qid (SName ^> SLens) probe_lib

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
let progress_lib = mk_nfun (SLens) progress_qid 
  (fun n -> 
     let l,ck = progress n in 
       Value.L (l,ck))
let _ = register_native progress_qid (SName ^> SLens) progress_lib

(* TRACEPOINT *)
let tracepoint_qid = "Native.Prelude.tracepoint"
let tracepoint n lc = 
  let lc_lens, lc_checker = lc in 
  let lens = Lens.tracepoint n lc_lens in
    (lens,lc_checker)

let tracepoint_lib = 
  mk_nfun (SLens ^> SLens) tracepoint_qid
    (fun n ->
       mk_lfun (SLens) tracepoint_qid 
         (fun lc -> 
            let (l,ck) = tracepoint n lc in 
              Value.L (l,ck)))
let _ = register_native tracepoint_qid (SName ^> SLens ^> SLens) tracepoint_lib

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

let invert_lib = mk_lfun (SLens) invert_qid 
  (fun lc -> 
     let l,ck = invert lc in 
       Value.L (l,ck))

let _ = register_native invert_qid (SLens ^> SLens) invert_lib

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
let _ = register_native id_qid (SLens) id_lib

(*** CONST ***)
let const_qid = "Native.Prelude.const"
let const v d =
  let lens = 
    { get = (fun c -> v);
      put = (fun a co ->
               if Tree.equal a v then
                 match co with
                   | None -> d
                   | Some(c) -> c
               else error [`String (const_qid ^ "(put): abstract tree"); 
                           `Space; `Tree a; `Space;
                           `String "is not equal to"; `Tree v]) } in
  let checker = 
    VWB (fun c -> 
           if not (Treeschema.member d c) then
             error [`String const_qid; `Space;
                    `Tree d; `Space; 
                    `String "is not a member of"; `Space;
                    `Prim (fun () -> Treeschema.format_t c)]
           else Treeschema.t_of_tree v) in 
    (lens,checker)

let const_lib =
  mk_vfun (SView ^> SLens) const_qid
    (fun v ->
      mk_vfun (SLens) const_qid 
        (fun d -> 
          let i = Info.M "const" in
          let l,ck =
            Value.v_of_tree_lens (
              const (V.tree_of i v) (V.tree_of i d))
          in 
            Value.L (l,ck)))
let _ = register_native const_qid (SView ^> SView ^> SLens) const_lib

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
  mk_lfun (SLens ^> SLens) compose2_qid
    (fun lc1 ->
       mk_lfun (SLens) compose2_qid 
         (fun lc2 -> 
            let l,ck = compose2 lc1 lc2 in 
              Value.L (l,ck)))
let _ = register_native compose2_qid (SLens ^> SLens ^> SLens) compose2_lib

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
                 Tree.fold
                   (fun k ck bindacc ->
                      let ck' = (l.get ck) in
                        ((k,Some ck')::bindacc))
                   c [] in
                 Tree.create_star binds);
      put =
        (* we can do a type-based optimization here, but i haven't done it -JNF *)
        (fun a co ->
           let c = match co with None -> Tree.empty | Some c -> c in
            let cbinds = Tree.fold
             (fun k vk bindacc ->
                (k,
                 begin match (Tree.get c k),(Tree.get a k) with
                   | Some ck, Some ak -> Some ((l.put ak (Some ck)))
                   | Some ck, None -> None
                   | None, Some ak -> Some ((l.put ak None))
                   | _ -> assert false 
                 end)::bindacc)
               a [] in
             Tree.create_star cbinds)} in 
  let checker = 
    let mk_map_checker ck c = 
      match Treeschema.project_all c with
          None -> error [`String map_qid; `Space;
                         `String "expected schema with consistent subschema"; `Space;
                         `String "found:"; `Space;
                         `Prim (fun () -> Treeschema.format_t c)]
        | Some c1 -> 
            let a1 = ck c1 in 
              Treeschema.inject c a1 in  
      match c with 
          BIJ(c2a,a2c)       -> BIJ(mk_map_checker c2a, mk_map_checker a2c)
        | VWB(c2a) | WB(c2a) -> WB(mk_map_checker c2a) in 
    (lens, checker)

(* map - library interface *)
let map_lib = 
  mk_lfun (SLens) map_qid 
    (fun lc -> 
      let l,ck =
        Value.v_of_tree_lens (
          map (Value.tree_of_v_lens lc))
      in 
        Value.L (l,ck))
let _ = register_native map_qid (SLens ^> SLens) map_lib

(*** WMAP ***)
(* wmap - native interface *)
let wmap_qid = "Native.Prelude.wmap"
let wmap fm = 
  let lens = 
    (* do we really need this memo table? *)
    let lookup =
      let memo = Hashtbl.create 11 in
        (fun k -> 
           try Hashtbl.find memo k 
           with Not_found -> 
             let res,_ = Value.tree_of_v_lens (Name.Map.safe_find k fm id) in 
               Hashtbl.add memo k res; res) in
      { get = 
          (fun c -> let binds = Tree.fold
             (fun k ck bindacc ->
                let lens = lookup k in 
                let ck' = (lens.get ck) in
                  ((k,Some ck')::bindacc))
             c [] in
             Tree.create_star binds);
        put =
          (fun a co -> 
             let c = match co with None -> Tree.empty | Some c -> c in
             let cbinds = Tree.fold
               (fun k vk bindacc ->
                  (k,
                   begin match (Tree.get c k),(Tree.get a k) with
                     | Some ck, Some ak -> Some ((lookup k).put ak (Some ck))
                     | Some ck, None -> None
                     | None, Some ak -> Some ((lookup k).put ak None)
                     | _ -> assert false
                   end)::bindacc)
                 a [] in
               Tree.create_star cbinds) } in 
  let checker =     
    let empty_view_schema = Treeschema.mk_cat [] in 
    let mk_wmap_checker ck_get s = 
      let ns = Name.Map.domain fm in 
      let schema_map = Name.Set.fold 
        (fun ni acc -> 
           let ni_set = Name.Set.singleton ni in
           let s_ni,_ = Treeschema.restrict ni_set s in
             if Treeschema.equivalent s_ni empty_view_schema then
               Name.Map.add ni Treeschema.mk_empty acc
             else match Treeschema.project_all s_ni with 
                 Some s_ni_x -> 
                   let _,ck = Value.tree_of_v_lens (Name.Map.find ni fm) in
                   let checker = match ck_get,ck with 
                       true,BIJ(x,_) | true,VWB(x) | true,WB(x) -> x
                     | false,BIJ(_,x) -> x
                     | false,_ -> assert false in 
                     Name.Map.add ni (checker s_ni_x) acc
               | None -> error [`String map_qid; `Space;
                                `String "expected schema with consistent subschema"; `Space;
                                `String "found:"; `Space;
                                `Prim (fun () -> Treeschema.format_t s_ni)])
        ns Name.Map.empty in
        Treeschema.inject_map s schema_map in
    let checker_sort = Name.Map.fold 
      (fun _ (_,ck) acc -> 
         match ck,acc with 
             _,BIJ(_)      -> ck
           | BIJ(_),_      -> acc
           | _,VWB(_)      -> ck
           | VWB(_),_      -> acc
           | WB(_),WB(_)   -> ck)
      fm id_checker in 
      match checker_sort with 
          BIJ(_) -> BIJ(mk_wmap_checker true, mk_wmap_checker false)
        | VWB(_) -> VWB(mk_wmap_checker true)
        | WB(_)  -> WB(mk_wmap_checker true) 
  in
    (lens, checker)

let wmap_lib = 
  mk_fmfun (SLens) wmap_qid 
    (fun m -> 
       let l,ck = Value.v_of_tree_lens (wmap m) in 
         Value.L (l,ck))
let _ = register_native wmap_qid (SMap ^> SLens) wmap_lib

(* XFORK *)
let xfork_qid = "Native.Prelude.xfork"
let xfork pcv pav (l1,c1) (l2,c2) = 
  let dom_pcv = Tree.dom pcv in (* FIXME: check that pcv and pca have height <= 1? *)
  let dom_pav = Tree.dom pav in
  let pc = fun n -> Name.Set.mem n dom_pcv in
  let pa = fun n -> Name.Set.mem n dom_pav in
  let lens = 
    { get = 
        (fun c ->
           let c1,c2 = Tree.split pc c in
           let a1 = l1.get c1 in
           let a2 = l2.get c2 in
             if not(Name.Set.for_all pa (Tree.dom a1)) then
               error [`String xfork_qid; `String "(get): l1 yielded a child not ";
                       `String "satisfying pa"; 
                      `Tree a1];
             if not(Name.Set.for_all (fun k -> not (pa k)) (Tree.dom a2)) then
               error [`String xfork_qid; `String "(get): l2 yielded a child satisfying pa";
                      `Tree a2];
             Tree.concat a1 a2);
      put = (fun a co -> 
               let co1,co2 =
                 match co with None -> None,None
                   | Some c -> let c1,c2 = Tree.split pc c in (Some c1,Some c2) in
               let a1, a2 = Tree.split pa a in
               let c1' = l1.put a1 co1 in
               let c2' = l2.put a2 co2 in
                 if not(Name.Set.for_all pc (Tree.dom c1')) then
                   error [`String xfork_qid; `String "(put): l1 yielded a child ";
                          `String "not satisfying pc"; 
                          `Tree c1'];
                 if not(Name.Set.for_all (fun k -> not (pc k)) (Tree.dom c2')) then
                   error [`String xfork_qid; `String "(put): l2 yielded a child ";
                          `String "satisfying pc"; 
                          `Tree c2'];
                 Tree.concat c1' c2')} in 
  let checker = 
    let aux pc ck1 ck2 = 
      fun s -> 
        if Treeschema.empty s then s 
        else let s_pc,s_neg_pc = Treeschema.restrict pc s in 
          Treeschema.mk_cat[ck1 s_pc; ck2 s_neg_pc] in
      match c1,c2 with
          BIJ(c2a,a2c),BIJ(c2a',a2c') -> 
            BIJ(aux dom_pcv c2a c2a', aux dom_pav a2c a2c')
        | BIJ(c2a,_),VWB(c2a') | VWB(c2a),BIJ(c2a',_) | VWB(c2a),VWB(c2a') -> 
            VWB(aux dom_pcv c2a c2a')
        | BIJ(c2a,_),WB(c2a') | WB(c2a),BIJ(c2a',_) | VWB(c2a),WB(c2a') | WB(c2a),VWB(c2a') | WB(c2a),WB(c2a') -> 
            WB(aux dom_pcv c2a c2a') in
    (lens, checker)

let xfork_lib = 
  mk_vfun (SView ^> SLens ^> SLens ^> SLens) xfork_qid
    (fun pcv ->
      mk_vfun (SLens ^> SLens ^> SLens) xfork_qid
        (fun pav ->
          mk_lfun (SLens ^> SLens) xfork_qid
            (fun l1 ->
              mk_lfun (SLens) xfork_qid 
                (fun l2 -> 
                  let i = Info.M "xfork" in
                  let l,ck =
                    Value.v_of_tree_lens (
                      xfork
                        (V.tree_of i pcv)
                        (V.tree_of i pav)
                        (Value.tree_of_v_lens l1)
                        (Value.tree_of_v_lens l2))
                  in 
                    Value.L (l,ck)))))
let _ = register_native xfork_qid (SView ^> SView ^> SLens ^> SLens ^> SLens) xfork_lib

(* BFORK *)
let bfork_qid = "Native.Prelude.bfork"
let bfork sc pca lc1 lc2 =
  let lens = 
    let (l1,_),(l2,_) = lc1,lc2 in 
    let dom_pav = Tree.dom pca in
    let pc = fun v -> Treeschema.member v sc in
    let pa = fun n -> Name.Set.mem n dom_pav in
      { get = 
          (fun c ->
             let c1l, c2l = Safelist.partition (fun (n,v) -> pc v) (Tree.to_list c) in
             let c1,c2 = (Tree.from_list c1l, Tree.from_list c2l) in
             let a1 = l1.get c1 in
             let a2 = l2.get c2 in
               if not(Name.Set.for_all pa (Tree.dom a1)) then
                 error [`String bfork_qid; `String "(get): l1 yielded a child not ";
                         `String "satisfying pa"; 
                        `Tree a1];
               if not(Name.Set.for_all (fun k -> not (pa k)) (Tree.dom a2)) then
                 error [`String bfork_qid; `String "(get): l2 yielded a child satisfying pa";
                        `Tree a2];
               Tree.concat a1 a2);
        put = (fun a co -> 
                 let co1,co2 =
                   match co with None -> None,None
                     | Some c -> 
                         let c1l, c2l = Safelist.partition (fun (n,v) -> pc v) (Tree.to_list c) in
                           (Some (Tree.from_list c1l), Some(Tree.from_list c2l)) in
                 let a1, a2 = Tree.split pa a in
                 let c1' = l1.put a1 co1 in
                 let c2' = l2.put a2 co2 in
                   if not(Safelist.for_all (fun (_,v) -> pc v) (Tree.to_list c1')) then
                     error [`String bfork_qid; `String "(put): l1 yielded a child ";
                            `String "not satisfying sc"; 
                            `Tree c1'];
                   if not(Safelist.for_all (fun (_,v) -> not (pc v)) (Tree.to_list c2')) then
                     error [`String xfork_qid; `String "(put): l2 yielded a child ";
                            `String "satisfying pc"; 
                            `Tree c2'];
                   Tree.concat c1' c2')} in 
    (lens, unchecked bfork_qid)

let bfork_lib = 
  mk_sfun (SView ^> SLens ^> SLens ^> SLens) bfork_qid
    (fun c ->
      mk_vfun (SLens ^> SLens ^> SLens) bfork_qid
        (fun pav ->
          mk_lfun (SLens ^> SLens) bfork_qid
            (fun l1 ->
              mk_lfun (SLens) bfork_qid 
                (fun l2 -> 
                  let i = Info.M "bfork" in
                  let l,ck =
                    Value.v_of_tree_lens (
                      bfork
                        (Schema.treeschema_of i c)
                        (V.tree_of i pav)
                        (Value.tree_of_v_lens l1)
                        (Value.tree_of_v_lens l2))
                  in
                    Value.L(l,ck)))))
let _ = register_native bfork_qid (SSchema ^> SView ^> SLens ^> SLens ^> SLens) bfork_lib

(* HOIST *)
let hoist_qid = "Native.Prelude.hoist"
let hoist k =
  let lens = 
    { get = 
        (fun c ->
           if   (Name.Set.cardinal (Tree.dom c)) <> 1 
             or (Name.Set.choose (Tree.dom c)) <> k then
               error [`String hoist_qid
                     ; `String "(get): expecting exactly one child named"
                     ; `Break
                     ; `Name k
                     ; `Break
                     ; `Tree c];
           Tree.get_required ~msg:"hoist" c k);
      put = 
        (fun a _ -> 
           Tree.set Tree.empty k (Some a)) } in 
  let c2a c = 
    match 
      Treeschema.project k c, 
      Treeschema.subschema c 
        (Treeschema.mk_wild (Name.Set.empty) 1 false Treeschema.mk_any) 
    with     
        None,_ | _,false -> 
          error [`String hoist_qid;`Space;
                 `String "may only be used with schemas of the form {n=A}, for some A"]
      | Some a,true -> a in 
  let a2c a = Treeschema.mk_atom k a in
    (lens, BIJ(c2a,a2c))

let hoist_lib = 
  mk_nfun (SLens) hoist_qid 
    (fun k -> 
      let l,ck = Value.v_of_tree_lens (hoist k) in 
      Value.L (l,ck))    
let _ = register_native hoist_qid (SName ^> SLens) hoist_lib

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
             try Tree.get_required ~msg:"copy" c m
             with (Error.Harmony_error _) ->
               error [`String copy_qid
                     ; `String "(get): expecting one child named"
                     ; `Break
                     ; `Name m 
                     ; `String ")" 
                     ; `Tree c] in
             Tree.set c n (Some child)) ;
      put = 
        (fun a _ -> 
           if (try Tree.equal 
                 (Tree.get_required  ~msg:"copy "a m) 
                 (Tree.get_required ~msg:"copy" a n)
               with (Error.Harmony_error _) -> 
                 error [`String copy_qid
                       ; `String "(put): expecting two children named "
                       ; `Name m
                       ; `String "and"
                     ; `Name n
                     ; `Tree a])
         then Tree.set a n None
           else 
             error [`String copy_qid; `String "(put): expecting two equal children named ";
                    `Name m; 
                    `String " and ";
                    `Name n;
                    `Tree a]) } in  
    (lens, unchecked copy_qid)

let copy_lib =
  mk_nfun (SName ^> SLens) copy_qid
    (fun m ->
      mk_nfun (SLens) copy_qid 
        (fun n -> 
          let l,ck = Value.v_of_tree_lens (copy m n) in 
            Value.L (l,ck)))
let _ = register_native copy_qid (SName ^> SName ^> SLens) copy_lib

(* MERGE *)
let merge_qid = "Native.Prelude.merge"
let merge m n =
  let lens = 
    { get = (fun c -> Tree.set c n None) ;
      put = 
        (fun a ->
           function
             | None -> 
                 (try Tree.set a n (Some (Tree.get_required ~msg:"merge" a m))
                  with (Error.Harmony_error _) -> 
                    error
                      [ `String merge_qid
                      ; `String "(put): expecting a child named "
                      ; `Name m])
             | Some c ->
                 let cmo,cno = (Tree.get c m), (Tree.get c n) in
                 let eqCmCn = 
                   match cmo,cno with
                     | None, None -> true
                     | None, Some _ -> false
                     | Some _, None -> false
                     | Some cm, Some cn -> Tree.equal cm cn
                 in
                   if (eqCmCn) 
                   then Tree.set a n (Tree.get a m)
                   else Tree.set a n cno)} in 
  let checker =
    VWB(fun c -> 
          let mn = Name.Set.add m (Name.Set.singleton n) in 
          let c_mn,c_rest = Treeschema.restrict mn c in 
          let d = match Treeschema.project_all c_mn with 
              None -> error [`String merge_qid; `Space
                            ; `String "may only be used with a schema of the form (C1 + { ?m=D, ?n=D })"]
            | Some d -> d in 
            Treeschema.mk_cat [c_rest; Treeschema.mk_atom m d]) in    
    (lens, checker)

let merge_lib =
  mk_nfun (SName ^> SLens) merge_qid
    (fun m ->
      mk_nfun (SLens) merge_qid 
        (fun n -> 
          let l,ck = Value.v_of_tree_lens (merge m n) in 
            Value.L (l,ck)))
let _ = register_native merge_qid (SName ^> SName ^> SLens) merge_lib

(****************)
(* Conditionals *)
(****************)

(* COND *)
let cond_qid = "Native.Prelude.cond"
let cond_impl c1 a1 a2 f21o f12o (lt,ct) (lf,cf) = 
  let lens = 
    { get = 
        (fun cv ->
           if Treeschema.member cv c1 then lt.get cv
           else lf.get cv);
      put = 
        (fun a co ->
           if Treeschema.member a a1 then
             if Treeschema.member a a2 then
               match co with
                 | None -> lf.put a co
                 | Some cv ->
                     if Treeschema.member cv c1
                     then lt.put a co
                     else lf.put a co
             else
               match co with
                 | None -> lt.put a co
                 | Some cv ->
                     if Treeschema.member cv c1
                     then lt.put a co
                     else lt.put a (match f21o with 
                                      | Some (l,_) -> (Some (l.get cv))
                                      | None   -> None)
           else
             if Treeschema.member a a2 then
               match co with
                 | None -> lf.put a co
                 | Some cv ->
                     if Treeschema.member cv c1
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
      let c_inter_c1 = Treeschema.mk_isect [c;c1] in 
      let c_minus_c1 = Treeschema.mk_diff c c1 in 
      let calc_a1 = c2a c_inter_c1 in 
      let calc_a2 = c2a' c_minus_c1 in 
      let _ = 
        (* check that abstract schemas are subschemas of the ones written down *)
        if not (Treeschema.subschema calc_a1 a1) then 
          error [`String cond_qid; `Space;
                 `String "the abstract schema of l1 must be a subschema of a1"];
        if not (Treeschema.subschema calc_a2 a2) then 
          error [`String cond_qid; `Space;
                 `String "the abstract schema of l2 must be a subschema of a1"] in
      let _ = 
        (* check fixup functions *)
        let aux msg fi2jo ci cj = match fi2jo with 
            None -> ()
          | Some (_,BIJ(i2j,_)) | Some(_,VWB(i2j)) | Some(_,WB(i2j)) -> 
              if not (Treeschema.subschema (i2j ci) cj) then 
                error [`String cond_qid; `Space;
                       `String msg; `Space;
                       `String "has an incorrect type"] in
          aux "f21" f21o c_minus_c1 c_inter_c1;
          aux "f12" f12o c_inter_c1 c_minus_c1 in
        Treeschema.mk_union [calc_a1;calc_a2] in        
      match ct,cf with
          BIJ(c2a,a2c),BIJ(c2a',a2c') ->               
            (* the cond is bijective iff a1 and a2 are disjoint *)
            if Treeschema.empty (Treeschema.mk_isect [a1;a2]) then 
              let ck_a2c a = 
                let a_inter_a1 = Treeschema.mk_isect [a;a1] in 
                let a_inter_a2 = Treeschema.mk_isect [a;a2] in 
                let calc_c1 = a2c a_inter_a1 in
                let calc_c2 = a2c a_inter_a2 in 
                  Treeschema.mk_union [calc_c1;calc_c2] in
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
  mk_sfun (SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens ^> SLens) cond_qid
    (fun c ->
      mk_sfun (SSchema ^> SLens ^> SLens ^> SLens ^> SLens ^> SLens) cond_qid
        (fun a1 ->
          mk_sfun (SLens ^> SLens ^> SLens ^> SLens ^> SLens) cond_qid
            (fun a2 ->
              mk_lfun (SLens ^> SLens ^> SLens ^> SLens) cond_qid
                (fun f21 ->
                  mk_lfun (SLens ^> SLens ^> SLens) cond_qid
                    (fun f12 ->
                      mk_lfun (SLens ^> SLens) cond_qid
                        (fun lt -> mk_lfun (SLens) cond_qid 
                          (fun lf -> 
                            let i = Info.M "cond_ff" in
                            let l,ck =
                              Value.v_of_tree_lens (
                                cond_ff
                                  (Schema.treeschema_of i c)
                                  (Schema.treeschema_of i a1)
                                  (Schema.treeschema_of i a2)
                                  (Value.tree_of_v_lens f21)
                                  (Value.tree_of_v_lens f12)
                                  (Value.tree_of_v_lens lt)
                                  (Value.tree_of_v_lens lf))
                            in
                              Value.L (l,ck))))))))
let _ = register_native
  cond_qid
  (SSchema ^> SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens ^> SLens)
  cond_ff_lib
let cond_ww_qid = "Native.Prelude.cond_ww"
let cond_ww_lib =
  mk_sfun (SSchema ^> SSchema ^> SLens ^> SLens ^> SLens) cond_ww_qid
    (fun c ->
      mk_sfun (SSchema ^> SLens ^> SLens ^> SLens) cond_ww_qid
        (fun a1 ->
          mk_sfun (SLens ^> SLens ^> SLens) cond_ww_qid
            (fun a2 ->
              mk_lfun (SLens ^> SLens) cond_ww_qid
                (fun lt -> mk_lfun (SLens) cond_ww_qid 
                  (fun lf -> 
                    let i = Info.M "cond_ww" in
                    let l,ck =
                      Value.v_of_tree_lens (
                        cond_ww
                          (Schema.treeschema_of i c)
                          (Schema.treeschema_of i a1)
                          (Schema.treeschema_of i a2)
                          (Value.tree_of_v_lens lt)
                          (Value.tree_of_v_lens lf))
                    in
                      Value.L (l,ck))))))
let _ = register_native
  cond_ww_qid
  (SSchema ^> SSchema ^> SSchema ^> SLens ^> SLens ^> SLens)
  cond_ww_lib
let cond_fw_qid = "Native.Prelude.cond_fw"
let cond_fw_lib =
  mk_sfun (SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens) cond_fw_qid
    (fun c ->
      mk_sfun (SSchema ^> SLens ^> SLens ^> SLens ^> SLens) cond_fw_qid
        (fun a1 ->
          mk_sfun (SLens ^> SLens ^> SLens ^> SLens) cond_fw_qid
            (fun a2 ->
              mk_lfun (SLens ^> SLens ^> SLens) cond_fw_qid
                (fun f21 ->
                  mk_lfun (SLens ^> SLens) cond_fw_qid
                    (fun lt -> mk_lfun (SLens) cond_fw_qid 
                      (fun lf -> 
                        let i = Info.M "cond_fw" in
                        let l,ck =
                          Value.v_of_tree_lens (
                            cond_fw
                              (Schema.treeschema_of i c)
                              (Schema.treeschema_of i a1)
                              (Schema.treeschema_of i a2)
                              (Value.tree_of_v_lens f21)
                              (Value.tree_of_v_lens lt)
                              (Value.tree_of_v_lens lf))
                        in 
                          Value.L (l,ck)))))))

let _ = register_native
  cond_fw_qid
  (SSchema ^> SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens)
  cond_fw_lib
let cond_wf_qid = "Native.Prelude.cond_wf"
let cond_wf_lib =
  mk_sfun (SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens) cond_wf_qid
    (fun c ->
      mk_sfun (SSchema ^> SLens ^> SLens ^> SLens ^> SLens) cond_wf_qid
        (fun a1 ->
          mk_sfun (SLens ^> SLens ^> SLens ^> SLens) cond_wf_qid
            (fun a2 ->
              mk_lfun (SLens ^> SLens ^> SLens) cond_wf_qid
                (fun f12 ->
                  mk_lfun (SLens ^> SLens) cond_wf_qid
                    (fun lt -> mk_lfun (SLens) cond_wf_qid 
                      (fun lf -> 
                        let i = Info.M "cond_fw" in
                        let l,ck =
                          Value.v_of_tree_lens (
                            cond_wf
                              (Schema.treeschema_of i c)
                              (Schema.treeschema_of i a1)
                              (Schema.treeschema_of i a2)
                              (Value.tree_of_v_lens f12)
                              (Value.tree_of_v_lens lt)
                              (Value.tree_of_v_lens lf))
                        in
                          Value.L (l,ck)))))))

let _ = register_native
  cond_wf_qid
  (SSchema ^> SSchema ^> SSchema ^> SLens ^> SLens ^> SLens ^> SLens)
  cond_wf_lib

(* ACOND *)
let acond c a lt lf = cond_impl c a (Treeschema.mk_neg a) None None lt lf
let acond_qid = "Native.Prelude.acond"
let acond_lib =
  mk_sfun (SSchema ^> SLens ^> SLens ^> SLens) acond_qid
    (fun c ->
      mk_sfun (SLens ^> SLens ^> SLens) acond_qid
        (fun a ->
          mk_lfun (SLens ^> SLens) acond_qid
            (fun lt ->
              mk_lfun (SLens) acond_qid 
                (fun lf -> 
                  let i = Info.M "acond" in
                  let l,ck =
                    Value.v_of_tree_lens (
                      acond
                        (Schema.treeschema_of i c)
                        (Schema.treeschema_of i a)
                        (Value.tree_of_v_lens lt)
                        (Value.tree_of_v_lens lf))
                  in 
                    Value.L (l,ck)))))

let _ = register_native
  acond_qid
  (SSchema ^> SSchema ^> SLens ^> SLens ^> SLens)
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
             let ck = Tree.get_required  ~msg:"pivot" c k in
             let ckv = Tree.get_value ck in
               Tree.set Tree.empty ckv (Some (Tree.set c k None))
           with (Error.Harmony_error _) -> 
             error [`String "Native.Prelude.pivot(get): the following tree should have ";
                    `String "exactly one child named "; 
                    `Name k; 
                    `String ", leading to a value ";
                    `Tree c]);
      put = 
        (fun a _ ->
           if (Name.Set.cardinal (Tree.dom a)) <> 1 then
             error [`String "Native.Prelude.pivot(get): the following tree should have ";
                    `String "exactly one child"; 
                    `Tree a]
           else
             let ak = Name.Set.choose (Tree.dom a) in
             let w = try Tree.get_required ~msg:"pivot" a ak with (Error.Harmony_error f) -> f (); assert false in
               if Tree.get w k <> None then
                 error [`String "Native.Prelude.pivot(put): child"; `Space
                       ; `Name k; `Space
                       ; `String "of this tree should not exist:"; `Space
                       ; `Tree w]
               else
                 Tree.set w k (Some (Tree.new_value ak))
        )} in
  let checker = 
    let mk_skinny s = Treeschema.mk_wild Name.Set.empty 1 false s in 
    let value = mk_skinny (Treeschema.mk_cat []) in 
    let c2a c = 
      let c_k,c_rest = Treeschema.restrict (Name.Set.singleton k) c in
      let _ = 
        let err () = error [`String pivot_qid; `Space
                           ; `String "may only be used with a shema that has a child"; `Space
                           ; `Name k; `Space
                           ; `String "leading to a value"] in          
          match Treeschema.project k c_k with
              None -> err ()
            | Some c_k -> 
                if not (Treeschema.equivalent c_k value) then err () in
        mk_skinny c_rest in 
    let a2c a = 
      if not (Treeschema.subschema a (mk_skinny Treeschema.mk_any)) then 
        error [`String pivot_qid; `Space
              ; `String "may only be used with a shema of the form {!=A}"];
      match (Treeschema.project_all a) with 
          None -> assert false
        | Some a_bang -> Treeschema.mk_cat [Treeschema.mk_atom k value; a_bang] in
      BIJ(c2a,a2c) in
    (lens, checker)

let pivot_lib = mk_nfun (SLens) pivot_qid 
  (fun k -> 
     let l,ck = Value.v_of_tree_lens (pivot k) in 
       Value.L(l,ck))
let _ = register_native 
  pivot_qid 
  (SName ^> SLens) 
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
                 if (not (Tree.is_empty b1)) then (Some b1)
                 else if (not (Tree.is_empty b2)) then (Some b2)
                 else None 
               in
                 match bo with
                   | None -> acc
                   | Some b -> 
                       let k = Name.Set.choose (Tree.dom b) in
                       let b1', b2' = (Tree.set b1 k None, Tree.set b2 k None) in
                       let tk1, tk2 = (Tree.get b1 k, Tree.get b2 k) in
                       let tk = 
                         match tk1, tk2 with
                           | None, None       -> assert false                             
                           | Some t1, None    -> Tree.set Tree.empty m1 (Some t1)
                           | None, Some t2    -> Tree.set Tree.empty m2 (Some t2)
                           | Some t1, Some t2 -> Tree.set (Tree.set Tree.empty m2 (Some t2)) m1 (Some t1)
                       in
                         compute_join b1' b2' (Tree.set acc k (Some tk))
             in
             let tm1, tm2 = (Tree.get_required ~msg:"join" c m1, Tree.get_required ~msg:"join" c m2) in         
               compute_join tm1 tm2 Tree.empty
           with (Error.Harmony_error _) -> 
             error [`String "Native.Prelude.join(get): expected tree with children: "; 
                    `Name m1; 
                    `String " and "; 
                    `Name m2]
        );
      put = 
        (fun a co ->
           let init = Tree.set (Tree.set Tree.empty m2 (Some Tree.empty)) m1 (Some Tree.empty) in
           let rec compute_unjoin b acc =
             if (Tree.is_empty b) then acc
             else
               try
                 let k = Name.Set.choose (Tree.dom b) in
                 let tk = Tree.get_required ~msg:"join" b k in
                 let b' = (Tree.set b k None) in
                 let cm1,cm2 = (Tree.get_required ~msg:"join" acc m1,Tree.get_required ~msg:"join" acc m2) in
                 let acc' = 
                   match (Tree.get tk m1, Tree.get tk m2) with
                     | None, None       -> 
                         error [`String "Native.Prelude.join(put): illformed abstract tree"]
                     | Some t1, None    -> 
                         Tree.set acc m1 (Some (Tree.set cm1 k (Some t1)))
                     | None, Some t2    -> 
                         Tree.set acc m2 (Some (Tree.set cm2 k (Some t2)))
                     | Some t1, Some t2 -> 
                         Tree.set 
                           (Tree.set acc m2 (Some (Tree.set cm2 k (Some t2)))) 
                           m1 (Some (Tree.set cm1 k (Some t1)))
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
  mk_nfun (SName ^> SLens) join_qid 
    (fun n1 -> mk_nfun (SLens) join_qid 
       (fun n2 -> 
          let l,ck = Value.v_of_tree_lens (join n1 n2) in 
            Value.L(l,ck)))
let _ = register_native join_qid (SName ^> SName ^> SLens) join_lib

(* FLATTEN *)
let flatten_op_qid = "Native.Prelude.flatten_op"
let flatten_op (l,ck) =   
  let rec get = function 
      c ->         
        if Tree.is_empty_list c then Tree.empty
        else 
          (* Error handling in case of ill-formed list *)
          (* the head that is considered is the get of l *)
          let head = l.get (Tree.get_required ~msg:"flatten" c Tree.hd_tag) in 
          let c' = Tree.get_required ~msg:"flatten" c Tree.tl_tag in
            (* List of labels pointing toward trees *)
            if Name.Set.cardinal (Tree.dom head) = 1 then 
              let c_list = Tree.to_list head in
              let (k,d) = Safelist.hd c_list in
              let a = get c' in
                (* removes spurious *nil labels in output *)
                (* Alan: not needed: the initial test in get already makes sure of this *)
                (*
              let a = if (Tree.is_empty_list a_rec) then Tree.empty else a_rec in *)
              let childk = Tree.get a k in
                begin
                  match childk with
                      None -> 
                        Tree.set a k (Some (Tree.cons d Tree.empty_list))
                    | Some s -> 
                        let a' = Tree.set a k None in
                          Tree.set a' k (Some (Tree.cons d s))
                end 
            else error [`String "Native.Prelude.flatten(get): expected a tree with exactly one child: "; 
                        `Tree head]
  in
  let listify v =
    let v_list = Tree.to_list v in
    let listifystep (k,child) =
      List.map (function x -> (k,x)) (Tree.list_from_structure child)
    in
      Safelist.map (function (k,x) -> 
                      Tree.set Tree.empty k (Some x)) 
        (Safelist.flatten (Safelist.map listifystep v_list)) in
  let rec put a = function
      None -> Tree.structure_from_list (Safelist.map (fun x -> l.put x None) (listify a))
    | Some c -> 
        if Tree.is_empty_list c then put a None
        else 
          let head = Tree.get_required ~msg:"flatten" c Tree.hd_tag in
          let transformed_head = l.get head in
          (* Error handling in case of ill-formed list *)
          let c' = Tree.get_required  ~msg:"flatten" c Tree.tl_tag in
          (* List of labels pointing toward trees *)
          if Name.Set.cardinal (Tree.dom transformed_head) = 1 then 
            let c_list = Tree.to_list transformed_head in
            let (k,d) = Safelist.hd c_list in
            match Tree.get a k with
                None -> put a (Some c')
              | Some ds -> 
                  (* Error handling in case of ill-formed list *)
                  let d' = Tree.get_required  ~msg:"flatten" ds Tree.hd_tag in 
                  let s = Tree.get_required  ~msg:"flatten" ds Tree.tl_tag in
                  if Tree.is_empty_list s then
                    Tree.cons 
                      (l.put (Tree.from_list [k,d']) (Some head)) 
                      (put (Tree.set a k None) (Some c'))
                  else 
                    Tree.cons 
                      (l.put (Tree.from_list [k,d']) (Some head))
                      (put (Tree.set (Tree.set a k None) k (Some s)) (Some c'))
          else error [`String "Native.Prelude.flatten(put): expected a tree with exactly one child: "; 
                      `Tree head]
  in
  let lens = {get = get ;
              put = put } in
    (lens, unchecked flatten_op_qid)

let flatten_op_lib =
  mk_lfun (SLens) flatten_op_qid 
    (fun l -> 
       let l,ck =
         Value.v_of_tree_lens (flatten_op (Value.tree_of_v_lens l)) in 
         Value.L (l,ck))
let _ = register_native flatten_op_qid (SLens ^> SLens) flatten_op_lib

(************)
(* EXPLODE  *)
(************)
let explode_qid = "Native.Prelude.explode"
let explode =
  let rec tree_of_string msg = function
      "" -> []
    | s -> 
        let sh = String.sub s 0 1 and st = String.sub s 1 (String.length s - 1) in
        (Tree.new_value sh)::(tree_of_string msg st)
  and string_of_tree msg = function
      [] -> ""
    | a::q -> 
        if( Name.Set.cardinal (Tree.dom a)) <> 1 then
          error [`String ("Native.Prelude.explode ("^msg^") : expecting exactly one child :");
                 `Tree a
                ];
        let ch = Name.Set.choose (Tree.dom a) in
          if String.length ch <> 1 then
            error [`String ("Native.Prelude.explode (" ^ msg ^ ") : expecting child with a one character name");
                   `Tree a
                  ];
          ch^(string_of_tree msg q)
  in
  let lens = 
    { get =
        (fun c ->
           if( Name.Set.cardinal (Tree.dom c)) <> 1 then
             error [`String " Native.Prelude.explode (get) : expecting exactly one child :";
                    `Tree c];
           let k = Name.Set.choose (Tree.dom c) in
             (* here is the string we have to 'explode' *)
             Tree.structure_from_list (tree_of_string "get" k)
        );
      put = (fun a _ -> Tree.new_value (string_of_tree "put" (Tree.list_from_structure a)))
    } in 
    (lens, unchecked explode_qid)

let explode_lib = 
  let l,ck = Value.v_of_tree_lens explode in 
    Value.L (l,ck)
let _ = register_native explode_qid (SLens) explode_lib

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
               if not (Tree.is_value c) then
                 error [`String ("Native.Prelude.split (get) : expecting exactly one child :"); `Tree c];
               Tree.structure_from_list (Safelist.map Tree.new_value (Misc.split_nonescape sepchar (Tree.get_value c))));
      put = (fun a c ->
               let split = Safelist.map Tree.get_value (Tree.list_from_structure a) in
                 if split=[] then
                   error [`String ("Native.Prelude.split (put) : abstract argument must be non-empty :"); `Tree a];
                 Safelist.iter
                   (fun s -> if String.contains s '\n' then
                      error [`String ("Native.Prelude.split (put): abstract tree contains '");
                             `String sep; `String "': ";
                             `Tree a])
                   split;
                 Tree.new_value (String.concat sep split)) } in 
  let checker =
    let value = Treeschema.mk_wild Name.Set.empty 1 false (Treeschema.mk_cat []) in
    let mk_value_list c =
      let x = split_qid ^ "generated value list" in
      let fresh_x = Syntax.fresh x in 
      let x_t = Treeschema.mk_var fresh_x in
        Treeschema.mark_tvars [x,Info.M x];
        Treeschema.update fresh_x (Treeschema.mk_union [nil;cons c x_t]);
        Treeschema.finalize ();        
        x_t in
    let c2a c = 
      if not (Treeschema.subschema c value) then error 
        [`String split_qid; `Space;
         `String "may only be used with concrete schema that is a subschema of {!={}}"];
      mk_value_list c in
    let a2c a = 
      if not (Treeschema.subschema a (mk_value_list value)) then error
        [`String split_qid; `Space;
         `String "may only be used with the abstract schema (List.T {!={}})"];
      match Treeschema.project hd_tag a with 
          None -> assert false;
        | Some sub_value -> sub_value in 
      BIJ(c2a,a2c) in
    (lens, checker)

let split_lib =
  mk_nfun (SLens) split_qid
    (fun k -> 
       let l,ck = Value.v_of_tree_lens (split k) in
         Value.L (l,ck))
let _ = register_native split_qid (SName ^> SLens) split_lib

(* split an even list in half *)
let even_split_qid = "Native.Prelude.even_split"
let even_split = 
  let check_list v dir = 
    if not (Tree.is_list v) then 
      error [`String (Printf.sprintf "Native.Prelude.even_split (%s): " dir); `Tree v; `String "is not a list"]
  in
  let lens = 
    { get = 
        (fun c -> 
           check_list c "get";
           let l = Tree.list_from_structure c in
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
                   let vl1 = Tree.structure_from_list l1 in
                   let vl2 = Tree.structure_from_list l2 in
                     Tree.structure_from_list [vl1;vl2] 
                 end);    
      put = 
        (fun a co -> 
           check_list a "put";
           match Tree.list_from_structure a with
               [] -> a
             | [l1;l2] -> 
                 check_list l1 "put";
                 check_list l2 "put";
                 Tree.structure_from_list ((Tree.list_from_structure l1) @ (Tree.list_from_structure l2))
             | _ -> 
                 error 
                   [`String "Native.Prelude.even_split (get): "
                   ; `Space
                   ; `Tree a
                   ; `Space
                   ; `String "is not a list of length two"])} in 
    (lens, unchecked even_split_qid)
let even_split_lib = 
  let l,ck = Value.v_of_tree_lens even_split in 
    Value.L (l,ck)
let _ = register_native even_split_qid (SLens) even_split_lib

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
               if Tree.equal a v then
                 match co with
                   | None -> 
                       (match output_from d with
                            Some name -> Tree.new_value name
                          | None -> error [`String (fconst_qid ^ "(put): cmd");
                                           `String d;
                                           `String "returned with non-zero value"])
                   | Some(c) -> c
               else error [`String (fconst_qid ^ "(put): abstract tree");
                           `Tree a;
                           `String "is not equal to"; `Tree (v)]) } in
    (lens, unchecked fconst_qid)

let fconst_lib =
  mk_vfun (SName ^> SLens) fconst_qid
    (fun v ->
      mk_nfun (SLens) fconst_qid 
        (fun n -> 
          let i = Info.M "fconst" in
          let l,ck =
            Value.v_of_tree_lens (
              fconst (V.tree_of i v) n)
          in 
            Value.L (l,ck)))
let _ = register_native fconst_qid (SView ^> SName ^> SLens) fconst_lib

(*** FMODIFY ***)
let fmodify_qid = "Native.Prelude.fmodify"
let fmodify n cmd =
  let lens = 
    { get = (fun c -> Tree.set c n None);
      put = (fun a co ->
               match co with 
                   Some c ->
                     let a' = Tree.set c n None in
                       if Tree.equal a a' then
                         c
                       else begin
                         match output_from cmd with
                             Some value -> 
                               Tree.set a n (Some (Tree.new_value value))
                           | None ->
                               error [`String (fmodify_qid ^ "(put): cmd");
                                      `String cmd;
                                      `String "returned with non-zero value"]
                       end
                 | None ->
                     match output_from cmd with
                         Some value -> 
                           Tree.set a n (Some (Tree.new_value value))
                       | None ->
                           error [`String (fmodify_qid ^ "(put): cmd");
                                  `String cmd;
                                  `String "returned with non-zero value"])} in
    (lens, unchecked fmodify_qid)

let fmodify_lib =
  mk_nfun (SName ^> SLens) fmodify_qid
    (fun n ->
       mk_nfun (SLens) fmodify_qid 
         (fun cmd -> 
            let l,ck = Value.v_of_tree_lens (fmodify n cmd) in 
              Value.L (l,ck)))
let _ = register_native fmodify_qid (SName ^> SName ^> SLens) fmodify_lib

(* force loading when compiled in a library *)
let init () = ()

