(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id$ *)

open Syntax
open Registry
open Value
open Lens

module Rel = Db.Relation
module Pred = Rel.Pred
module Relschema = Dbschema.Relschema
module Fd = Relschema.Fd

(*** General functions ***)

let revise_rel_1 rel r fd =
  let accum rcd = Rel.insert (Fd.revise rcd r fd) in
  Rel.fold accum rel (Rel.create (Rel.fields rel))

let revise_rel rel r fds =
  let accum rcd = Rel.insert (Fd.Set.revise rcd r fds) in
  Rel.fold accum rel (Rel.create (Rel.fields rel))

let merge_rel rel r fds =
  Rel.union (revise_rel rel r fds) r

let type_error (print_msg : unit -> unit) : 'a =
  raise (Error.Harmony_error (fun () ->
    Format.printf "Type Error:@ "; print_msg ()))

let db_replace rn sn (f : Rel.t -> Rel.t) (db : Db.t) : Db.t =
  Db.extend sn (f (Db.lookup rn db)) (Db.remove rn db)

let db_schema_replace
  (error : (unit -> unit) -> 'a) (rn : Name.t) (sn : Name.t)
  (f : Relschema.t -> Relschema.t) (ds : Dbschema.t) : Dbschema.t =
  if not (Dbschema.mem rn ds) then
    error (fun () ->
      Dbschema.format_t ds;
      Format.printf
        "@ has no relational schema associated with \"%s\"" rn)
  else if sn <> rn && Dbschema.mem sn ds then
    error (fun () ->
      Dbschema.format_t ds;
      Format.printf
        "@ already has a relational schema associated with \"%s\"" rn)
  else
    Dbschema.extend sn (f (Dbschema.lookup rn ds)) (Dbschema.remove rn ds)

let unchecked q = 
  WB(fun s -> error [`String q; `Space; `String "is unchecked"])

let error_on_missing (put : 'a -> 'b -> 'b) (a : 'a) (co : 'b option) : 'b =
  match co with
  | None ->
      raise (Lens.error [`String "relatioinal lenses cannot be applied to
        \"missing\""])
  | Some c -> put a c

(*** rename ***)

let rename_qid = "Native.Relational.rename"
let rename_error printer =
  type_error (fun () -> Format.printf "%s:@ " rename_qid; printer ())
let rename rn sn att att' =
  let getfun c = db_replace rn sn (Rel.rename att att') c in
  let putfun a co = db_replace sn rn (Rel.rename att' att) a in
  let rename_schema rn sn att att' ds =
    try
      db_schema_replace rename_error rn sn (Relschema.rename att att') ds
    with
    | Relschema.Attribute_not_found n ->
        let rs = Dbschema.lookup rn ds in
        rename_error (fun () ->
          Relschema.format_t rs;
          Format.printf "@ does not contain the attribute \"%s\"" n)
    | Relschema.Attribute_not_fresh n ->
        let rs = Dbschema.lookup rn ds in
        rename_error (fun () ->
          Relschema.format_t rs;
          Format.printf "@ already contains the attribute \"%s\"" n)
  in
  let c2a = rename_schema rn sn att att' in
  let a2c = rename_schema sn rn att' att in
  (native getfun putfun, BIJ (c2a, a2c))

let rename_lib =
  mk_nfun (SName ^> SName ^> SName ^> SLens) rename_qid (
    fun r ->
      mk_nfun (SName ^> SName ^> SLens) rename_qid (
        fun s ->
          mk_nfun (SName ^> SLens) rename_qid (
            fun m ->
              mk_nfun (SLens) rename_qid (
                fun n ->
                  let l, ck = Value.v_of_db_lens (rename r s m n) in
                  Value.L (l, ck)))))
let () =
  register_native
    rename_qid (SName ^> SName ^> SName ^> SName ^> SLens) rename_lib

(*** select ***)

let select_qid = "Native.Relational.select"
let select_error printer =
  type_error (fun () -> Format.printf "%s:@ " select_qid; printer ())
let select rn fds sn p =
  let getfun c = db_replace rn sn (Rel.select p) c in
  let putfun a c =
    let r = Db.lookup rn c in
    let f s =
      let r0 = merge_rel (Rel.select (Pred.Not p) r) s fds in
      let xx = Rel.diff (Rel.select p r0) s in
      Rel.diff r0 xx
    in
    db_replace sn rn f a
  in
  let c2a =
    let f s =
      let u = Relschema.attributes s in
      let q = Relschema.get_pred s in
      let fds1 = Relschema.get_fdset s in
      if not (Fd.Set.equal fds fds1) then
        select_error (fun () ->
          Fd.Set.format_t fds;
          Format.printf "@ is not equal to@ ";
          Fd.Set.format_t fds1);
      if not (Fd.Set.tree_form fds1) then
        select_error (fun () ->
          Fd.Set.format_t fds;
          Format.printf "@ is not in tree form");
      if not (Pred.ignores p (Fd.Set.outputs fds1)) then
        select_error (fun () ->
          Pred.format_t p;
          Format.printf "@ constrains the outputs of@ ";
          Fd.Set.format_t fds1);
      Relschema.set_pred (
        Relschema.set_fdset (Relschema.create u) fds1
      ) (Pred.Conj (p, q))
    in
    db_schema_replace select_error rn sn f
  in
  (native getfun (error_on_missing putfun), WB c2a)

let select_lib =
  mk_nfun (SFD ^> SName ^> SPred ^> SLens) select_qid (
    fun rn ->
      mk_fdfun (SName ^> SPred ^> SLens) select_qid (
        fun fds ->
          mk_nfun (SPred ^> SLens) select_qid (
            fun sn ->
              mk_pfun (SLens) select_qid (
                fun p ->
                  let l, ck =
                    Value.v_of_db_lens (select rn fds sn p)
                  in
                  Value.L (l, ck)))))
let () =
  register_native
    select_qid (SName ^> SFD ^> SName ^> SPred ^> SLens) select_lib

(*** drop ***)

let drop_qid = "Native.Relational.drop"
let drop_error printer =
  type_error (fun () -> Format.printf "%s:@ " drop_qid; printer ())
let drop rn sn att det dflt =
  let rm x ls = List.filter (( <> ) x) ls in
  let getfun c =
    let flds = Rel.fields (Db.lookup rn c) in
    db_replace rn sn (Rel.project (rm att flds)) c
  in
  let putfun a c =
    let r = Db.lookup rn c in
    let f s =
      let added = Rel.diff s (Rel.project (rm att (Rel.fields r)) r) in
      let rel_dflt = Rel.insert_tuple [ dflt ] (Rel.create [ att ]) in
      let unrevised = Rel.union (Rel.join r s) (Rel.join added rel_dflt) in
      revise_rel_1 unrevised r (Tree.dom det, Name.Set.singleton att)
    in
    db_replace sn rn f a
  in
  let c2a =
    let f s =
      let u = Relschema.attributes s in
      let detset = Tree.dom det in
      let attset = Name.Set.singleton att in
      if not (Name.Set.mem att u) then
        drop_error (fun () ->
          Relschema.format_t s;
          Format.printf "@ does not contain then attribute %s" att);
      let u' = Name.Set.remove att u in
      let p = Relschema.get_pred s in
      let p' = Pred.project u' p in
      let p_att = Pred.project attset p in
      if not (Pred.equiv p (Pred.Conj (p', p_att))) then
        drop_error (fun () ->
          Pred.format_t p;
          Format.printf "@ cannot be decomposed into@ ";
          Pred.format_t p';
          Format.printf "@ and@ ";
          Pred.format_t p_att);
      let rcd_dflt = Name.Map.add att dflt Name.Map.empty in
      if not (Pred.member rcd_dflt (Pred.project attset p)) then
        drop_error (fun () ->
          Pred.format_t p_att;
          Format.printf "@ does not include the record@ ";
          Format.printf "{%s = \"%s\"}" att dflt);
      let fds = Relschema.get_fdset s in
      let fds' =
        Fd.Set.filter (fun fd -> Fd.ranges_over fd u') fds
      in
      let fds_att = Fd.Set.diff fds fds' in
      let valid_split =
        if Name.Set.equal detset attset then
          Fd.Set.is_empty fds_att
        else
          Fd.Set.cardinal fds_att = 1 &&
          Fd.Set.for_all (
            fun (xs, ys) ->
              Name.Set.equal xs detset && Name.Set.equal ys attset) fds_att
      in
      if not valid_split then
        drop_error (fun () ->
          Fd.Set.format_t fds_att;
          Format.printf "@ is not equivalent to@ ";
          Fd.Set.format_t (Fd.Set.singleton (detset, attset)));
      Relschema.set_pred (Relschema.set_fdset (Relschema.create u') fds') p'
    in
    db_schema_replace drop_error rn sn f
  in
  (native getfun (error_on_missing putfun), WB c2a)

let drop_lib =
  mk_nfun (SName ^> SName ^> SView ^> SName ^> SLens) drop_qid (
    fun rn ->
      mk_nfun (SName ^> SView ^> SName ^> SLens) drop_qid (
        fun sn ->
          mk_nfun (SView ^> SName ^> SLens) drop_qid (
            fun att ->
              mk_vfun (SName ^> SLens) drop_qid (
                fun det ->
                  mk_nfun (SLens) drop_qid (
                    fun dflt ->
                      let l, ck =
                        Value.v_of_db_lens (
                          drop rn sn att (V.tree_of (Info.M drop_qid) det) dflt
                        )
                      in
                      Value.L (l, ck))))))
let () =
  register_native
    drop_qid (SName ^> SName ^> SName ^> SView ^> SName ^> SLens) drop_lib


(*
(* List sorting *)
let listsort =
  let getfun c =
    V.structure_from_list (List.sort V.compare (V.list_from_structure c))
  in
  let putfun a co =
    match co with
    | None -> a
    | Some(c) -> if a = getfun c then c else a in
    Lens.native getfun putfun

let () = register_native "Native.listsort" "lens" (L(listsort,no_checker))

let lift_to_opt f xo =
  match xo with
  | None -> None
  | Some(x) -> Some(f x)


(* Binary relational lenses *)

let lift_binary dblens src1 src2 dst =
  let getfun c =
    let dbc = Treedb.tree_to_db c in
    Treedb.db_to_tree (Lens.get (dblens src1 src2 dst) dbc)
  and putfun a co =
    let dba = Treedb.tree_to_db a in
    let dbco = lift_to_opt Treedb.tree_to_db co in
    Treedb.db_to_tree (Lens.put (dblens src1 src2 dst) dba dbco)
  in
  Lens.native getfun putfun

let make_binary_lib fclname lens =
  mk_nfun "name -> name -> lens" fclname 
    (fun n1 -> mk_nfun "name -> lens" fclname
       (fun n2 -> mk_nfun "lens" fclname
          (fun n3 -> L(lens n1 n2 n3,no_checker))))

let register_binary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> lens"
    (make_binary_lib fclname (lift_binary dblens))

let schemas_to_bias_fun fclname sl sr rcd =
  let tr = Treedb.rcd_to_tree rcd in
  let l = Schema.member tr sl
  and r = Schema.member tr sr in
  if l && r then Rlens.Both
  else if l then Rlens.Left
  else if r then Rlens.Right
  else Lens.error
    [ `String(fclname^":")
    ; `Space; `String("schemas provided do not include")
    ; `Space; `Tree(tr)
    ]

let register_schema_schema_binary_dblens fclname dblens =
  register_native fclname "schema -> schema -> name -> name -> name -> lens" (
    mk_sfun "schema -> name -> name -> name -> lens" fclname (
      fun s1 -> mk_sfun "name -> name -> name -> lens" fclname (
        fun s2 -> make_binary_lib fclname (
          lift_binary (dblens (schemas_to_bias_fun fclname s1 s2))))))

let register_tree_tree_binary_dblens fclname dblens =
  register_native fclname "view -> view -> name -> name -> name -> lens" (
    mk_vfun "view -> name -> name -> name -> lens" fclname 
      (fun t1 -> mk_vfun "name -> name -> name -> lens" fclname
         (fun t2 -> make_binary_lib fclname (lift_binary (dblens t1 t2))))
  )

let () =
  register_schema_schema_binary_dblens
    "Native.Relational.union" Dblens.union

let () =
  register_schema_schema_binary_dblens
    "Native.Relational.inter" Dblens.inter

let () =
  register_schema_schema_binary_dblens
    "Native.Relational.diff" Dblens.diff

(* Union *)
(*
let () = register_binary_dblens "Native.Relational.union" Dblens.union
let () = register_binary_dblens "Native.Relational.unionl" Dblens.unionl
let () = register_binary_dblens "Native.Relational.unionr" Dblens.unionr
*)
    (*
(* Intersection *)
let () = register_binary_dblens "Native.Relational.inter" Dblens.inter
let () = register_binary_dblens "Native.Relational.interl" Dblens.interl
let () = register_binary_dblens "Native.Relational.interr" Dblens.interr

(* Difference *)
let () = register_binary_dblens "Native.Relational.diff" Dblens.diff
let () = register_binary_dblens "Native.Relational.diffl" Dblens.diffl
let () = register_binary_dblens "Native.Relational.diffr" Dblens.diffr
    *)

    (*
(* Join *)
let () = register_binary_dblens "Native.Relational.join" Dblens.join
let () = register_binary_dblens "Native.Relational.joinl" Dblens.joinl
let () = register_binary_dblens "Native.Relational.joinr" Dblens.joinr
*)
let () =
  register_schema_schema_binary_dblens
    "Native.Relational.ijoin" Dblens.ijoin

    (*
(* Outer join *)
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoin" (
    fun dv1 dv2 ->
      Dblens.ojoin (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoinl" (
    fun dv1 dv2 ->
      Dblens.ojoinl (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoinr" (
    fun dv1 dv2 ->
      Dblens.ojoinr (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )
*)

let () =
  let fclname = "Native.Relational.ojoin" in
  register_native fclname
  "view -> view -> schema -> schema -> schema -> schema -> name -> name -> name -> lens" (
    mk_vfun
    "view -> schema -> schema -> schema -> schema -> name -> name -> name -> lens" fclname (
      fun t1 ->
        mk_vfun
        "schema -> schema -> schema -> schema -> name -> name -> name -> lens" fclname (
          fun t2 ->
            mk_sfun "schema -> schema -> schema -> name -> name -> name -> lens" fclname (
              fun s1 ->
                mk_sfun "schema -> schema -> name -> name -> name -> lens" fclname (
                  fun s2 ->
                    mk_sfun "schema -> name -> name -> name -> lens" fclname (
                      fun s3 ->
                        mk_sfun "name -> name -> name -> lens" fclname (
                          fun s4 -> make_binary_lib fclname (
                            lift_binary (
                              Dblens.ojoin
                                (Treedb.tree_to_rel t1)
                                (Treedb.tree_to_rel t2)
                                (fun r -> Schema.member (Treedb.rcd_to_tree r) s1)
                                (fun r -> Schema.member (Treedb.rcd_to_tree r) s2)
                                (schemas_to_bias_fun fclname s3 s4)
                            )))))))))


(* Unary relational lenses *)

let lift_unary dblens src dst =
  let getfun c =
    let dbc = Treedb.tree_to_db c in
    Treedb.db_to_tree (Lens.get (dblens src dst) dbc)
  and putfun a co =
    let dba = Treedb.tree_to_db a in
    let dbco = lift_to_opt Treedb.tree_to_db co in
    Treedb.db_to_tree (Lens.put (dblens src dst) dba dbco)
  in
  Lens.native getfun putfun

let make_unary_lib fclname lens =
  mk_nfun "name -> lens" fclname
    (fun n1 -> mk_nfun "lens" fclname
       (fun n2 -> L(lens n1 n2,no_checker)))

let register_name_name_unary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> name -> lens" (
    mk_nfun "name -> name -> name -> lens" fclname 
      (fun n1' -> mk_nfun "name -> name -> lens" fclname
         (fun n2' -> make_unary_lib fclname (lift_unary (dblens n1' n2'))))
  )

let register_schema_unary_dblens fclname dblens =
  register_native fclname "schema -> name -> name -> lens" (
    mk_sfun "name -> name -> lens" fclname (
      fun s -> make_unary_lib fclname (
        lift_unary (dblens (
          fun rcd -> (Schema.member (Treedb.rcd_to_tree rcd)) s)))))

let register_tree_tree_tree_unary_dblens fclname dblens =
  register_native fclname "view -> view -> view -> name -> name -> lens" (
    mk_vfun "view -> view -> name -> name -> lens" fclname 
      (fun v1 -> mk_vfun "view -> name -> name -> lens" fclname
         (fun v2 -> mk_vfun "name -> name -> lens" fclname
            (fun v3 -> make_unary_lib fclname (lift_unary (dblens v1 v2 v3)))))
  )

(* Rename *)
let () =
  register_name_name_unary_dblens
    "Native.Relational.rename" Dblens.rename

(* Select *)
let () =
  register_schema_unary_dblens
    "Native.Relational.select" Dblens.select
let () =
  register_name_name_unary_dblens
    "Native.Relational.select_eq" Dblens.select_eq

(* Project *)
let tree_to_string_list v =
  Name.Set.elements (V.dom v)

let () =
  register_tree_tree_tree_unary_dblens
    "Native.Relational.project" (
      fun fldstree keystree dflttree ->
        Dblens.project
          (tree_to_string_list fldstree)
          (tree_to_string_list keystree)
          (Treedb.tree_to_rel dflttree)
    )
*)

