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

let mk_type_error (msg : string) (printer : unit -> unit) : 'a =
  raise (Error.Harmony_error (fun () ->
    Format.printf "type error:@ %s:@ " msg;
    printer ()))

let db_replace rn sn (f : Rel.t -> Rel.t) (db : Db.t) : Db.t =
  let orig =
    try 
      Db.lookup rn db
    with Not_found ->
      raise (Error.Harmony_error (fun() ->
        Format.printf "Db.db_replace: database@ "; Db.format_t db; Format.printf "@ has no relation %s" rn))
    in
  Db.extend sn (f orig) (Db.remove rn db)

let db_replace2 rn sn tn (f : Rel.t -> Rel.t -> Rel.t) (db : Db.t) : Db.t =
  Db.extend tn (f (Db.lookup rn db) (Db.lookup sn db))
    (Db.remove rn (Db.remove sn db))

let db_schema_replace
  (error : (unit -> unit) -> 'a) (rn : Name.t) (sn : Name.t)
  (f : Relschema.t -> Relschema.t) (ds : Dbschema.t) : Dbschema.t =
  if not (Dbschema.mem rn ds) then
    error (fun () ->
      Dbschema.format_t ds;
      Format.printf
        "@ has no relational schema associated with \"%s\"" rn);
  if sn <> rn && Dbschema.mem sn ds then
    error (fun () ->
      Dbschema.format_t ds;
      Format.printf
        "@ already has a relational schema associated with \"%s\"" rn);
  Dbschema.extend sn (f (Dbschema.lookup rn ds)) (Dbschema.remove rn ds)

let db_schema_replace2
  (error : (unit -> unit) -> 'a) (rn : Name.t) (sn : Name.t) (tn : Name.t)
  (f : Relschema.t -> Relschema.t -> Relschema.t) (ds : Dbschema.t)
  : Dbschema.t =
  if not (Dbschema.mem sn ds) then
    error (fun () ->
      Dbschema.format_t ds;
      Format.printf
        "@ has no relational schema associated with \"%s\"" sn);
  db_schema_replace error rn tn
    (fun rs -> f rs (Dbschema.lookup sn ds))
    (Dbschema.remove sn ds)

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
let rename_error = mk_type_error rename_qid
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
let select_error = mk_type_error select_qid
let select rn fds sn p =
  let getfun c =
    let f r =
      try Rel.select p r
      with Not_found -> raise (Error.Harmony_error (fun()->
        Format.printf "select: Rel.select failed for@ "; Db.Relation.Pred.format_t p; 
        Format.printf "in@ "; Db.Relation.format_t r)) in
    db_replace rn sn f c in
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
      Relschema.set_fdset (
        Relschema.set_pred (
          Relschema.create u
        ) (Pred.Conj (p, q))
      ) fds1
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
let drop_error = mk_type_error drop_qid
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

(*** join_dl ***)

let joinl_qid = "Native.Relational.join_dl"
let joinl_error = mk_type_error joinl_qid
let joinl rn rfds sn sfds tn =
  let getfun c =
    Db.extend tn (Rel.join (Db.lookup rn c) (Db.lookup sn c))
      (Db.remove rn (Db.remove sn c))
  in
  let putfun a c =
    let r = Db.lookup rn c in
    let s = Db.lookup sn c in
    let t = Db.lookup tn a in
    let u = Rel.fields r in
    let v = Rel.fields s in
    let r0 = merge_rel r (Rel.project u t) rfds in
    let s0 = merge_rel s (Rel.project v t) sfds in
    let tx = Rel.diff (Rel.join r0 s0) t in
    let r1 = Rel.diff r0 (Rel.project u tx) in
    Db.extend rn r1 (Db.extend sn s0 (Db.remove tn a))
  in
  let c2a =
    let f rs ss =
      let u = Relschema.attributes rs in
      let v = Relschema.attributes ss in
      let p = Relschema.get_pred rs in
      let q = Relschema.get_pred ss in
      let rfds' = Relschema.get_fdset rs in
      let sfds' = Relschema.get_fdset ss in
      if rn = sn then
        joinl_error (fun () ->
          Format.printf "The source relations must be distinct.");
      if not (Fd.Set.equal rfds rfds') then
        joinl_error (fun () ->
          Fd.Set.format_t rfds;
          Format.printf "@ is not equal to@ ";
          Fd.Set.format_t rfds');
      if not (Fd.Set.equal sfds sfds') then
        joinl_error (fun () ->
          Fd.Set.format_t sfds;
          Format.printf "@ is not equal to@ ";
          Fd.Set.format_t sfds');
      if not (Fd.Set.tree_form rfds) then
        joinl_error (fun () ->
          Fd.Set.format_t rfds;
          Format.printf "@ is not in tree form");
      if not (Fd.Set.tree_form sfds) then
        joinl_error (fun () ->
          Fd.Set.format_t sfds;
          Format.printf "@ is not in tree form");
      let fd = (Name.Set.inter u v, v) in
      if not (Fd.Set.mem fd (Fd.Set.closure v sfds)) then
        joinl_error (fun () ->
          Fd.Set.format_t sfds;
          Format.printf "@ does not entail the dependency@ ";
          Fd.format_fd fd);
      if not (Pred.ignores p (Fd.Set.outputs rfds)) then
        joinl_error (fun () ->
          Pred.format_t p;
          Format.printf "@ constrains the outputs of@ ";
          Fd.Set.format_t rfds);
      if not (Pred.ignores q (Fd.Set.outputs sfds)) then
        joinl_error (fun () ->
          Pred.format_t q;
          Format.printf "@ constrains the outputs of@ ";
          Fd.Set.format_t sfds);
      Relschema.set_fdset (
        Relschema.set_pred (
          Relschema.create (Name.Set.union u v)
        ) (Pred.Conj (p, q))
      ) (Fd.Set.union rfds sfds)
    in
    db_schema_replace2 joinl_error rn sn tn f
  in
  (native getfun (error_on_missing putfun), WB c2a)

let joinl_lib =
  mk_nfun (SFD ^> SName ^> SFD ^> SName ^> SLens) joinl_qid (
    fun rn ->
      mk_fdfun (SName ^> SFD ^> SName ^> SLens) joinl_qid (
        fun rfds ->
          mk_nfun (SFD ^> SName ^> SLens) joinl_qid (
            fun sn ->
              mk_fdfun (SName ^> SLens) joinl_qid (
                fun sfds ->
                  mk_nfun (SLens) joinl_qid (
                    fun tn ->
                      let l, ck =
                        Value.v_of_db_lens (joinl rn rfds sn sfds tn)
                      in
                      Value.L (l, ck))))))
let () =
  register_native
    joinl_qid (SName ^> SFD ^> SName ^> SFD ^> SName ^> SLens) joinl_lib

