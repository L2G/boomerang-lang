(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)


(*** Modules, Types, and Exceptions ***)

open Unittest

module NameListSet = Set.Make (
  struct
    type t = string list
    let compare = Pervasives.compare
  end
)


(*** Misc ***)

let build_ns ls =
  List.fold_right Name.Set.add ls Name.Set.empty

let format_ns ns =
  Format.printf "@[{";
  Misc.format_list ",@ " format_string (Name.Set.elements ns);
  Format.printf "}@]"


(*** Relations ***)

module Relation = struct

  (*** Records ***)

  type record = Name.t Name.Map.t


  (*** Predicates ***)

  module Pred = struct

    type t =
      | True
      | False
      | EqAttAtt of Name.t * Name.t
      | EqAttVal of Name.t * Name.t
      | Not of t
      | Conj of t * t
      | Disj of t * t
      | Impl of t * t

    type pos = Left | Right

    let rec format_t' (p : t) (par : t option) (pos : pos) =
      (*  [format_t' p par pos] formats [p] nicely in the context of being the
          [pos] child of its (optional) parent [par]. *)
      match p, par with
      | True, _ -> Format.printf "TRUE"
      | False, _ -> Format.printf "FALSE"
      | Not (EqAttAtt (a, b)), Some (Not _) ->
          Format.printf "@[(%s <> %s)@]" a b
      | Not (EqAttAtt (a, b)), _ ->
          Format.printf "@[%s <> %s@]" a b
      | Not (EqAttVal (a, v)), Some (Not _) ->
          Format.printf "@[(%s <> \"%s\")@]" a v
      | Not (EqAttVal (a, v)), _ ->
          Format.printf "@[%s <> \"%s\"@]" a v
      | EqAttAtt (a, b), _ -> Format.printf "@[%s = %s@]" a b
      | EqAttVal (a, v), _ -> Format.printf "@[%s = \"%s\"@]" a v
      | Not q, _ ->
          Format.printf "@[~ ";
          format_t' q (Some p) Left;
          Format.printf "@]"
      | Conj (q, r), Some (Not _) ->
          (* paren needed *)
          Format.printf "@[(";
          format_t' q (Some p) Left;
          Format.printf "@ /\\@ ";
          format_t' r (Some p) Right;
          Format.printf ")@]"
      | Conj (q, r), _ ->
          (* no paren *)
          Format.printf "@[";
          format_t' q (Some p) Left;
          Format.printf "@ /\\@ ";
          format_t' r (Some p) Right;
          Format.printf "@]"
      | Disj (q, r), Some (Not _)
      | Disj (q, r), Some (Conj (_, _)) ->
          (* paren needed *)
          Format.printf "@[(";
          format_t' q (Some p) Left;
          Format.printf "@ \\/@ ";
          format_t' r (Some p) Right;
          Format.printf ")@]"
      | Disj (q, r), _ ->
          (* no paren *)
          Format.printf "@[";
          format_t' q (Some p) Left;
          Format.printf "@ \\/@ ";
          format_t' r (Some p) Right;
          Format.printf "@]"
      | Impl (q, r), Some (Not _)
      | Impl (q, r), Some (Conj (_, _))
      | Impl (q, r), Some (Disj (_, _))
      | Impl (q, r), Some (Impl (_, _)) when pos = Left ->
          (* paren needed *)
          Format.printf "@[(";
          format_t' q (Some p) Left;
          Format.printf "@ -> @ ";
          format_t' r (Some p) Right;
          Format.printf ")@]"
      | Impl (q, r), _ ->
          (* no paren *)
          Format.printf "@[";
          format_t' q (Some p) Left;
          Format.printf "@ ->@ ";
          format_t' r (Some p) Right;
          Format.printf "@]"

    let format_t p = format_t' p None Left

    let () =
      let p_ab = EqAttAtt ("A", "B") in
      let p_cd = EqAttAtt ("C", "D") in
      let p_b1 = EqAttVal ("B", "1") in
      let p_c2 = EqAttVal ("C", "2") in
      let p_conj1 = Conj (p_ab, Not p_b1) in
      let p_conj2 = Conj (Not (Not p_cd), p_c2) in
      let p_conj3 = Conj (p_conj1, p_conj2) in
      let p_disj1 = Disj (p_cd, p_conj1) in
      let p_disj2 = Disj (p_c2, Not p_b1) in
      let p_disj3 = Disj (p_disj1, p_disj2) in
      let p_impl1 = Impl (p_c2, Not p_ab) in
      let p_impl2 = Impl (p_disj1, p_conj2) in
      let p_impl3 = Impl (p_impl1, p_impl2) in
      let p1 = Conj (p_impl1, p_disj2) in
      let p2 = Disj (Not p_conj1, p_impl2) in
      let p3 = Conj (Not p1, p2) in
      let p4 = Not (Impl (p_conj2, p3)) in
      add_test "Db.Relation.Pred.format_t.1"
      (fun () ->
        format_t p_conj1; Format.printf "@\n";
        format_t p_conj2; Format.printf "@\n";
        format_t p_conj3; Format.printf "@\n";
        format_t p_disj1; Format.printf "@\n";
        format_t p_disj2; Format.printf "@\n";
        format_t p_disj3; Format.printf "@\n";
        format_t p_impl1; Format.printf "@\n";
        format_t p_impl2; Format.printf "@\n";
        format_t p_impl3; Format.printf "@\n";
        format_t p1; Format.printf "@\n";
        format_t p2; Format.printf "@\n";
        format_t p3; Format.printf "@\n";
        format_t p4; Format.printf "@\n";
      )

    let of_record rcd =
      Name.Map.fold (fun k v p -> Conj (EqAttVal (k, v), p)) rcd True

    let rec names = function
      | True
      | False -> Name.Set.empty
      | EqAttAtt (a, b) -> Name.Set.add b (Name.Set.singleton a)
      | EqAttVal (a, _) -> Name.Set.singleton a
      | Not p -> names p
      | Conj (p, q)
      | Disj (p, q)
      | Impl (p, q) -> Name.Set.union (names p) (names q)

    let rec values = function
      | True
      | False
      | EqAttAtt (_, _) -> Name.Set.empty
      | EqAttVal (_, v) -> Name.Set.singleton v
      | Not p -> values p
      | Conj (p, q)
      | Disj (p, q)
      | Impl (p, q) -> Name.Set.union (values p) (values q)

    let ranges_over p ns = Name.Set.subset (names p) ns

    let ignores p ns = Name.Set.equal ns (Name.Set.diff ns (names p))
      (* This function is written as a syntactic check here.  This is sound but
       * not complete.  A semantic check would be better. *)

    let () =
      let p1 = Disj (Conj (True, Impl (False, True)), Impl (True, False)) in
      let p2 = Impl (EqAttVal ("C", "1"), EqAttAtt ("A", "D")) in
      let p3 = Conj (p1, Disj (Not (EqAttVal ("A", "3")), p2)) in
      let assert_eq_ns = assert_equal format_ns Name.Set.equal in
      begin
        add_test "Db.Relation.Pred.names.1"
        (fun () ->
          assert_eq_ns
            Name.Set.empty
            (names p1)
        );
        add_test "Db.Relation.Pred.names.2"
        (fun () ->
          assert_eq_ns
            (build_ns ["A"; "C"; "D"])
            (names p2)
        );
        add_test "Db.Relation.Pred.names.3"
        (fun () ->
          assert_eq_ns
            (build_ns ["A"; "C"; "D"])
            (names p3)
        );
        add_test "Db.Relation.Pred.values.1"
        (fun () ->
          assert_eq_ns
            Name.Set.empty
            (values p1)
        );
        add_test "Db.Relation.Pred.values.2"
        (fun () ->
          assert_eq_ns
            (build_ns ["1"])
            (values p2)
        );
        add_test "Db.Relation.Pred.values.3"
        (fun () ->
          assert_eq_ns
            (build_ns ["1"; "3"])
            (values p3)
        );
        add_test "Db.Relation.Pred.ranges_over.1"
        (fun () ->
          assert_true (ranges_over p1 Name.Set.empty)
        );
        add_test "Db.Relation.Pred.ranges_over.2"
        (fun () ->
          assert_true (ranges_over p1 (build_ns ["A"; "C"]))
        );
        add_test "Db.Relation.Pred.ranges_over.3"
        (fun () ->
          assert_false (ranges_over p3 (build_ns ["A"; "D"]))
        );
        add_test "Db.Relation.Pred.ranges_over.4"
        (fun () ->
          assert_true (ranges_over p3 (build_ns ["A"; "B"; "C"; "D"; "E"]))
        );
      end

    let rec member rcd = function
      | True -> true
      | False -> false
      | EqAttAtt (a, b) -> (Name.Map.find a rcd) = (Name.Map.find b rcd)
      | EqAttVal (a, v) -> (Name.Map.find a rcd) = v
      | Not p -> not (member rcd p)
      | Conj (p, q) -> (member rcd p) && (member rcd q)
      | Disj (p, q) -> (member rcd p) || (member rcd q)
      | Impl (p, q) -> (not (member rcd p)) || (member rcd q)

    let rec all_rcds (dom : Name.t list) (rng : Name.t list)
    : (Name.t Name.Map.t) list =
      (* a little list monad *)
      let return a = [a] in
      let rec ( >>= ) m k =
        match m with
        | [] -> []
        | a :: x -> (k a) @ (x >>= k)
      in
      match dom with
      | [] -> return Name.Map.empty
      | n :: dom' ->
          (* a list comprehension *)
          rng                 >>= fun v ->
          (all_rcds dom' rng) >>= fun map ->
          return (Name.Map.add n v map)

    let fresh_names (k : int) (ns : Name.Set.t) : Name.Set.t =
      (* compute k names that are distinct from those in ns *)
      let rec range i j = (* make a list of the ints x s.t. i <= x < j *)
        if i >= j then [] else i :: range (succ i) j
      in
      let maxlen = (* maximum length of a string in [ns] *)
        List.fold_left max 0 (List.map String.length (Name.Set.elements ns))
      in
      List.fold_right
        (fun k -> Name.Set.add (String.make k 'X'))
        (range (maxlen + 1) (maxlen + 1 + k))
        (Name.Set.empty)

    let satisfiable p =
      (*  We should look for a witness to the satisfiability of p among records
          whose domain is exactly the same as the set of names appearing in p.
          Furthermore, we may restrict our attention to the finite set of
          records whose values range over all values mentioned in p plus enough
          fresh values so that each attribute can be mapped to a distinct fresh
          value. *)
      let ns, vs = names p, values p in
      let vs' = Name.Set.union vs (fresh_names (Name.Set.cardinal ns) vs) in
        (* the set of values in p plus a fresh name for each attribute *)
      List.exists (fun rcd -> member rcd p)
        (all_rcds (Name.Set.elements ns) (Name.Set.elements vs'))

    let valid p = not (satisfiable (Not p))

    let includes p q = valid (Impl (p, q))

    let equiv p q = includes p q && includes q p

    let normalize p = p

  end


  (*** Relations ***)

  type t = {
    fldseq : Name.t list;
    flds : Name.Set.t;
    rows : NameListSet.t
  }
  (*  This type has the invariants that
      - [fldseq] contains no duplicate values;
      - [flds] and [fldseq] contain the same values;
      - all lists in [rows] are the same length as [fldseq].

      The relation represented by this type has its attributes given by
      [fldseq] and associates the kth element of each row in [rows] with the
      kth attribute in [fldseq].
  *)


  (* FIXME: Replace these exception with Harmony_error *)
  exception Unequal_domains of string list * string list
  exception Duplicate_attribute of string
  exception Missing_attribute of string


  (*** Formatting ***)

  let format_tuple t =
    Format.printf "@[<h 2>(";
    Misc.format_list ",@ " format_string t;
    Format.printf ")@]"

  let format_t r =
    Format.printf " = {@[<v>";
    format_tuple r.fldseq;
    if not (NameListSet.is_empty r.rows) then Format.printf "@ ";
    Misc.format_list "@ " format_tuple (NameListSet.elements r.rows);
    Format.printf "}@]"


  (*** fields ***)

  let fields r = r.fldseq


  (*** fold ***)

  let fold f r init =
    let f' row accum =
      let rcd = Name.Map.from_list (List.combine r.fldseq row) in
      f rcd accum
    in
    NameListSet.fold f' r.rows init


  (*** choose ***)

  let choose r =
    Name.Map.from_list (List.combine r.fldseq (NameListSet.choose r.rows))


  (*** create ***)

  let find_dup (ls : string list) : string option =
    let rec check_dup x xs =
      match xs with
      | [] -> None
      | y :: ys when x = y -> Some x
      | y :: ys -> check_dup y ys
    in
    match List.sort compare ls with
    | [] -> None
    | x :: xs -> check_dup x xs

  let create flds =
    match find_dup flds with
    | None ->
        { fldseq = flds;
          flds = build_ns flds;
          rows = NameListSet.empty }
    | Some fld ->
        raise (Duplicate_attribute (fld))


  (*** insert ***)

  let listproj (rcd : record) (flds : string list) : string list =
    List.map (fun att -> Name.Map.find att rcd) flds

  let insert rcd r =
    if not (Name.Set.equal (Name.Map.domain rcd) r.flds) then
      raise (Unequal_domains (Name.Set.elements (Name.Map.domain rcd), r.fldseq));
    {r with rows = NameListSet.add (listproj rcd r.fldseq) r.rows}

  let insert_tuple tup r =
    if List.length tup <> Name.Set.cardinal r.flds then
      raise (Unequal_domains (r.fldseq, r.fldseq));
    {r with rows = NameListSet.add tup r.rows}


  (*** equal ***)

  let equal r1 r2 =
    Name.Set.equal r1.flds r2.flds &&
    if r1.fldseq = r2.fldseq then
      NameListSet.equal r1.rows r2.rows
    else
      let r3 = fold insert r2 (create r1.fldseq) in
      NameListSet.equal r1.rows r3.rows


  (*** rename ***)

  let rename m n r =
    if not (List.mem m r.fldseq) then
      raise (Missing_attribute (m));
    if List.mem n r.fldseq && n <> m then
      raise (Duplicate_attribute (n));
    let swapname s = if s = m then n else s in
    let fldseq' = List.map swapname r.fldseq in
    {r with fldseq = fldseq'; flds = build_ns fldseq'}


  (*** select ***)

  let filter p r =
    fold (fun rcd r -> if p rcd then insert rcd r else r) r (create r.fldseq)

  let select p r =
    filter (fun rcd -> Pred.member rcd p) r


  (*** project ***)

  let project flds r =
    try
      let accum row =
        let rcd = List.combine r.fldseq row in
        insert_tuple (List.map (fun att -> List.assoc att rcd) flds)
      in
      NameListSet.fold accum r.rows (create flds)
    with
    | Not_found ->
        raise (Missing_attribute (
          List.find (fun x -> not (List.mem x r.fldseq)) flds
          ))


  (*** union, inter, diff ***)

  let lift_set_op op r1 r2 =
    if not (Name.Set.equal r1.flds r2.flds) then
      raise (Unequal_domains (r1.fldseq, r2.fldseq));
    if r1.fldseq = r2.fldseq then
      {r1 with rows = op r1.rows r2.rows}
    else
      let r3 = fold insert r2 (create r1.fldseq) in
      {r1 with rows = op r1.rows r3.rows}

  let union = lift_set_op NameListSet.union
  let inter = lift_set_op NameListSet.inter
  let diff = lift_set_op NameListSet.diff


  (*** join ***)

  let list_diff l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

  let list_union l1 l2 =
    l1 @ (list_diff l2 l1)

  let join r s =
    let iflds = Name.Set.inter r.flds s.flds in
    let accum rcd =
      let p = Pred.of_record (Name.Map.project iflds rcd) in
      union (select p s)
    in
    fold accum r (create (list_union r.fldseq s.fldseq))

end

(*** Databases ***)

type t = Relation.t Name.Map.t

let format_t db =
  Format.printf "@[<hv4>{{{ ";
  Name.Map.iter_with_sep
    (fun k rel -> 
      Format.printf "@[<hv3>%s" k;
      Relation.format_t rel;
      Format.printf "@]")
    (fun() -> Format.printf ",@ ")
    db;
  Format.printf "@] }}}"

let equal db1 db2 =
  Name.Set.equal (Name.Map.domain db1) (Name.Map.domain db2) &&
  Name.Set.for_all
    (fun n ->
      (Relation.equal (Name.Map.find n db1) (Name.Map.find n db2)))
    (Name.Map.domain db1)

let empty = Name.Map.empty
let extend = Name.Map.add
let remove = Name.Map.remove
let lookup = Name.Map.find
let fold = Name.Map.fold


(*** Unit tests ***)

let () =
  let module R = Relation in
  let r1 = R.create ["A"; "B"] in
  let r2 = R.create ["A"; "B"; "C"] in
  let r3 = R.create ["C"; "A"; "B"] in
  let assert_eq_t = assert_equal R.format_t R.equal in
  begin
    add_test "Db.Relation.create.1"
    (fun () ->
      assert_eq_t (R.create []) (R.create [])
    );
    add_test "Db.Relation.create.2"
    (fun () ->
      assert_false (R.equal r1 (R.create []));
      assert_false (R.equal r1 r2)
    );
    add_test "Db.Relation.create.3"
    (fun () ->
      assert_eq_t r2 r3
    );
    add_test "Db.Relation.create.4"
    (fun () ->
      assert_exn (R.Duplicate_attribute ("A"))
      (* Whether "A" or "B" is reported as the duplicate is unspecified. *)
      (fun () -> ignore (R.create ["A"; "B"; "B"; "A"]))
    );
    add_test "Db.Relation.insert.1"
    (fun () ->
      assert_eq_t
        (R.insert_tuple ["3"; "1"; "2"] (
          R.insert_tuple ["6"; "5"; "4"] r2))
        (R.insert (Name.Map.from_list [("A", "6"); ("C", "4"); ("B", "5")]) (
          R.insert (Name.Map.from_list [("B", "1"); ("A", "3"); ("C", "2")]) (
            R.insert (Name.Map.from_list [("A", "3"); ("B", "1"); ("C", "2")]) r3)))
    );
    add_test "Db.Relation.insert.2"
    (fun () ->
      assert_false (R.equal (R.create []) (R.insert Name.Map.empty (R.create [])))
    );
    add_test "Db.Relation.insert.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"], ["A"; "B"]))
      (fun () -> ignore (R.insert (Name.Map.from_list [("A", "1")]) r1))
    );
    add_test "Db.Relation.insert.4"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"; "C"], ["A"; "B"]))
      (fun () -> ignore (R.insert (Name.Map.from_list [("A", "1"); ("B", "3"); ("C", "5")]) r1))
    );
  end;
  let r =
    R.insert_tuple ["1"; "2"; "3"] (
      R.insert_tuple ["4"; "5"; "6"] (
        R.create ["A"; "B"; "C"]))
  in
  begin
    add_test "Db.Relation.rename.1"
    (fun () ->
      assert_eq_t
        (R.insert (Name.Map.from_list (List.combine ["A"; "X"; "C"] ["1"; "2"; "3"])) (
          (R.insert (Name.Map.from_list (List.combine ["A"; "X"; "C"] ["4"; "5"; "6"])) (
            R.create ["A"; "C"; "X"]))))
        (R.rename "B" "X" r)
    );
    add_test "Db.Relation.rename.2"
    (fun () ->
      assert_exn (R.Missing_attribute ("X"))
      (fun () -> ignore (R.rename"X" "Y" r))
    );
    add_test "Db.Relation.rename.3"
    (fun () ->
      assert_exn (R.Duplicate_attribute ("C"))
      (fun () -> ignore (R.rename"B" "C" r))
    );
    add_test "Db.Relation.rename.4"
    (fun () ->
      assert_eq_t r (R.rename"B" "B" r)
    );
    add_test "Db.Relation.project.1"
    (fun () ->
      assert_eq_t
        (R.insert (Name.Map.from_list (List.combine ["A"; "C"] ["1"; "3"])) (
          (R.insert (Name.Map.from_list (List.combine ["A"; "C"] ["4"; "6"])) (
            R.create ["C"; "A"]))))
        (R.project ["A"; "C"] r)
    );
    add_test "Db.Relation.project.2"
    (fun () ->
      assert_eq_t (R.insert Name.Map.empty (R.create [])) (R.project [] r)
    );
    add_test "Db.Relation.project.3"
    (fun () ->
      assert_exn (R.Missing_attribute ("X"))
      (fun () -> ignore (R.project ["A"; "X"] r))
    );
    add_test "Db.Relation.project.4"
    (fun () ->
      assert_exn (R.Missing_attribute ("X"))
      (fun () -> ignore (R.project ["A"; "X"; "B"] r))
    );
    add_test "Db.Relation.project.4"
    (fun () ->
      assert_exn (R.Duplicate_attribute ("A"))
      (fun () -> ignore (R.project ["A"; "A"; "B"] r))
    );
  end;
  let r0 = R.create ["A"; "B"] in
  let r0' = R.create ["B"; "A"] in
  let r1 = R.insert_tuple ["2"; "y"] r0 in
  let r2 =
    R.insert_tuple ["1"; "x"] (
      R.insert_tuple ["2"; "y"] r0) in
  let r3 =
    R.insert_tuple ["1"; "x"] (
      R.insert_tuple ["3"; "z"] r0) in
  let r3' =
    R.insert_tuple ["x"; "1"] (
      R.insert_tuple ["z"; "3"] r0') in
  let r4 =
    R.insert_tuple ["2"; "y"] (
      R.insert_tuple ["3"; "z"] r0) in
  let r4' =
    R.insert_tuple ["y"; "2"] (
      R.insert_tuple ["z"; "3"] r0') in
  let r5 =
    R.insert_tuple ["1"; "x"] (
      R.insert_tuple ["2"; "y"] (
        R.insert_tuple ["3"; "z"] r0)) in
  let r5' =
    R.insert_tuple ["x"; "1"] (
      R.insert_tuple ["y"; "2"] (
        R.insert_tuple ["z"; "3"] r0')) in
  begin
    add_test "Db.Relation.union.1"
    (fun () ->
      List.iter (assert_eq_t r4)
      [ R.union r4 r0
      ; R.union r4 r0'
      ; R.union r0 r4
      ; R.union r0' r4
      ]
    );
    add_test "Db.Relation.union.2"
    (fun () ->
      List.iter (assert_eq_t r5)
      [ R.union r3 r4
      ; R.union r3 r4'
      ; R.union r4 r3
      ; R.union r4' r3
      ]
    );
    add_test "Db.Relation.union.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.union r5 (R.create ["A"])))
    );
    add_test "Db.Relation.inter.1"
    (fun () ->
      List.iter (assert_eq_t r0)
      [ R.inter r4 r0
      ; R.inter r4 r0'
      ; R.inter r0 r4
      ; R.inter r0' r4
      ]
    );
    add_test "Db.Relation.inter.2"
    (fun () ->
      List.iter (assert_eq_t r1)
      [ R.inter r2 r4
      ; R.inter r2 r4'
      ; R.inter r4 r2
      ; R.inter r4' r2
      ]
    );
    add_test "Db.Relation.inter.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.inter r5 (R.create ["A"])))
    );
    add_test "Db.Relation.diff.1"
    (fun () ->
      List.iter (assert_eq_t r0)
      [ R.diff r0 r5
      ; R.diff r0 r5'
      ]
    );
    add_test "Db.Relation.diff.2"
    (fun () ->
      List.iter (assert_eq_t r1)
      [ R.diff r4 r3
      ; R.diff r4 r3'
      ]
    );
    add_test "Db.Relation.diff.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.diff r5 (R.create ["A"])))
    );
  end;

