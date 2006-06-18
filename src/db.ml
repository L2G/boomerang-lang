(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)


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

let format_name n =
  Format.printf "%s" n

let format_ns ns =
  Format.printf "@[{";
  Misc.format_list ",@ " format_name (Name.Set.elements ns);
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

    let subst a b c = if c = a then b else c

    let rec rename_att a b = function
      | True -> True
      | False -> False
      | EqAttAtt (c, d) -> EqAttAtt(subst a b c, subst a b d)
      | EqAttVal (c, x) -> EqAttVal(subst a b c, x)
      | Not p -> Not (rename_att a b p)
      | Conj (p, q) -> Conj (rename_att a b p, rename_att a b q)
      | Disj (p, q) -> Disj (rename_att a b p, rename_att a b q)
      | Impl (p, q) -> Impl (rename_att a b p, rename_att a b q)

    let ignores p ns = Name.Set.is_empty (Name.Set.inter (names p) ns)
      (* This function is written as a syntactic check here.  This is sound but
       * not complete.  A semantic check would be better. *)

    (* FIXME: put more info in error message. *)
    let assert_mem msg att rcd =
      if not (Name.Set.mem att (Name.Map.domain rcd)) then
        raise (Error.Harmony_error (fun () ->
          Format.printf "%s:@ " msg;
          Format.printf "%s is mentioned in the predicate@ " att;
          Format.printf "but is not in the domain of the record."))

    let rec member rcd = function
      | True -> true
      | False -> false
      | EqAttAtt (a, b) ->
          assert_mem "Db.Relation.Pred.member" a rcd;
          assert_mem "Db.Relation.Pred.member" b rcd;
          (Name.Map.find a rcd) = (Name.Map.find b rcd)
      | EqAttVal (a, v) ->
          assert_mem "Db.Relation.Pred.member" a rcd;
          (Name.Map.find a rcd) = v
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

    type eq_atom =
      | TrueDNF
      | FalseDNF
      | AttAtt of Name.t * Name.t
      | NotAttAtt of Name.t * Name.t
      | AttVal of Name.t * Name.t
      | NotAttVal of Name.t * Name.t

    type dnf_clause = eq_atom list
    type dnf_formula = dnf_clause list

    let of_eq_atom = function
      | TrueDNF -> True
      | FalseDNF -> False
      | AttAtt (a, b) -> EqAttAtt (a, b)
      | NotAttAtt (a, b) -> Not (EqAttAtt (a, b))
      | AttVal (a, x) -> EqAttVal (a, x)
      | NotAttVal (a, x) -> Not (EqAttVal (a, x))

    let of_dnf_clause (c : dnf_clause) : t =
      List.fold_right
        (fun p q -> Conj (p, q))
        (List.map of_eq_atom c)
        True

    let of_dnf (f : dnf_formula) : t =
      List.fold_right
        (fun p q -> Disj (p, q))
        (List.map of_dnf_clause f)
        False

    let dnf_conj_clause (c : dnf_clause) (f : dnf_formula) : dnf_formula =
      List.map ((@) c) f

    let dnf_conj (f1 : dnf_formula) (f2 : dnf_formula) : dnf_formula =
      List.fold_right (fun c -> (@) (dnf_conj_clause c f2)) f1 []

    let to_dnf (p : t) : dnf_formula =
      let rec nnf : t -> t = function
        | Not True -> False
        | Not False -> True
        | Not (Not p) -> nnf p
        | Not (Conj (p, q)) -> Disj (nnf (Not p), nnf (Not q))
        | Not (Disj (p, q)) -> Conj (nnf (Not p), nnf (Not q))
        | Conj (p, q) -> Conj (nnf p, nnf q)
        | Disj (p, q) -> Disj (nnf p, nnf q)
        | Impl (p, q) -> Disj (nnf (Not p), nnf q)
        | p -> p
      in
      let rec dnf = function
        | True -> [ [] ]
        | False -> []
        | EqAttAtt (a, b) -> [ [ AttAtt (a, b) ] ]
        | EqAttVal (a, x) -> [ [ AttVal (a, x) ] ]
        | Not (EqAttAtt (a, b)) -> [ [ NotAttAtt (a, b) ] ]
        | Not (EqAttVal (a, x)) -> [ [ NotAttVal (a, x) ] ]
        | Disj (p, q) -> (dnf p) @ (dnf q)
        | Conj (p, q) -> dnf_conj (dnf p) (dnf q)
        | _ -> assert false (* only consider formulas in nnf *)
      in
      dnf (nnf p)

    type eq_symbol =
      | Att of Name.t
      | Val of Name.t

    let remove_att_clause (a : Name.t) (c : dnf_clause) : dnf_clause =
      (* Returns a dnf_clause [c'] such that, forall [rcd],
       *   [rcd] satisfies [c']
       *     if and only if there exists a [v] for which
       *   [rcd]::(a -> v) satisfies [c]
       *)
      if not (satisfiable (of_dnf_clause c)) then
        [ FalseDNF ]
      else
        let cons x ls = x :: ls in
        let equal_items =
          (* Collect all the names and attributes to which a is equal. *)
          List.fold_right (
            function
              | AttAtt (b, c) when b = a -> cons (Att c)
              | AttAtt (c, b) when b = a -> cons (Att c)
              | AttVal (b, x) when b = a -> cons (Val x)
              | _ -> fun ls -> ls
          ) c []
        in
        let c_ext =
          (* Extend c with additional equality assertions. *)
          List.fold_right2 (
            fun item1 item2 ->
              match item1, item2 with
              | Att b, Att c -> cons (AttAtt (b, c))
              | Att b, Val x -> cons (AttVal (b, x))
              | Val x, Att b -> cons (AttVal (b, x))
              | Val x, Val y -> if x <> y then assert false else fun c -> c
              (* In this last case, if x <> y, then we must have had a = x and
               * a = y in the original formula, which would mean that it would
               * not be satisfiable and we would not be at this point. *)
          ) ([ Att a ] @ equal_items) (equal_items @ [ Att a ]) c
        in
        (* Return the extended predicate while throwing away assertions that
         * mention a. *)
        List.filter (
          function
            | AttAtt (b, c) when b = a || c = a -> false
            | NotAttAtt (b, c) when b = a || c = a -> false
            | AttVal (b, x) when b = a -> false
            | NotAttVal (b, x) when b = a -> false
            | _ -> true
        ) c_ext

    let remove_att_formula (a : Name.t) : dnf_formula -> dnf_formula =
      List.map (remove_att_clause a)

    let project ns p =
      of_dnf (
        Name.Set.fold remove_att_formula
          (Name.Set.diff (names p) ns)
          (to_dnf p))

    let normalize p = project (names p) p

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


  (*** Formatting ***)

  let format_tuple t =
    Format.printf "@[<h 2>(";
    Misc.format_list ",@ " format_name t;
    Format.printf ")@]"

  let format_t r =
    Format.printf "{@[<v>";
    format_tuple r.fldseq;
    if not (NameListSet.is_empty r.rows) then Format.printf "@ ";
    Misc.format_list "@ " format_tuple (NameListSet.elements r.rows);
    Format.printf "}@]"

  let format_t_data r =
    Format.printf "{@[<v>";
    Misc.format_list ",@ " format_tuple (NameListSet.elements r.rows);
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
        raise (Error.Harmony_error (fun () ->
          Format.printf "%s cannot be used twice as an attribute name." fld))


  (*** insert ***)

  let listproj (rcd : record) (flds : string list) : string list =
    List.map (fun att -> Name.Map.find att rcd) flds

  let insert rcd r =
    if not (Name.Set.equal (Name.Map.domain rcd) r.flds) then
      raise (Error.Harmony_error (fun () ->
        Format.printf "Cannot insert a record with domain@ ";
        format_ns (Name.Map.domain rcd);
        Format.printf "@ into a relation with domain@ ";
        format_ns r.flds));
    {r with rows = NameListSet.add (listproj rcd r.fldseq) r.rows}

  let insert_tuple tup r =
    if List.length tup <> Name.Set.cardinal r.flds then
      raise (Error.Harmony_error (fun () ->
        Format.printf "Cannot insert a tuple of length %d" (List.length tup);
        Format.printf "@ into a relation with domain@ ";
        format_ns r.flds));
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
    if not (Name.Set.mem m r.flds) then
      raise (Error.Harmony_error (fun () ->
        Format.printf "%s is not in the domain@ " m;
        format_ns r.flds));
    if Name.Set.mem n r.flds && n <> m then
      raise (Error.Harmony_error (fun () ->
        Format.printf "%s is already in the domain@ " n;
        format_ns (Name.Set.remove m r.flds)));
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
        raise (Error.Harmony_error (fun () ->
          format_ns (List.fold_right Name.Set.add flds Name.Set.empty);
          Format.printf "@ is not a subset of the domain@ ";
          format_ns r.flds))


  (*** union, inter, diff ***)

  let lift_set_op msg op r1 r2 =
    if not (Name.Set.equal r1.flds r2.flds) then
      raise (Error.Harmony_error (fun () ->
        Format.printf "%s:@ " msg;
        format_ns r1.flds;
        Format.printf "@ is not equal to the domain@ ";
        format_ns r2.flds));
    if r1.fldseq = r2.fldseq then
      {r1 with rows = op r1.rows r2.rows}
    else
      let r3 = fold insert r2 (create r1.fldseq) in
      {r1 with rows = op r1.rows r3.rows}

  let union = lift_set_op "Db.Relation.union" NameListSet.union
  let inter = lift_set_op "Db.Relation.inter" NameListSet.inter
  let diff = lift_set_op "Db.Relation.diff" NameListSet.diff


  (*** join ***)

  let list_diff l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

  let list_union l1 l2 =
    l1 @ (list_diff l2 l1)

  let join r s =
    let iflds = Name.Set.inter r.flds s.flds in
    (* let rflds = Name.Set.diff r.flds iflds in *)
    let newseq = list_union r.fldseq s.fldseq in
    let accum rcd =
      let p = Pred.of_record (Name.Map.project iflds rcd) in
      let rcdrel =
        fold (fun rcd' -> insert (Name.Map.combine rcd rcd'))
          (select p s) (create newseq)
      in
      union rcdrel
    in
    fold accum r (create newseq)

end

(*** Databases ***)

type t = Relation.t Name.Map.t

let format_t db =
  Format.printf "@[<hv4>{{{ ";
  Name.Map.iter_with_sep
    (fun k rel -> 
      Format.printf "%s@[<v -1>" k;
      Relation.format_tuple (Relation.fields rel);
      Format.printf " =@ ";
      Relation.format_t_data rel;
      Format.printf "@]")
    (fun() -> Format.printf ",@ ")
    db;
  Format.printf "@] }}}"

let string_of_t db = Misc.format_to_string (fun () -> format_t db)

let equal db1 db2 =
  Name.Set.equal (Name.Map.domain db1) (Name.Map.domain db2) &&
  Name.Set.for_all
    (fun n ->
      (Relation.equal (Name.Map.find n db1) (Name.Map.find n db2)))
    (Name.Map.domain db1)

let empty = Name.Map.empty
let extend = Name.Map.add
let remove = Name.Map.remove
let fold = Name.Map.fold
let lookup rn db =
  try Name.Map.find rn db with
  | Not_found ->
      raise (Error.Harmony_error (fun () ->
        Format.printf "Db.lookup:@ ";
        format_t db;
        Format.printf "@ contains no relation named %s" rn))


(*** Unit tests ***)

let () =
  let module P = Relation.Pred in
  let p1 =
    P.Disj (
      P.Conj (P.True, P.Impl (P.False, P.True)),
      P.Impl (P.True, P.False))
  in
  let p2 = P.Impl (P.EqAttVal ("C", "1"), P.EqAttAtt ("A", "D")) in
  let p3 = P.Conj (p1, P.Disj (P.Not (P.EqAttVal ("A", "3")), p2)) in
  let assert_eq_ns = assert_equal format_ns Name.Set.equal in
  begin
    add_test "Db.Relation.Pred.names.1"
    (fun () ->
      assert_eq_ns
        Name.Set.empty
        (P.names p1)
    );
    add_test "Db.Relation.Pred.names.2"
    (fun () ->
      assert_eq_ns
        (build_ns ["A"; "C"; "D"])
        (P.names p2)
    );
    add_test "Db.Relation.Pred.names.3"
    (fun () ->
      assert_eq_ns
        (build_ns ["A"; "C"; "D"])
        (P.names p3)
    );
    add_test "Db.Relation.Pred.values.1"
    (fun () ->
      assert_eq_ns
        Name.Set.empty
        (P.values p1)
    );
    add_test "Db.Relation.Pred.values.2"
    (fun () ->
      assert_eq_ns
        (build_ns ["1"])
        (P.values p2)
    );
    add_test "Db.Relation.Pred.values.3"
    (fun () ->
      assert_eq_ns
        (build_ns ["1"; "3"])
        (P.values p3)
    );
    add_test "Db.Relation.Pred.ranges_over.1"
    (fun () ->
      assert_true (P.ranges_over p1 Name.Set.empty)
    );
    add_test "Db.Relation.Pred.ranges_over.2"
    (fun () ->
      assert_true (P.ranges_over p1 (build_ns ["A"; "C"]))
    );
    add_test "Db.Relation.Pred.ranges_over.3"
    (fun () ->
      assert_false (P.ranges_over p3 (build_ns ["A"; "D"]))
    );
    add_test "Db.Relation.Pred.ranges_over.4"
    (fun () ->
      assert_true (P.ranges_over p3 (build_ns ["A"; "B"; "C"; "D"; "E"]))
    );
  end

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
    (*
    add_test "Db.Relation.create.4"
    (fun () ->
      assert_exn (R.Duplicate_attribute ("A"))
      (* Whether "A" or "B" is reported as the duplicate is unspecified. *)
      (fun () -> ignore (R.create ["A"; "B"; "B"; "A"]))
    );
    *)
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
    (*
    add_test "Db.Relation.insert.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"], ["A"; "B"]))
      (fun () -> ignore (R.insert (Name.Map.from_list [("A", "1")]) r1))
    );
    *)
    (*
    add_test "Db.Relation.insert.4"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"; "C"], ["A"; "B"]))
      (fun () -> ignore (R.insert (Name.Map.from_list [("A", "1"); ("B", "3"); ("C", "5")]) r1))
    );
    *)
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
    (*
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
    *)
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
    (*
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
    *)
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
    (*
    add_test "Db.Relation.union.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.union r5 (R.create ["A"])))
    );
    *)
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
    (*
    add_test "Db.Relation.inter.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.inter r5 (R.create ["A"])))
    );
    *)
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
    (*
    add_test "Db.Relation.diff.3"
    (fun () ->
      assert_exn (R.Unequal_domains (["A"; "B"], ["A"]))
      (fun () -> ignore (R.diff r5 (R.create ["A"])))
    );
    *)
  end;

