
let rcd_to_tree rcd =
  Name.Map.fold
    (fun k s t -> Tree.set t k (Some (Tree.new_value s))) rcd Tree.empty

let tree_to_rcd tree =
  Tree.fold
    (fun k s rcd -> Name.Map.add k (Tree.get_value s) rcd) tree
    Name.Map.empty

let rel_to_tree rel =
  Tree.structure_from_list
    (* FIXME: If Ocaml >= 3.08.3, List.rev is sufficient. *)
    (List.sort Tree.compare
      (Db.Relation.fold (fun rcd ls -> rcd_to_tree rcd :: ls) rel []))

let tree_to_rel tree =
  let rcds = List.map tree_to_rcd (Tree.list_from_structure tree) in
  match rcds with
  | [] -> Db.Relation.create []
  | (fstrow :: rest) as rcds ->
      List.fold_right Db.Relation.insert rcds
        (Db.Relation.create (Name.Set.elements (Name.Map.domain fstrow)))

let db_to_tree db =
  Db.fold (fun k s v -> Tree.set v k (Some(rel_to_tree s))) db Tree.empty

let tree_to_db tree =
  Tree.fold (fun k v db -> Db.extend k (tree_to_rel v) db) tree Db.empty

