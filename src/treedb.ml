
module Db = Map.Make (String)

let rcd_to_tree rcd =
  let keys, data = List.split rcd in
  List.fold_right2
    (fun k s v -> V.set v k (Some (V.new_value s))) keys data V.empty

let tree_to_rcd tree =
  V.fold (fun k s rcd -> (k, V.get_value s) :: rcd) tree []

let rel_to_tree rel =
  V.structure_from_list
    (* FIXME: If Ocaml >= 3.08.3, List.rev is sufficient. *)
    (List.sort V.compare
      (Relation.fold (fun rcd ls -> rcd_to_tree rcd :: ls) rel []))

let tree_to_rel tree =
  let rcds = List.map tree_to_rcd (V.list_from_structure tree) in
  match rcds with
  | [] -> Relation.create []
  | (fstrow :: rest) as rcds ->
      List.fold_right Relation.insert rcds
        (Relation.create (fst (List.split fstrow)))

let db_to_tree db =
  Db.fold (fun k s v -> V.set v k (Some(rel_to_tree s))) db V.empty

let tree_to_db tree =
  V.fold (fun k v db -> Db.add k (tree_to_rel v) db) tree Db.empty

