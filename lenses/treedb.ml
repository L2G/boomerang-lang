
module Db = Map.Make (String)

let rcd_to_view rcd =
  let keys, data = List.split rcd in
  List.fold_right2
    (fun k s v -> V.set v k (Some (V.new_value s))) keys data V.empty

let view_to_rcd view =
  V.fold (fun k s rcd -> (k, V.get_value s) :: rcd) view []

let rel_to_view rel =
  V.structure_from_list
    (Relation.fold (fun rcd ls -> rcd_to_view rcd :: ls) rel [])

let view_to_rel view =
  let rcds = List.map view_to_rcd (V.list_from_structure view) in
  match rcds with
  | [] -> Relation.create []
  | (fstrow :: rest) as rcds ->
      List.fold_right Relation.insert rcds
        (Relation.create (fst (List.split fstrow)))

let db_to_view db =
  Db.fold (fun k s v -> V.set v k (Some(rel_to_view s))) db V.empty

let view_to_db view =
  V.fold (fun k v db -> Db.add k (view_to_rel v) db) view Db.empty

