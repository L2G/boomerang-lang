
module Rel = Db.Relation

let ( |> ) f g x =
  g (f x)

let trim_comment_prefix = function
    [] -> []
  | f::fs ->
         (if Util.startswith f "# " then String.sub f 2 ((String.length f) - 2) else f) 
      :: fs

let list_to_rel tbl =
  match tbl with
  | [] -> Rel.create []
  | flds :: rows ->
      let flds = trim_comment_prefix flds in
      List.fold_right Rel.insert_tuple rows (Rel.create flds)

let rel_to_list rel =
  let accum rcd ls =
    (Name.Map.list_project (Rel.fields rel) rcd) :: ls
  in
  (Rel.fields rel) :: (Rel.fold accum rel [])

let check filename =
  Filename.check_suffix filename ".csv"

let get_csvfiles dir =
  List.filter check (Array.to_list (Sys.readdir dir))

let load_tbl = Csv.load |> list_to_rel

let load_db dir =
  let csvfiles = get_csvfiles dir in
  let tblnames = List.map Filename.chop_extension csvfiles in
  let tbls = List.map ((Filename.concat dir) |> load_tbl) csvfiles in
  List.fold_right2 Db.extend tblnames tbls Db.empty

let add_comment_prefix_to_fields = function
    [] -> []
  | []::rest -> []::rest
  | (f::flds)::rest -> (("# "^f) :: flds) :: rest

let save_tbl tbl =
  let l = rel_to_list tbl in
    (* Util.format "@[Saving %d csv rows@\n@]" (List.length l); *)
    let l = add_comment_prefix_to_fields l in
    let buf = Buffer.create 100 in
      Csv.save_buf buf l;
      Buffer.contents buf

let save_db dir db =
  if not (Sys.file_exists dir) then Unix.mkdir dir 493;
  List.iter (fun x -> Sys.remove (Filename.concat dir x)) (get_csvfiles dir);
  let storetbl tblname tbl () =
    let filename = Filename.concat dir (tblname ^ ".csv") in
    Csv.save filename (rel_to_list tbl)
  in
  Db.fold storetbl db ()

