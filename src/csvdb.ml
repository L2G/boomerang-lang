
module Db = Map.Make (String)
module Rel = Relation

let ( |> ) f g x =
  g (f x)

(*
(* Return the lines of text from an input channel. *)
let rec readlines inch =
  try
    let line = input_line inch in
    line :: (readlines inch)
  with
    End_of_file -> [] ;;

(* Get lines of text from a file. *)
let filelines filename =
  let inch = open_in filename in
  let lines = readlines inch in
  close_in inch;
  lines ;;

let filecontents filename =
  (String.concat "\n" (filelines filename)) ^ "\n"
*)

let list_to_rel tbl =
  match tbl with
  | [] -> Rel.create []
  | flds :: rows ->
      let rcds = List.map (fun x -> List.combine flds x) rows in
      List.fold_right Rel.insert rcds (Rel.create flds)

let rel_to_list rel =
  let accum rcd ls =
    (snd (List.split rcd)) :: ls
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
  List.fold_right2 Db.add tblnames tbls Db.empty

let save_tbl file tbl =
  Csv.save file (rel_to_list tbl)

let save_db dir db =
  if not (Sys.file_exists dir) then Unix.mkdir dir 493;
  List.iter (fun x -> Sys.remove (Filename.concat dir x)) (get_csvfiles dir);
  let storetbl tblname tbl () =
    let filename = Filename.concat dir (tblname ^ ".csv") in
    Csv.save filename (rel_to_list tbl)
  in
  Db.fold storetbl db ()

