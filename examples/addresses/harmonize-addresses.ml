(* We substitute the following header line for the one created by the pilot-address
   tool, which gives alphabetical titles to the columns of the CSV file, so that
   they will appear in the right order. *)
let goodheader = "# 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21"

(* BCP: Some of this definition duplicates code in Viewers module; it would be cleaner
   to call that code instead, but it's only a little bit so I didn't bother. *)
let _ =
  let read_tbl =
    let fixup_header s =
      let lines = Util.splitIntoWords s '\n' in
      String.concat "\n" (goodheader :: Safelist.tl (Safelist.tl lines)) in
    Surveyor.simple_reader
      (fun s -> 
        let (fn, ouch) = Filename.open_temp_file "harmony" ".csv" in
        output_string ouch (fixup_header s);
        close_out ouch;
        let x = Treedb.rel_to_tree (Csvdb.load_tbl fn) in
        Sys.remove fn;
        x) in
  let write_tbl tr fn = Csvdb.save_tbl fn (Treedb.tree_to_rel tr) in
  let etest filename copt = (Misc.filename_extension filename = "csv") in
  let encoding = {
    Surveyor.description = "CSV containing Palm address book data";
    Surveyor.encoding_test = etest;
    Surveyor.reader = read_tbl;
    Surveyor.writer = write_tbl; } in
  Surveyor.register_encoding "palmaddrcsv" encoding

type addresstype = XCard | PilotAddressesCsv 

let chooseEncoding f =
  if Util.endswith f ".xml" then ("xml",XCard,None,None)
  else if Util.endswith f ".csv" then ("palmaddrcsv",PilotAddressesCsv,None,None)   (* Need to strip first line, etc.! *)
  else raise Not_found

let chooseAbstractSchema types = "Addr.AddrBook"

let chooseLens t schema =
  match t with
    XCard              -> "Addr.xcard"
  | PilotAddressesCsv  -> "Addr.csv";;

Toplevel.toplevel
  "harmonize-addresses"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens



