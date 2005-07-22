let orderedpref = Prefs.createBool "ordered" true "*keep bookmark ordering" ""

let recompressPlists = Prefs.createBool "compress" true "*convert plist files to binary form after sync" ""

let archNameUniquifier () =
  if Prefs.read orderedpref then "ord" else "nonord"

type bookmarkType = Mozilla | Safari | Meta

let bookmarktype2string = function
  | Mozilla -> "Mozilla"
  | Safari  -> "Safari"
  | Meta    -> "Meta"
        
let moz2xml f fpre =
  if Sys.file_exists f then
    Toplevel.runcmd (Printf.sprintf "moz2xml < %s > %s" f fpre)

let xml2moz fpost f =
  if Sys.file_exists fpost then
    Toplevel.runcmd (Printf.sprintf "xml2moz < %s > %s" fpost f)

let plutil f fpre =
  Toplevel.runcmd (Printf.sprintf "plutil -convert xml1 %s -o %s" f fpre)

let plutilback f fpost =
  if Prefs.read recompressPlists then
    Toplevel.runcmd (Printf.sprintf "plutil -convert binary1 %s -o %s" f fpost)
  else 
    Toplevel.runcmd (Printf.sprintf "cp %s %s" f fpost)
  
let chooseEncoding f =
  if Filename.check_suffix f ".html" then ("xml",Mozilla,Some moz2xml,Some xml2moz)
  else if Filename.check_suffix f ".plist" then ("xml",Safari,Some plutil,Some plutilback)
  else raise Not_found

let chooseAbstractSchema types =
  let ordered = Prefs.read orderedpref in
  match types,ordered with
    [Safari],true -> "Bookmarks.Abstract"
  | [Safari],false -> "Bookmarks.BushAbstract"
  | [Mozilla],true -> "Bookmarks.Abstract" 
  | [Mozilla;Safari],true -> "Bookmarks.Abstract" 
  | _ -> failwith (Printf.sprintf "Unimplemented combination of file types: %s, ordered=%b"
                     (String.concat "," (List.map bookmarktype2string types)) ordered);;

let chooseLens t schema =
  match t,schema with
    Safari,"Bookmarks.Abstract"     -> "Safari.l2"
  | Safari,"Bookmarks.BushAbstract" -> "Safari.l2"
  | Mozilla,"Bookmarks.Abstract"    -> "Mozilla.l"
  | _                               -> assert false;;

Toplevel.toplevel
  "harmonize-bookmarks"
  archNameUniquifier
  chooseEncoding
  chooseAbstractSchema
  chooseLens
