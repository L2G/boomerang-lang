(* Remove this: *)
open Toplevel

let orderedpref = Prefs.createBool "ordered" true "*keep bookmark ordering" ""

let archNameUniquifier () =
  if Prefs.read orderedpref then "ord" else "nonord"

type bookmarkType = Mozilla | Safari | Meta

let bookmarktype2string = function
  | Mozilla -> "Mozilla"
  | Safari  -> "Safari"
  | Meta    -> "Meta"
        
let moz2xml f fpre = runcmd (Printf.sprintf "moz2xml < %s > %s" f fpre)

let xml2moz fpost f = runcmd (Printf.sprintf "xml2moz < %s > %s" fpost f)

let plutil f fpre = runcmd (Printf.sprintf "plutil -convert xml1 %s -o %s" f fpre)
  
let chooseEncoding f =
  if Filename.check_suffix f ".html" then ("xml",Mozilla,Some moz2xml,Some xml2moz)
  else if Filename.check_suffix f ".plist" then ("xml",Safari,Some plutil,None)
  else failwith ("Unrecognized suffix: "^f)

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

  
  
