let unorderedpref = Prefs.createBool "unordered" false "discard bookmark ordering" ""

let recompressPlists = Prefs.createBool "compress" false "convert plist files to binary form after sync" ""

let archNameUniquifier () =
  if Prefs.read unorderedpref then "nonord" else "ord"

type bookmarkType = Mozilla | Safari | Meta

let bookmarktype2string = function
  | Mozilla -> "Mozilla"
  | Safari  -> "Safari"
  | Meta    -> "Meta"
        
let moz2xml f fpre =
  if Sys.file_exists f then
    let fc = open_in f in
    let fprec = open_out fpre in 
      Moz2xml.go fc fprec;
      close_in fc;
      close_out fprec

let xml2moz fpost f =
  if Sys.file_exists fpost then
    let fpostc = open_in fpost in 
    let fc = open_out f in 
      Xml2moz.go fpostc fc;
      close_in fpostc;
      close_out fc

let plutil f fpre =
  Toplevel.runcmd (Printf.sprintf "plutil -convert xml1 %s -o %s" (Misc.whack f) (Misc.whack fpre))

let plutilback f fpost =
  if Prefs.read recompressPlists then
    Toplevel.runcmd (Printf.sprintf "plutil -convert binary1 %s -o %s" (Misc.whack f) (Misc.whack fpost))
  else 
    Toplevel.runcmd (Printf.sprintf "cp %s %s" (Misc.whack f) (Misc.whack fpost))
  
let chooseEncoding f =
  if Filename.check_suffix f ".html" then ("xml",Mozilla,Some moz2xml,Some xml2moz)
  else if Filename.check_suffix f ".plist" then ("xml",Safari,Some plutil,Some plutilback)
  else if Filename.check_suffix f ".xml" then ("xml",Safari,None,None)
  else if Filename.check_suffix f ".meta" then ("meta",Meta,None,None)
  else raise Not_found

let chooseAbstractSchema types =
  let unordered = Prefs.read unorderedpref in
  match types,unordered with
      [Safari],true -> "Bookmarks.BushAbstract"
    | [Mozilla],true -> "Bookmarks.BushAbstract"
    | _ -> "Bookmarks.Abstract"
        
let chooseLens t schema =
  match t,schema with
    Safari,"Bookmarks.Abstract"     -> "Safari.l2"
  | Safari,"Bookmarks.BushAbstract" -> "Safari.l3"
  | Mozilla,"Bookmarks.Abstract"    -> "Mozilla.l2"
  | Mozilla,"Bookmarks.BushAbstract" -> "Mozilla.l3"
  | Meta, "Bookmarks.Abstract"      -> "Prelude.id"
  | Meta, "Bookmarks.BushAbstract"      -> "Prelude.id"
  | _                               -> assert false;;

Toplevel.toplevel
  "harmonize-bookmarks"
  archNameUniquifier
  chooseEncoding
  chooseAbstractSchema
  chooseLens
