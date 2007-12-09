(* FIX: It's a shame that these definitions occur both here and in the safari
   synchronizer (and they will occur in every other synchronizer that uses plists!). 
   They should be broken out into their own little module -- the only question is
   where it should live. (Not in /src, I think.)  --B *)

let recompressPlists = Prefs.createBool "compress" false "convert plist files to binary form after sync" ""

let plutil f fpre =
  Toplevel.runcmd (Printf.sprintf "plutil -convert xml1 %s -o %s" (Misc.whack f) (Misc.whack fpre))

let plutilback f fpost =
  if Prefs.read recompressPlists then
    Toplevel.runcmd (Printf.sprintf "plutil -convert binary1 %s -o %s" (Misc.whack f) (Misc.whack fpost))
  else 
    Toplevel.runcmd (Printf.sprintf "cp %s %s" (Misc.whack f) (Misc.whack fpost))
  
let chooseEncoding f =
  if Util.endswith f ".plist" then ("xml",(),Some plutil,Some plutilback)
  else raise Not_found

let chooseAbstractSchema types = "Plist.T"

let chooseLens t schema = "Plist.l"

;;
Toplevel.toplevel
  "harmonize-plist"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens


