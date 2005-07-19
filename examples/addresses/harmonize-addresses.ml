type addresstype = XCard | PilotAddressesCsv 

let chooseEncoding f =
  if Util.endswith f ".xml" then ("xml",XCard,None,None)
  else if Util.endswith f ".csv" then ("csv",PilotAddressesCsv,None,None)   (* Need to strip first line, etc.! *)
  else failwith ("Unrecognized address book suffix: "^f) 

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

  
  
