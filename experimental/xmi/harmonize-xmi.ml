let chooseEncoding f =
  if Util.endswith f ".xmi" then ("xml",(),None,None)
  else raise Not_found

let chooseAbstractSchema types = "XmiUml.Abstract"

let chooseLens t schema =
  "XmiUml.l"
;;

Toplevel.toplevel
  "harmonize-xmi"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens
