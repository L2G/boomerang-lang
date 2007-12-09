let simplified = Prefs.createBool "simplified" false "use simplified variant of structured text (for illustration)" ""

let chooseEncoding f =
  if Util.endswith f ".txt" then ("blob",(),None,None)
  else raise Not_found

let chooseAbstractSchema types = "Structuredtext.NestedListOfValues"

let chooseLens t schema =
  if Prefs.read simplified then
    "Structuredtext.text_with_simple_star_headers"
  else
    "Structuredtext.text_with_combined_headers"
;;

Toplevel.toplevel
  "harmonize-structuredtext"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens


