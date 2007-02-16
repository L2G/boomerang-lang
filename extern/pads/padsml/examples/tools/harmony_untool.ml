type t = Tree.t

let processBaseType conv t = conv (Tree.get_value t)
let processInt = processBaseType int_of_string
let processFloat = processBaseType float_of_string
let processChar = processBaseType (fun s -> if String.length s <> 1 then assert false else s.[0])
let processString = processBaseType (fun s -> s)
let processUnit = processBaseType (fun s -> if s <> "()" then assert false)

let processRecord ks t = List.fold_left 
  (fun acc ki -> Tree.get_required t ki::acc) 
  [] (List.rev ks)

let processTuple t = 
  List.rev (Tree.fold (fun k tk acc -> tk::acc) t [])

let processDatatype t = List.hd (Tree.to_list t)

let processList t = Tree.list_from_structure t

let scold _ = ()
