module OrderedInt =
struct 
  type t = int
  let compare (x:int) (y:int) = 
    if x = y then 0
    else if x < y then -1 
    else  1
end

module Map = Map.Make(OrderedInt)
module Set = Set.Make(OrderedInt)
