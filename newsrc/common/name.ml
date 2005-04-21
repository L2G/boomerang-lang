type name = string

module NameMap =
  Mapplus.Make(
    struct
      type t = name
      let compare = compare
      let to_string n = n
    end)

type t = name
module Map = NameMap.Map
module Set = NameMap.KeySet

