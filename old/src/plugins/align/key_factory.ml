
open Surveyor 

module EVTKey : Map.OrderedType with type t = (encoding_key * type_desc) =
  struct
    type t = encoding_key * type_desc
    let compare = Pervasives.compare
  end
module EVTMap = Map.Make (EVTKey)

let kfmap = ref EVTMap.empty

let register_keyfactory kf evt = kfmap := EVTMap.add evt kf !kfmap

let get_keyfactory evt =
  try EVTMap.find evt !kfmap with Not_found -> Align.default_kf
