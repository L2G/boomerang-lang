
type tag = string 

module TagMap = Map.Make(
  struct
    type t = tag
    let compare (s1:tag) s2 = Pervasives.compare s1 s2
  end)
               
type t =
  | Box of tag * t
  | Key of t
  | Seq of t * t
  | Alt of t * t
  | Star of t
  | Rx of Brx.t

type spine_elt =
  | SBox of tag
  | SBoxStar of tag
  | SString of string

type spine = spine_elt list

type key = string
               
type box_content = (key * string) list
                 
type skeleton = spine * box_content TagMap.t

val parse : t -> string -> skeleton
