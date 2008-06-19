type tag = string

module TagSet : 
  Set.S with type elt = tag

module TagMap : Mapplus.SMap with type key_t = string 
                             and type key_set = TagSet.t
             
type t

val erase : t -> Brx.t

val format_t : t -> unit
val string_of_t : t -> string

type spine 

type key = string
               
type box_content 
                 
type skeleton = spine * box_content TagMap.t

val mk_box : Info.t -> tag -> t -> t
val mk_star : Info.t -> t -> t
val mk_seq : Info.t -> t -> t -> t
val mk_alt : Info.t -> t -> t -> t
val mk_key : Info.t -> Brx.t -> t 
val mk_leaf : Info.t -> Brx.t -> t

val match_string : t -> string -> bool
val match_string_positions : t -> string -> Int.Set.t
val splittable_cex : t -> t -> string option
val iterable_cex : t -> string option
val split_positions : t -> t -> string -> Int.Set.t
val seq_split : t -> t -> string -> string * string
val star_split : t -> string -> string list
val representative : t -> string option
val disjoint_cex : t -> t -> string option
val erase_equiv : t -> t -> bool

val parse : t -> string -> skeleton
val unparse : skeleton -> string
val box_content : skeleton -> tag -> box_content
val box_type : t -> tag -> t option
val spine_tags : spine -> TagSet.t



               
