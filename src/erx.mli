type tag = string

module TagSet : 
  Set.S with type elt = tag

module TagMap : Mapplus.SMap with type key_t = string 
                             and type key_set = TagSet.t
             
type t

type spine 
type key = string
type box_content = (key * string) list                 
type skeleton = spine * box_content TagMap.t

(* pretty printers *)
val format_t : t -> unit
val format_spine : spine -> unit
val format_box_content : box_content -> unit
val format_skeleton : skeleton -> unit
val string_of_t : t -> string
val string_of_spine : spine -> string
val string_of_box_content : box_content -> string
val string_of_skeleton : skeleton -> string

(* constructors *)
val mk_box : tag -> t -> t 
val mk_star : t -> t 
val mk_seq : t -> t -> t 
val mk_alt : t -> t -> t 
val mk_key : Bregexp.t -> t 
val mk_leaf : Bregexp.t -> t 


(* operations *)
val bare : t -> Bregexp.t
val boxes : t -> int
val iterable : t -> bool
val parse : t -> string -> skeleton
val unparse : skeleton -> string
val box_content : skeleton -> tag -> box_content
val box_type : t -> tag -> t option
val spine_tags : spine -> TagSet.t



               
