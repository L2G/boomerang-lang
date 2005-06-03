type t
type record = (string * string) list
exception Type_error of string
val create : string list -> t
val fields : t -> string list
val insert : record -> t -> t
val fold : (record -> 'a -> 'a) -> t -> 'a -> 'a
val rename : string -> string -> t -> t
val project : string list -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val equal : t -> t -> bool
