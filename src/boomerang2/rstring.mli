(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* fclstr.ml - an abstract implementation for strings    *)
(*********************************************************)
(* $Id$ *)

(* the type of the "characters" of the "strings" *)
type sym
val char_code_min : sym
val char_code_max : sym
val is_upper : sym -> bool
val is_lower : sym -> bool
val is_alpha : sym -> bool
val lowercase : sym -> sym
val uppercase : sym -> sym 
val compare_sym : sym -> sym -> int
val leq : sym -> sym -> bool
val l : sym -> sym -> bool
val succ : sym -> sym
val pred : sym -> sym
val of_char : char -> sym
val of_int : int -> sym
val to_int : sym -> int
val repr : sym -> string (* raises Not_found *)
val escaped_repr : sym -> string (* raises Not_found *)
val size_of_repr : sym -> int (* raises Not_found *)
val new_special : string -> sym
  
(* the type of the "strings" *)
type t
val empty : t
val compare : t -> t -> int
val of_string : string -> t
val to_string : t -> string
val make : int -> sym -> t
val length : t -> int
  
val get : t -> int -> sym
val set : t -> int -> sym -> unit
val append : t -> t -> t 
val sub : t -> int -> int -> t    
val blit : t -> int -> t -> int -> int -> unit
val split_prefix : t -> t -> t option
val escaped : string -> string

