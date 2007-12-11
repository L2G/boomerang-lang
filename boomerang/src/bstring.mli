(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/bstring.ml                                                   *)
(* Boomerang strings interface                                                 *)
(* $Id$                                                                        *)
(*******************************************************************************)

(* the type of the "characters" of the "strings" *)
type sym
val char_int_min : int
val char_int_max : int
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
val sym_of_char : char -> sym
val sym_of_int : int -> sym
val int_of_sym : sym -> int
val repr : sym -> string (* raises Not_found *)
val escaped_repr : sym -> string (* raises Not_found *)
val size_of_repr : sym -> int (* raises Not_found *)
val new_special : string -> sym
  
(* the type of the "strings" *)
type t
val empty : t
val compare : t -> t -> int
val t_of_string : string -> t
val string_of_t : t -> string
val make : int -> sym -> t
val length : t -> int
  
val get : t -> int -> sym
val set : t -> int -> sym -> unit
val append : t -> t -> t 
val sub : t -> int -> int -> t    
val blit : t -> int -> t -> int -> int -> unit
val split_prefix : t -> t -> t option
val escaped : string -> string

