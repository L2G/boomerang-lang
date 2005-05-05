(***********************************************************)
(* The Harmony Project                                     *)
(* harmony@lists.seas.upenn.edu                            *)
(*                                                         *)
(* pretty.ml - pretty printing functions                   *)
(*                                                         *)
(* $Id: pretty.ml,v 1.4 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                         *)
(***********************************************************)

(* debugging *)
let debug_flag = true
let debug s = if debug_flag then (prerr_string (s ^ "\n"))

(* generic pretty-printing helper functions *)
let concat sep list = 
  if (list = []) 
  then "" 
  else
    Safelist.fold_right 
      (fun h t -> if (t = sep) then h else (h ^ sep ^ t))
      list
      sep
      
let enclose p1 s p2 = p1 ^ s ^ p2
let braces s = enclose "(" s ")"
let brackets s = enclose "[" s "]" 
let curlybraces s = enclose "{" s "}" 
let sprintf = Printf.sprintf
