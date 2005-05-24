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
let sprintf = Printf.sprintf
let debug_flag = false
let debug msg_thk = 
  if debug_flag then (prerr_string (sprintf "%s\n" (msg_thk ())); flush stderr)

(* generic pretty-printing helper functions *)
let concat 
    (fold:('a -> string -> string) -> 't -> string -> string)
    (sep:string) 
    (pretty:'a -> string) 
    (structure: 't) : string =
  let f (h:'a) (acc:string) : string = 
    if acc = "" then (pretty h) 
    else (sprintf "%s%s%s" (pretty h) sep acc)
  in    
    fold f structure ""
    
let concat_list sep l = concat Safelist.fold_right sep (fun x -> x) l 

let enclose p1 s p2 = p1 ^ s ^ p2
let braces s = enclose "(" s ")"
let brackets s = enclose "[" s "]" 
let curlybraces s = enclose "{" s "}" 
