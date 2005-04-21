(** common definitions for reporting parsing errors **)

(* location info for lex'd things *)
(* Line number and char number *)
type pos = int * int
type info = pos * pos

let bogusInfo = ((0,0),(0,0))

(* helper functions for constructing/extracting parsing info *)

let merge_inc = fun (pos1,_) (_,pos2) -> (pos1,pos2)
let merge_exc =fun (_,pos1) (pos2,_) -> (pos1,pos2)

let info_to_string ((l1,c1),(l2,c2)) = 
  Printf.sprintf "line %d, char %d, to line %d, char %d " l1 c1 l2 c2

(* Errors *)
exception Syntax_error of string*info
exception Parse_error of string*info
exception Type_error of string*info
exception Run_error of string*info

(* type error *)
let focal_type_error s = 
  raise (Run_error (("Incorrect type for Focal expression:" ^ s), bogusInfo))
    



