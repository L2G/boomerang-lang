(*****************************************************)
(* The Harmony Project                               *)
(* harmony@lists.seas.upenn.edu                      *)
(*                                                   *)
(* error.ml - run-time exceptions                    *)
(*****************************************************)
(* $Id$ *)

(* run-time errors in compiled code *)
exception Compile_error of Info.t * string * string 
  
(* run-time errors in native code *)
exception Native_error of string 
  
(* unexpected, fatal errors *)
exception Fatal_error of string 

(* string_of_file_info : string -> Info.t -> string *)
let string_of_file_info fn i = 
  Printf.sprintf "File \"%s\", %s"
    fn 
    (Info.string_of_t i) 
    
(* fail_on_error : (unit -> 'a) -> 'a 
 *    simple error handling: print and exit. Used in the text UI *)
let fail_on_error f = 
  try 
    f ()
  with 
      Compile_error(i, fn,msg) ->
	Printf.eprintf "%s:\n%s\n"
	  (string_of_file_info fn i)
	  msg;
	exit 1
    | Native_error(msg) 
    | Fatal_error(msg) ->	  
	Printf.eprintf "%s\n" msg;		  
 	exit 1
	  
