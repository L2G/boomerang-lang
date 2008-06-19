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
(* /boomerang/src/berror.ml                                                    *)
(* Boomerang errors                                                            *)
(* $Id$                                                                        *)
(*******************************************************************************)

(* format a string, converting newlines to @\n *)
let nlify s = Misc.format_list "@\n" 
  (Util.format "%s") 
  (Misc.split_nonescape '\n' s)

let nop () = ()

let static_error i n ?(suppl =  nop) msg = 
  raise (Error.Harmony_error(fun () -> 
    Util.format "@[%s: static error in@\n" (Info.string_of_t i);
    Util.format "  @["; 
    nlify n;
    Util.format "@]@\n@\n";
    Util.format "  [@["; 
    nlify msg; 
    suppl ();
    Util.format "@]]@\n"))

let type_error i t s1 (s3l,s3r) =
  (* Util.format "  T=@[%s@]@\n@\n" t;
     Util.format "  @["; 
     nlify s1;
     Util.format "@]@\n@\n"; *)
  raise 
    (Error.Harmony_error 
       (fun () -> 
          Util.format "@[%s: type errors in@\n" (Info.string_of_t i);
          Util.format "  [@["; 
          nlify s3l; 
          Util.format "@]]@\n<<HERE>>@\n  [@[";
          nlify s3r; 
          Util.format "@]]@]@\n"))
    
let split_error i t pos nf =
  raise (Error.Harmony_error (fun () -> 
    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
    Util.format "  Cannot find any string in @\nT=@[%s@]@\n@\n" t;
    Util.format "  in the %s file at posistion %d@]" nf pos;))

let sort_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> 
              Util.format "@[%s: Sort checking error@\n" (Info.string_of_t i);
              msg_thk ();
              Util.format "@]"))

let run_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> 
              Util.format "@[%s: Unexpected run-time error@\n" (Info.string_of_t i);
              msg_thk ();
              Util.format "@]"))

let blame_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> 
              Util.format "@[%s: Run-time checking error@\n" (Info.string_of_t i);
              msg_thk ();
              Util.format "@]"))
    
