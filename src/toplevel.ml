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
(* /boomerang/src/toplevel.ml                                                  *)
(* Boomerang front-end                                                         *)
(* $Id$                                                                        *)
(*******************************************************************************)

(* imports *)
module L = Blenses.DLens
open Error 

let sprintf = Printf.sprintf

let () = 
  (* initialize unison state (sets directory name) *)
  Util.supplyFileInUnisonDirFn (fun s -> sprintf "./.%s/%s" "boomerang" s);
  (* turn off logging *)
  Prefs.set Trace.logging false;
  (* redirect tracing and debugging output to stdout *)
  Trace.redirect `FormatStdout;
  (* hack to ensure that Boomerang compiler gets linked *)
  Bdriver.init()
    
let exit x = 
  Brx.print_stats ();
  exit x

(* Debugging *)
let debug thk = Trace.debug "toplevel" (fun () -> thk (); Util.format "%!")

let debug_sync thk = Trace.debug "sync" (fun () -> thk (); Util.format "%!")

(* Registry lookup helpers *)
let lookup qid_str = Bregistry.lookup_library (Bregistry.parse_qid qid_str)

let lookup_lens qid_str =
  match lookup qid_str with
      None -> Error.simple_error (Printf.sprintf "lens %s not found" qid_str)
    | Some rv -> Bvalue.get_l (Bregistry.value_of_rv rv)
          

(* Filesystem helpers *)          
let read_string fn = 
  debug (fun () -> Util.format "Reading %s@\n" fn);
  Misc.read fn

let write_string fn s = 
  debug (fun () -> Util.format "Writing back %s@\n" fn);
  Misc.write fn s
    
(*********)
(* CHECK *)
(*********)
let check m = 
  let modname =
    if Util.endswith m ".boom" then
      String.capitalize (Misc.replace_suffix m ".boom" "")
    else if Util.endswith m ".src" then 
      String.capitalize (Misc.replace_suffix m ".src" "")
    else m in
  if not (Bregistry.load modname) then
    raise (Error.Harmony_error 
             (fun () -> 
                Util.format "Error: could not find module %s@\n" modname))
      
let get_str l_n c = L.rget (lookup_lens l_n) c
let put_str l_n a c = L.rput (lookup_lens l_n) a c
let create_str l_n a = L.rcreate (lookup_lens l_n) a

(*******)
(* GET *)
(*******)
let get l_n c_fn o_fn = 
  write_string o_fn (get_str l_n (read_string c_fn));
  0

(*******)
(* PUT *)
(*******)
let put l_n a_fn c_fn o_fn = 
  write_string o_fn (put_str l_n (read_string a_fn) (read_string c_fn));
  0

(**********)
(* CREATE *)
(**********)
let create l_n a_fn o_fn = 
  write_string o_fn (create_str l_n (read_string a_fn));
  0

(********)
(* SYNC *)
(********)
let sync l_n o_fn a_fn b_fn o_fn' a_fn' b_fn' =   
  let l = lookup_lens l_n in 
  let get_f f =
    if Sys.file_exists f then Some (L.rget l (read_string f)) else None in
  let putback f ro' ro =
    match ro' with
      None -> Misc.remove_file_or_dir f
    | Some r' ->
        let newcontents =
          match ro with
            None -> L.rcreate l r'
          | Some r -> L.rput l r' r in
        write_string f newcontents  in

  let oo = get_f o_fn in 
  let ao = get_f a_fn in 
  let bo = get_f b_fn in 
  let xt = match L.xtype l with
    | None -> 
        Error.simple_error (sprintf
          "Error: cannot synchronize with %s" (L.string l))
    | Some xt -> xt in     
  let acts,oo',ao',bo' = Bsync.opt_sync xt oo ao bo in 
  Bprint.nlify acts;
  putback o_fn' oo' oo;
  putback a_fn' ao' ao;
  putback b_fn' bo' bo;
  (* Return non-zero exit code if any conflicts were detected *)
  if ao' = bo' then 0 else 1
  
(*
    Util.format "O  = [@[";
    nlify (get_w oo);
    Util.format "@]]@\nA  = [@[";
    nlify (get_w ao);
    Util.format "@]]@\nB  = [@[";
    nlify (get_w bo);
    Util.format "@]]@\n@\n<~ SYNC ~>@\n@\n";
    Util.format "O' = [@[";
    nlify (get_w oo');
    Util.format "@]]@\nA' = [@[";
    nlify (get_w ao');
    Util.format "@]]@\nB' = [@[";
    nlify (get_w bo');
    Util.format "@]]@\n";
    0
*)

let rest = Prefs.createStringList "rest" "*no docs needed" ""

let o_pref = Prefs.createString     "output" "" "output" ""
let l_pref = Prefs.createStringList "lens"      "lens"          "" 
let c_pref = Prefs.createStringList "concrete"  "concrete file" ""
let a_pref = Prefs.createStringList "abstract"  "abstract file" ""

let _ = Prefs.alias o_pref "o" 
let _ = Prefs.alias l_pref "l" 
let _ = Prefs.alias c_pref "c" 
let _ = Prefs.alias a_pref "a" 

let check_pref = Prefs.createStringList "check" "run unit tests for given module(s)" ""

let toplevel' progName () = 
  let baseUsageMsg = 
    "Usage:\n"
    ^ "    "^progName^" [get] l C             [options] : get\n"
    ^ " or "^progName^" [put] l A C           [options] : put\n"
    ^ " or "^progName^" create l A            [options] : create\n"
    ^ " or "^progName^" sync l O A B          [options] : sync\n"
    ^ " or "^progName^" sync l O A B O' A' B' [options] : sync\n"
    ^ " or "^progName^" F.boom [F.boom...]    [options] : run unit tests\n"
    ^ "\n" in 
  let shortUsageMsg = 
    baseUsageMsg 
    ^ "For a list of options, type \"" ^progName^ " -help\".\n" in 
  let usageMsg = baseUsageMsg ^ "Options:" in 

  let bad_cmdline () = Util.format "%s" shortUsageMsg; exit 2 in 

  (* Parse command-line options *)
  Prefs.parseCmdLine usageMsg;
  
  (* Read preferences *)
  let ll = Safelist.rev (Prefs.read l_pref) in 
  let cl = Safelist.rev (Prefs.read c_pref) in 
  let al = Safelist.rev (Prefs.read a_pref) in 
  let o = Prefs.read o_pref in
  let rest_pref = Safelist.rev (Prefs.read rest) in 

  (* run unit tests if needed *)
  if Prefs.read check_pref <> [] then
    begin
      Safelist.iter check (Prefs.read check_pref);
      if Prefs.read rest = [] then exit 0
    end;
  Util.finalize 
  (fun () -> 
     if rest_pref <> [] && 
       (Safelist.for_all 
          (fun s -> (Util.endswith s ".boom" || Util.endswith s ".src"))
          rest_pref)
     then begin
       (* barf on spurious command line options?! *)
       Prefs.set Bcompiler.test_all true;
       Safelist.iter check rest_pref;
       Trace.debug "casts" (fun () -> Bcompiler.print_stats ());
       0
     end
     else begin 
       let rest_pref,ll,cl,al,o = match rest_pref,ll,cl,al,o with 
         (* get *)
         | [l;c_fn],[],[],[],o
         | [c_fn],[l],[],[],o 
         | ["get"],[l],[c_fn],[],o
         | ["get"; c_fn],[l],[],[],o   
         | ["get"; l],[],[c_fn],[],o   
         | ["get"; l; c_fn],[],[],[],o -> 
             ["get"],[l],[c_fn],[],o             
         (* create *)
         | ["create"],[l],[],[a_fn],o
         | ["create"; l],[],[],[a_fn],o
         | ["create"; a_fn],[l],[],[],o 
         | ["create"; l; a_fn],[],[],[],o ->             
             ["create"],[l],[],[a_fn],o
         (* put *)
         | [l; a_fn; c_fn],[],[],[],o 
         | [a_fn; c_fn],[l],[],[],o 
         | ["put"],[l],[c_fn],[a_fn],o
         | ["put"; a_fn; c_fn],[l],[],[],o 
         | ["put"; l],[],[c_fn],[a_fn],o
         | ["put"; l; a_fn; c_fn],[],[],[],o -> 
             ["put"],[l],[c_fn],[a_fn],o
         (* sync *)
         | ["sync"; l; o_fn; a_fn; b_fn],[],[],[],"" -> 
             ["sync"],[l],[o_fn; a_fn; b_fn; o_fn; a_fn; b_fn],[],""
         | ["sync"; l; o_fn; a_fn; b_fn; o_fn'; a_fn'; b_fn'],[],[],[],"" -> 
             ["sync"],[l],[o_fn; a_fn; b_fn; o_fn'; a_fn'; b_fn'],[],""
         | _ -> bad_cmdline () in 
       let o_fn = if o="" then "-" else o in 
       match rest_pref,ll,cl,al with
         | ["get"],[l],[c_fn],[]        -> get l c_fn o_fn
         | ["create"],[l],[],[a_fn]     -> create l a_fn o_fn
         | ["put"],[l],[c_fn],[a_fn]    -> put l a_fn c_fn o_fn
         | ["sync"],[l],[o_fn; a_fn; b_fn; o_fn'; a_fn'; b_fn'],[]   -> sync l o_fn a_fn b_fn o_fn' a_fn' b_fn'
         | _ -> assert false
     end)
    (fun () -> Util.flush ())
    
let toplevel progName =
  try
    exit 
      (Unix.handle_unix_error 
         (fun () -> Error.exit_on_error (toplevel' progName))
         ())
  with e -> 
    Printf.printf "Uncaught exception %s" (Printexc.to_string e); 
    exit 2
