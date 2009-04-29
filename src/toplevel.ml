(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/src/toplevel.ml                                                 *)
(* Boomerang front-end                                                        *)
(* $Id$ *)
(******************************************************************************)

(* imports *)
module L = Blenses.DLens
open Error 

let sprintf = Printf.sprintf
let msg = Util.format

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
      
let check_str n r x = 
  if not (Brx.match_string r x) then 
    Berror.run_error (Info.M n)
      (fun () -> 
         let s1,s2 = Brx.split_bad_prefix r x in 
           msg "@[string does not match %s [%s] AROUND HERE [%s]@]" 
             (Brx.string_of_t r) 
             s1 
             s2)

let get_str l c = 
  check_str "get" (L.ctype l) c;
  L.rget l c

let put_str l a c = 
  check_str "put" (L.atype l) a;
  check_str "put" (L.ctype l) c;
  L.rput l a c

let create_str l a =
  check_str "create" (L.atype l) a;
  L.rcreate l a 

(*******)
(* GET *)
(*******)
let get l_n c_fn o_fn = 
  write_string o_fn (get_str (lookup_lens l_n) (read_string c_fn));
  0

(*******)
(* PUT *)
(*******)
let put l_n a_fn c_fn o_fn = 
  write_string o_fn 
    (put_str (lookup_lens l_n) (read_string a_fn) (read_string c_fn));
  0

(**********)
(* CREATE *)
(**********)
let create l_n a_fn o_fn = 
  write_string o_fn (create_str (lookup_lens l_n) (read_string a_fn));
  0

(********)
(* SYNC *)
(********)
let sync l_n o_fn a_fn b_fn o_fn' a_fn' b_fn' =   
  let l = lookup_lens l_n in 
  let read f =
    if Sys.file_exists f then Some (read_string f)
    else None in
  let oc, ac, bc = read o_fn, read a_fn, read b_fn in 
  let oa = Misc.map_option (get_str l) oc in 
  let aa = Misc.map_option (get_str l) ac in 
  let ba = Misc.map_option (get_str l) bc in 
  let xt = match L.xtype l with
    | Some xt -> xt     
    | None -> 
        Berror.run_error (Info.M "sync")
          (fun () -> msg "cannot synchronize with %s" (L.string_of_t l)) in
  let acts,oa',aa',ba' = Bsync.sync_opt xt oa aa ba in 
  let write_str fn vo so = match vo,so with 
    | None,_ -> Misc.remove_file_or_dir fn;
    | Some v, None -> write_string fn (create_str l v) 
    | Some v, Some s -> write_string fn (put_str l v s) in 
  Bprint.nlify acts;
  write_str o_fn' oa' oc;
  write_str a_fn' aa' ac;
  write_str b_fn' ba' bc;
  (* Return non-zero exit code if any conflicts were detected *)
  if aa' = ba' then 0 else 1
  
(* OLD SYNC *)
let archive_fn n = Util.fileInUnisonDir (sprintf ".#%s" n)
let oldsync l o_fn c_fn a_fn = 
  let o_fn = if o_fn = "" then archive_fn c_fn else o_fn in 
  match Sys.file_exists o_fn, Sys.file_exists c_fn, Sys.file_exists a_fn with 
    | _,false,false -> 
        (* if neither c nor a exists, clear o and return *)
        debug_sync (fun () -> Util.format "replicas %s and %s missing; removing archive %s@\n" c_fn a_fn o_fn);
        Misc.remove_file_or_dir o_fn;
        0

    | _,true,false -> 
        (* if c exists but a does not, set a to GET c *)
        debug_sync (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);
        let c = read_string c_fn in 
          write_string o_fn c;
          write_string a_fn (get_str (lookup_lens l) c);
          0
          
    | true,false,true ->
        (* if c does not exist but a and o do, set c and o to PUT a o *)        
        debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
        let a = read_string a_fn in
        let o = read_string o_fn in 
        let c' = put_str (lookup_lens l) a o in 
        let a' = get_str (lookup_lens l) c' in 
          write_string c_fn c';
          write_string a_fn a';
          write_string o_fn c';
          0
    
    | false,false,true ->
        (* if c and o do not exist but a does, set c and o to CREATE a *)        
        debug_sync (fun () -> Util.format "(lens: %s) %s <-- create -- %s\n" l c_fn a_fn);
        let a = read_string a_fn in 
        let c' = create_str (lookup_lens l) a in 
        let a' = get_str (lookup_lens l) c' in 
          write_string o_fn c';
          write_string a_fn a';
          write_string c_fn c';
          0

    | false,true,true -> 
        (* if c and a exist but o does not and a <> GET c then conflict; otherwise set o to c *)
        let a = read_string a_fn in 
        let c = read_string c_fn in 
        let a' = get_str (lookup_lens l) c in 
        if a = a' then 
          begin
            debug_sync (fun () -> Util.format "(lens: %s) setting archive %s to concrete replica %s\n" l o_fn c_fn);
            write_string o_fn c;
            0
          end
        else
          begin 
            debug_sync (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);          
            1
          end

    | true,true,true -> 
        (* otherwise, c, a, and o exist:
           - if a=GET o set a to GET c and o to c;
           - else if c=o, set c and o to PUT a o;
           - otherwise conflict. *)
        let o = read_string o_fn in
        let c = read_string c_fn in 
        let a = read_string a_fn in 
        let a' = get_str (lookup_lens l) c in 
        if a = a' then 
          begin 
            debug_sync (fun () -> Util.format "(lens: %s) setting archive %s to concrete replica %s\n" l o_fn c_fn);
            write_string o_fn c;
            0
          end
        else
          let a' = get_str (lookup_lens l) o in 
            if a = a' then 
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);            
                write_string a_fn (get_str (lookup_lens l) c);
                write_string o_fn c;
                0
              end
            else if c = o then
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
                let c' = put_str (lookup_lens l) a o in 
                let a' = get_str (lookup_lens l) c' in 
                  write_string o_fn c';
                  write_string a_fn a';
                  write_string c_fn c';
                  0
              end            
            else 
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);
                1
              end

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
       Prefs.set Binterp.test_all true;
       Safelist.iter check rest_pref;
       Brx.print_stats ();
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
         (* oldsync *)
         | ["oldsync"; l; o_fn; a_fn; b_fn],[],[],[],"" -> 
             ["oldsync"],[l],[o_fn; a_fn; b_fn],[],""
         | _ -> bad_cmdline () in 
       let o_fn = if o="" then "-" else o in 
       match rest_pref,ll,cl,al with
         | ["get"],[l],[c_fn],[]        -> get l c_fn o_fn
         | ["create"],[l],[],[a_fn]     -> create l a_fn o_fn
         | ["put"],[l],[c_fn],[a_fn]    -> put l a_fn c_fn o_fn
         | ["sync"],[l],[o_fn; a_fn; b_fn; o_fn'; a_fn'; b_fn'],[]   -> sync l o_fn a_fn b_fn o_fn' a_fn' b_fn'
         | ["oldsync"],[l],[o_fn; a_fn; b_fn],[] -> oldsync l o_fn a_fn b_fn 
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
