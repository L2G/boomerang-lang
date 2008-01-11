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
module BS = Bstring
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
    
let exit x = exit x

(* Debugging *)
let debug thk = Trace.debug "toplevel" (fun () -> thk (); Util.format "%!")

let debug_sync thk = Trace.debug "sync" (fun () -> thk (); Util.format "%!")

(* Registry lookup helpers *)
let lookup qid_str = Bregistry.lookup_library (Bvalue.parse_qid qid_str)

let lookup_lens qid_str =
  match lookup qid_str with
      None -> Error.simple_error (Printf.sprintf "lens %s not found" qid_str)
    | Some rv -> 
        Bvalue.get_l 
          (Bregistry.value_of_rv rv)
          (Info.M (Printf.sprintf "%s is not a lens" qid_str))

(* Filesystem helpers *)          
let read_string fn = 
  debug (fun () -> Util.format "Reading %s@\n" fn);
  BS.t_of_string (Misc.read fn)

let write_string fn s = 
  debug (fun () -> Util.format "Writing back %s@\n" fn);
  Misc.write fn (BS.string_of_t s)
    
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
      
let get_str l_n c = L.get (lookup_lens l_n) c
let put_str l_n a c = L.rput_of_dl (lookup_lens l_n) a c
let create_str l_n a = L.rcreate_of_dl (lookup_lens l_n) a

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
let archive_fn n = Util.fileInUnisonDir (sprintf ".#%s" n)
let sync l o_fn c_fn a_fn = 
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
          write_string a_fn (get_str l c);
          0
          
    | true,false,true ->
        (* if c does not exist but a and o do, set c and o to PUT a o *)        
        debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
        let a = read_string a_fn in
        let o = read_string o_fn in 
        let c' = put_str l a o in 
          write_string c_fn c';
          write_string o_fn c';
          0
    
    | false,false,true ->
        (* if c and o do not exist but a does, set c and o to CREATE a *)        
        debug_sync (fun () -> Util.format "(lens: %s) %s <-- create -- %s\n" l c_fn a_fn);
        let a = read_string a_fn in 
        let c' = create_str l a in 
          write_string c_fn c';
          write_string o_fn c';
          0

    | false,true,true -> 
        (* if c and a exist but o does not and a <> GET c then conflict; otherwise set o to c *)
        let a = read_string a_fn in 
        let c = read_string c_fn in 
        let a' = get_str l c in 
        if BS.equal a a' then 
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
        let a' = get_str l c in 
        if BS.equal a a' then 
          begin 
            debug_sync (fun () -> Util.format "(lens: %s) setting archive %s to concrete replica %s\n" l o_fn c_fn);
            write_string o_fn c;
            0
          end
        else
          let a' = get_str l o in 
            if BS.equal a a' then 
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);            
                write_string a_fn (get_str l c);
                write_string o_fn c;
                0
              end
            else if BS.equal c o then
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
                let c' = put_str l a o in 
                  write_string c_fn c';
                  write_string o_fn c';
                  0
              end            
            else 
              begin 
                debug_sync (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);
                1
              end
              
let refresh p_fn l o_fn c_fn a_fn = 
  let go () = Util.format "%d\n%!" (sync l o_fn c_fn a_fn) in 
  go ();
  while true do 
    let ch = open_in p_fn in 
    let _ = input_line ch in 
    let _ = close_in ch in 
    try go ()
    with Error.Harmony_error (err) -> err ()
  done;
    0

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
    ^ "    "^progName^" [get] l C          [options]     : get\n"
    ^ " or "^progName^" [put] l A C        [options]     : put\n"
    ^ " or "^progName^" create l A         [options]     : create\n"
    ^ " or "^progName^" sync l O C A       [options]     : sync\n"
    ^ " or "^progName^" [profilename]      [options]     : sync from profile\n"
    ^ " or "^progName^" F.boom [F.boom...] [options]     : run unit tests\n"
    ^ "\n" in 
  let shortUsageMsg = 
    baseUsageMsg 
    ^ "For a list of options, type \"" ^progName^ " -help\".\n" in 
  let usageMsg = baseUsageMsg ^ "Options:" in 

  let bad_cmdline () = Util.format "%s" shortUsageMsg; exit 2 in 

  let sync_profile profile_name = 
    Prefs.profileName := Some profile_name;
    let profile_fn = Prefs.profilePathname profile_name in       
      if Sys.file_exists profile_fn then 
        Prefs.loadTheFile ()
      else if profile_name = "default" then bad_cmdline ()
      else Error.simple_error (sprintf "Error: profile file %s does not exist" profile_fn);
      let ll = Prefs.read l_pref in 
      let cl = Prefs.read c_pref in 
      let al = Prefs.read a_pref in 
      let ll_len = Safelist.length ll in 
      let cl_len = Safelist.length cl in
      let al_len = Safelist.length al in
        if cl_len <> al_len then 
          Error.simple_error "Error: number of concrete and abstract replicas differs"
        else if cl_len <> ll_len then 
          Error.simple_error "Error: number of replicas and lenses differs";
        let rec loop ll cl al exitcode = match ll,cl,al with 
          | [],[],[] -> exitcode
          | l::lrest,c::crest,a::arest ->         
              loop lrest crest arest (sync l "" c a)
          | _ -> assert false in 
          loop ll cl al 0 in 
  
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
       0
     end
     else begin 
       let rest_pref,ll,cl,al,o = match rest_pref,ll,cl,al,o with 
         (* sync profile *)
         | [],[],[],[],""    -> ["default"],[],[],[],""
         | [arg],[],[],[],"" -> 
             if not (Util.endswith arg ".prf") then bad_cmdline ()
             else [Misc.replace_suffix arg ".prf" ""],[],[],[],""
         (* get *)
         | [l;c_fn],[],[],[],o
         | [c_fn],[l],[],[],o 
         | ["get"],[l],[c_fn],[],o
         | ["get"; c_fn],[l],[],[],o   
         | ["get"; l],[],[c_fn],[],o   
         | ["get"; l; c_fn],[],[],[],o -> 
             (* *)
             ["get"],[l],[c_fn],[],o             
         (* create *)
         | ["create"],[l],[],[a_fn],o
         | ["create"; l],[],[],[a_fn],o
         | ["create"; a_fn],[l],[],[],o 
         | ["create"; l; a_fn],[],[],[],o ->
             (* *)
             ["create"],[l],[],[a_fn],o
         (* put *)
         | [l; a_fn; c_fn],[],[],[],o 
         | [a_fn; c_fn],[l],[],[],o 
         | ["put"],[l],[c_fn],[a_fn],o
         | ["put"; a_fn; c_fn],[l],[],[],o 
         | ["put"; l],[],[c_fn],[a_fn],o
         | ["put"; l; a_fn; c_fn],[],[],[],o -> 
             (* *)
             ["put"],[l],[c_fn],[a_fn],o
         (* sync *)
         | [l; o_fn; c_fn; a_fn],[],[],[],"" 
         | ["sync"; l; o_fn; c_fn; a_fn],[],[],[],"" 
         | ["sync"; o_fn; c_fn; a_fn],[l],[],[],"" -> 
             (* *)
             ["sync"],[l],[c_fn],[a_fn],o_fn
         | ["refresh"; p; l; o_fn; c_fn; a_fn],[],[],[],"" -> 
             ["refresh"],[p;l],[c_fn],[a_fn],o_fn 
         | _ -> bad_cmdline () in 
       let o_fn = if o="" then "-" else o in 
       match rest_pref,ll,cl,al with
         | [prf],[],[],[]               -> sync_profile prf
         | ["get"],[l],[c_fn],[]        -> get l c_fn o_fn
         | ["create"],[l],[],[a_fn]     -> create l a_fn o_fn
         | ["put"],[l],[c_fn],[a_fn]    -> put l a_fn c_fn o_fn
         | ["sync"],[l],[c_fn],[a_fn]   -> sync l o_fn c_fn a_fn
         | ["refresh"],[p_fn;l],[c_fn],[a_fn] -> refresh p_fn l o_fn c_fn a_fn
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
