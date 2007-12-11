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
    
let exit x = 
  Memo.format_stats ();
  exit x

let debug thk = Trace.debug "toplevel" (fun () -> thk (); Util.format "%!")

let lookup qid_str = Bregistry.lookup_library (Bvalue.parse_qid qid_str)

let lookup_lens qid_str =
  match lookup qid_str with
      None -> Error.simple_error (Printf.sprintf "lens %s not found" qid_str)
    | Some rv -> 
        Bvalue.get_l 
          (Bregistry.value_of_rv rv)
          (Info.M (Printf.sprintf "%s is not a lens" qid_str))
          
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
        Misc.remove_file_or_dir o_fn;
        0

    | _,true,false -> 
        (* if c exists but a does not, set a to GET c *)
        debug (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);
        let c = read_string c_fn in 
          write_string o_fn c;
          write_string a_fn (get_str l c);
          0
          
    | true,false,true ->
        (* if c does not exist but a and o do, set c and o to PUT a o *)        
        debug (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
        let a = read_string a_fn in
        let o = read_string o_fn in 
        let c' = put_str l a o in 
          write_string c_fn c';
          write_string o_fn c';
          0
    
    | false,false,true ->
        (* if c and o do not exist but a does, set c and o to CREATE a *)        
        debug (fun () -> Util.format "(lens: %s) %s <-- create -- %s\n" l c_fn a_fn);
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
            debug (fun () -> Util.format "(lens: %s) setting %s to %s" l o_fn c_fn);
            write_string o_fn c
          end
        else
          debug (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);          
        0

    | true,true,true -> 
        (* otherwise, c, a, and o exist:
           - if a=GET o set a to GET c and o to c;
           - else if c=o, set c and o to PUT a o;
           - otherwise conflict. *)
        let o = read_string o_fn in
        let c = read_string c_fn in 
        let a = read_string a_fn in 
        let a' = get_str l o in 
        if BS.equal a a' then 
          begin 
            debug (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);            
            write_string a_fn (get_str l c);
            write_string o_fn c;
            0
          end
        else if BS.equal c o then
          begin 
            debug (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn);
            let c' = put_str l a o in 
            write_string c_fn c';
            write_string o_fn c';
            0
          end            
        else 
          begin 
            debug (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);
            1
          end

let rest = Prefs.createStringList "rest" "*no docs needed" ""

let opref =  Prefs.createString "o" "" "output" ""
let lpref = Prefs.createString "l" "" "lens" ""

let l_pref = Prefs.createStringList "lens" "lens" "lens"
let c_pref = Prefs.createStringList "concrete" "concrete" "abstract"
let a_pref = Prefs.createStringList "abstract" "abstract" "abstract"

let check_pref = Prefs.createStringList "check" "run unit tests for given module(s)" ""

let dryrun = Prefs.createBool "dryrun" false "don't write any files" ""

let toplevel' progName () = 
  let usageMsg = 
    "Usage:\n"
    ^ "    "^progName^" [get] C            [options] - get\n"
    ^ "    "^progName^" [put] A C          [options] - put\n"
    ^ "    "^progName^" create A           [options] - create\n"
    ^ "    "^progName^" sync O A C         [options] - sync\n"
    ^ "    "^progName^" [profilename]      [options] - sync\n"
    ^ "    "^progName^" F.boom [F.boom...] [options] - run unit tests\n"
    ^ "\n"
    ^ "Options:" in 

  let sync_profile profile_name = 
    Prefs.profileName := Some profile_name;
    let profile_fn = Prefs.profilePathname profile_name in       
      if Sys.file_exists profile_fn then 
        Prefs.loadTheFile ()
      else if profile_name = "default" then 
        begin 
          Prefs.printUsage usageMsg; 
          exit 2
        end
      else
        Error.simple_error 
          (sprintf "Error: profile file %s does not exist" profile_fn);
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
  
  let get_lens () = 
    let l = Prefs.read lpref in 
      if l="" then 
        Error.simple_error "-l preference must be specified explicitly"
      else l in 
  let get_out () = 
    let o = Prefs.read opref in 
      if o="" then "-"
      else o in 

  Prefs.parseCmdLine usageMsg;

  let rest_pref = Safelist.rev (Prefs.read rest) in 

  (* run unit tests *)
  if Prefs.read check_pref <> [] then
    begin
      Safelist.iter check (Prefs.read check_pref);
      if Prefs.read rest = [] then exit 0
    end;
  Util.finalize 
    (fun () -> 
       if rest_pref <> [] && Safelist.for_all (fun s -> Util.endswith s ".boom") rest_pref then 
         begin
           Prefs.set Bcompiler.test_all true;
           Safelist.iter check rest_pref;
           0
         end
       else 
         begin 
           let rest_pref = match rest_pref with 
             | []        -> ["default"]
             | [arg]     -> 
                 if Util.endswith arg ".prf" then [Misc.replace_suffix arg ".prf" ""]
                 else ["get"; arg]
             | ["create";_] -> rest_pref
             | [_;_] -> "put"::rest_pref 
             | _ -> rest_pref in 
           match rest_pref with
             | [prf]                      -> sync_profile prf
             | ["get"; c_fn]              -> get (get_lens ()) c_fn (get_out ())
             | ["create"; a_fn]           -> create (get_lens ()) a_fn (get_out ())
             | ["put"; a_fn; c_fn]        -> put (get_lens ()) a_fn c_fn (get_out ())
             | ["sync"; o_fn; c_fn; a_fn] -> sync (get_lens ()) o_fn a_fn c_fn
             | _ -> 
                 Prefs.printUsage usageMsg; 
                 exit 2
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