let sprintf = Printf.sprintf

let name = "boomison"

let () = Util.supplyFileInUnisonDirFn (fun s -> sprintf "./.%s/%s" name s)

let debug s = Trace.debug "uniboom" (fun () -> Util.format "%s" s)

let archive_fn n = Util.fileInUnisonDir (sprintf ".#%s" n) 
let tmp_fn n = Util.fileInUnisonDir (sprintf ".#%s-tmp" n) 

let go l c_fn a_fn = 
  let o_fn = archive_fn c_fn in 
  let cp fn1 fn2 = Misc.write fn2 (Misc.read fn1) in 
  let eq fn1 fn2 = Misc.read fn1 = Misc.read fn2 in 
  let get c a = ignore (Sys.command (sprintf "boomerang -c %s -o %s %s" c a l)) in 
  let put a o c = ignore (Sys.command (sprintf "boomerang -a %s -c %s -o %s %s" a o c l)) in 
  let crt a c = ignore (Sys.command (sprintf "boomerang -a %s -o %s %s" a c l)) in 
  match Sys.file_exists o_fn, Sys.file_exists c_fn, Sys.file_exists a_fn with 
    | _,false,false -> 
        (* if neither c nor a exists, clear o and return *)
        Misc.remove_file_or_dir o_fn

    | _,true,false -> 
        (* if c exists but a does not, set a to GET c *)
        cp c_fn o_fn;
        get c_fn a_fn;
        debug (sprintf "%s -- get(%s) --> %s\n" c_fn l a_fn)
          
    | true,false,true ->
        (* if c does not exist but a and o do, set c to PUT a o *)        
        put a_fn o_fn c_fn;
        cp c_fn o_fn;
        debug (sprintf "(%s,%s) -- put(%s) --> %s\n" a_fn o_fn l c_fn)
          
    | false,false,true ->
        (* if c and o do not exist but a does, set c to CRT a *)        
        crt a_fn c_fn;
        cp c_fn o_fn;
        debug (sprintf "%s -- create(%s) --> %s\n" a_fn l c_fn)

    | false,true,true -> 
        (* if c and a exist but o does not and a <> GET c then conflict *)
        let t_fn = tmp_fn a_fn in 
        get c_fn t_fn;
        if eq a_fn t_fn then
          cp c_fn o_fn
        else 
          debug (sprintf "%s --> conflict <-- %s\n" c_fn a_fn)

    | true,true,true -> 
        (* otherwise, c, a, and o exist:
           - if c=o, set c to PUT a o
           - else if a=GET o set a to GET c
           - otherwise conflict *)
        let t_fn = tmp_fn a_fn in 
        get o_fn t_fn;
        if eq a_fn t_fn then 
          (get c_fn a_fn;
           cp c_fn o_fn;
           debug (sprintf "%s -- get(%s) --> %s\n" c_fn l a_fn))
        else if eq c_fn o_fn then 
          (put a_fn o_fn c_fn;
           cp c_fn o_fn;
           debug (sprintf "(%s,%s) -- put(%s) --> %s\n" a_fn o_fn l c_fn))
        else 
          debug (sprintf "%s --> conflict <-- %s\n" c_fn a_fn)
            
let usage = sprintf "%s [options]" name

let l_pref = Prefs.createStringList "lens" "lens" "lens"
let c_pref = Prefs.createStringList "concrete" "concrete" "abstract"
let a_pref = Prefs.createStringList "abstract" "abstract" "abstract"

let profile_name = "default" 
let profile_fn = Prefs.profilePathname profile_name 

let main () = 
  Prefs.profileName := Some profile_name;
  Prefs.parseCmdLine usage;
  if not (Sys.file_exists profile_fn) then 
    Error.simple_error (sprintf "Error: profile file %s does not exist" profile_fn)
  else
    Prefs.loadTheFile ();

  let ll = Prefs.read l_pref in 
  let cl = Prefs.read c_pref in
  let al = Prefs.read a_pref in 
  let ll_len = Safelist.length ll in 
  let cl_len = Safelist.length cl in
  let al_len = Safelist.length al in
  if cl_len <> al_len then 
    Error.simple_error "Error: number of concrete and abstract replicas differs"
  else if cl_len <> ll_len then 
    Error.simple_error "Error: number of replicas and lenses differs"
      
  let rec loop ll cl al = match ll,cl,al with 
    | [],[],[] -> ()
    | l::lrest,c::crest,a::arest -> 
        go l c a;
        loop lrest crest arest 
    | _ -> assert false in 
  loop ll cl al 

let () = Error.exit_on_error main
