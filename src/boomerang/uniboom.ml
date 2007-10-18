let sprintf = Printf.sprintf

let name = "uniboom"
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
        Misc.remove_file_or_dir o_fn;
        None

    | _,true,false -> 
        (* if c exists but a does not, set a to GET c *)
        cp c_fn o_fn;
        get c_fn a_fn;
        None
          
    | true,false,true ->
        (* if c does not exist but a and o do, set c to PUT a o *)        
        put a_fn o_fn c_fn;
        cp c_fn o_fn;
        None
          
    | false,false,true ->
        (* if c and o do not exist but a does, set c to CRT a *)        
        crt a_fn c_fn;
        cp c_fn o_fn;
        None          

    | false,true,true -> 
        (* if c and a exist but o does not and a <> GET c then conflict *)
        let t_fn = tmp_fn a_fn in 
        get c_fn t_fn;
        if eq a_fn t_fn then
          (cp c_fn o_fn; 
           None)
        else 
          Some (c_fn, a_fn)
    | true,true,true -> 
        (* otherwise, c, a, and o exist:
           - if c=o, set c to PUT a o
           - else if a=GET o set a to GET c
           - otherwise conflict *)
        if eq c_fn o_fn then
          (put a_fn o_fn c_fn;
           cp c_fn o_fn;
           None)
        else 
          (let t_fn = tmp_fn a_fn in 
             get o_fn t_fn;             
            if eq a_fn t_fn then 
              (get c_fn a_fn;
               cp c_fn o_fn;                
               None)
            else 
              Some (c_fn,a_fn))
            
let usage = sprintf "%s [options]" name

let l_pref = Prefs.createStringList "lens" "lens" "lens"
let c_pref = Prefs.createStringList "concrete" "concrete" "abstract"
let a_pref = Prefs.createStringList "abstract" "abstract" "abstract"

let () = 
  Prefs.profileName := Some "uniboom";
  Prefs.parseCmdLine usage;
  Prefs.loadTheFile ()

let ll = Prefs.read l_pref  
let cl = Prefs.read c_pref  
let al = Prefs.read a_pref  
let ll_len = Safelist.length ll 
let cl_len = Safelist.length cl 
let al_len = Safelist.length al 
let () = if cl_len <> al_len then 
    Error.simple_error "number of concretes and abstracts differs"
  else if cl_len <> ll_len then 
    Error.simple_error "number of files and lenses differs"

let () = debug (sprintf "%d %d %d\n" ll_len cl_len al_len)

let rec aux ll cl al acc = match ll,cl,al with 
  | [],[],[] -> 
      Safelist.rev acc
  | l::lrest,c::crest,a::arest -> 
      aux lrest crest arest ((go l c a)::acc)
  | _ -> assert false 
    
let () = Safelist.iter
  (function None -> ()
    | Some (c_fn,a_fn) -> 
        Util.format "Conflict on %s and %s\n" c_fn a_fn)
    (aux ll cl al [])
