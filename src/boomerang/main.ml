(* --- provenance lenses --- *)
(* Nate Foster <jnfoster@cis.upenn.edu> *)

open Rvalue 
module L = Rlenses
module DL = L.DLens
module RS = Rstring
let (^) = Pervasives.(^)

let exit x = 
  Memo.format_stats ();
  exit x

let _ = Prefs.set Trace.logging false

let wrap_rep i msg r = 
  try 
    L.rep r 
  with Not_found -> 
    raise (Error.Harmony_error(fun () -> 
      Util.format "%s: %s: cannot calculate representative; %s is empty."
        (Info.string_of_t i)
        msg
        (L.string_of_r r)))

let prelude_spec =
  [ ("ins",
    SString ^> SDLens,
    mk_sfun (Info.M "ins built-in") (fun i s1 -> 
      DL(i,DL.const i (L.rx_epsilon) s1 RS.empty)))

  ; ("cp",
    SRegexp ^> SDLens,
    mk_rfun (Info.M "copy built-in") (fun i r1 -> 
      DL(i,DL.copy i r1)))

  ; ("const",
    SRegexp ^> SString ^> SString ^> SDLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 -> 
        mk_sfun i (fun i s2 -> 
        DL(i,DL.const i r1 s1 s2)))))

  ; ("qconst",
    SRegexp ^> SRegexp ^> SString ^> SString ^> SDLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_rfun i (fun i r2 ->
        mk_sfun i (fun i s1 -> 
          mk_sfun i (fun i s2 -> 
          DL(i,DL.qconst i r1 r2 s1 s2))))))

  ; ("del",
    SRegexp ^> SDLens,
    mk_rfun (Info.M "del built-in") (fun i r1 -> 
      DL(i,DL.const i r1 RS.empty (wrap_rep i "del built-in" r1))))

  ; ("default",
     SDLens ^> SString ^> SDLens,
    mk_dlfun (Info.M "default built-in") (fun i cl1 -> 
      mk_sfun i (fun i def -> 
        DL(i,DL.default i def cl1))))

  ; ("epsilon",
    SRegexp,
    R(Info.M "epsilon built-in",L.rx_epsilon))

  ; ("empty",
    SRegexp,
    R(Info.M "empty",L.rx_empty))

  ; ("key",
     SRegexp ^> SDLens,
     mk_rfun (Info.M "key built-in") (fun i r -> 
       DL(i,DL.key i r)))

  ; ("read",
     SString ^> SString,
     mk_sfun (Info.M "read built-in") (fun i s1 -> 
       S(i,RS.of_string (Misc.read (RS.to_string s1)))))

  ; ("tr",
    SString ^> SString ^> SDLens,
    mk_sfun (Info.M "translate built-in") (fun i s1 -> 
      mk_sfun i (fun i s2 -> 
        DL(i,DL.const i (L.rx_str false s1) s2 s1))))

  ; ("canonizer_of_dlens",
    SDLens ^> SCanonizer,
    mk_dlfun (Info.M "canonizer_of_clens built-in") (fun i cl1 -> 
      CN(i,DL.canonizer_of_t i cl1)))

  ; ("string_of_regexp",
     SRegexp ^> SString ,
     mk_rfun(Info.M "string_of_regexp built-in") (fun i r1 ->
      S(i, RS.of_string (L.string_of_r r1))))

  ; ("equal_rx",
     SRegexp ^> SRegexp ^> SString ,
     mk_rfun(Info.M "are_equal_regexp built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 S(i, RS.of_string(string_of_bool (L.rx_equiv r1 r2))))))

  ; ("set",
    SRegexp ^> SString ^> SDLens,
    mk_rfun (Info.M "set built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 ->         
        DL(i,DL.const i r1 s1 (wrap_rep i "set built-in" r1)))))

  ; ("ctype",
     SDLens ^> SRegexp, 
     mk_dlfun (Info.M "ctype built-in") (fun i cl ->
       R(i, DL.ctype cl)))

  ; ("atype",
     SDLens ^> SRegexp, 
     mk_dlfun (Info.M "atype built-in") (fun i cl ->
       R(i, DL.atype cl)))

  ; ("determinize",
     SDLens ^> SDLens,
     mk_dlfun (Info.M "determinize built-in") (fun i cl ->
       DL(i, DL.determinize_dlens cl)))

  ; ("forgetkey",
     SDLens ^> SDLens,
     mk_dlfun (Info.M "forgetkey built-in") (fun i cl ->
       DL(i, DL.forgetkey cl)))

  ; ("filter",
     SRegexp ^> SRegexp ^> SDLens,
     mk_rfun(Info.M "filter built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
		    DL(i,DL.filter i r1 r2))))

  ; ("lowercase",
     SRegexp ^> SRegexp,
     mk_rfun(Info.M "lowercase built-in") (fun i r1 ->
       R(i, L.rx_lowercase r1)))

   ]
      
let init_env = 
  Safelist.fold_left (fun (se,ve) (x,s,v) -> 
    (Renv.update se x s, Renv.update ve x (v,true)))
    (Renv.empty (), Renv.empty ())
    prelude_spec

let go fn e = 
  let fcl_buf = Src2fcl.fcl_of_src fn in 
  let _ = Rlexer.setup fn in 
  let lexbuf = Lexing.from_string fcl_buf in 
    try
      let pl = Rparser.top Rlexer.main lexbuf in             
        (pl e)
    with
        Parsing.Parse_error -> 
          Util.format "@[%s: Parse error@\n@]%!" (Info.string_of_t (Rlexer.info lexbuf));
          exit 1

let rest = Prefs.createStringList "rest" "*no docs needed" ""

let cpref = Prefs.createString "c" "" "concrete file" ""
let apref = Prefs.createString "a" "" "abtsract file" ""
let lpref = Prefs.createString "l" "" "R-lens identifier" ""
let opref = Prefs.createString "o" "" "output file" ""
let setpref = Prefs.createStringList "set" "command-line arguments" ""

let usageMsg = 
    "usage: boomerang -l NAME -o FILE -c FILE [-a FILE] [-set x=y ...] FILE.fcl ... \n"
 
let bad_cmdline () = 
  Prefs.printUsage usageMsg; 
  exit 2 
    
let _ =   
  (try 
    Prefs.parseCmdLine usageMsg;
    let i = Info.M "command-line argument string" in 
    let cmdline_env = Safelist.fold_left 
      (fun (senv,venv) arg -> 
        let eq = try String.index arg '=' with Not_found -> 
          Util.format "bad argument to set: no occurrence of '=' in %s@\n" arg;
          exit 1 in 
        let x = String.sub arg 0 eq in 
        let y = String.sub arg (succ eq) (String.length arg - eq -1) in 
          (Renv.update senv x SString,
           Renv.update venv x (S(i,RS.of_string y),false)))
      init_env (Prefs.read setpref) in 
    let _,env = 
      Safelist.fold_left
        (fun env fi -> go fi env) 
        cmdline_env 
        (Prefs.read rest) in 
    let i = Info.M "Top-level loop" in 
    let fs_of_file f = RS.of_string (Misc.read f) in 
    let o = Prefs.read opref in 
    let write_result fs = Misc.write o (RS.to_string fs) in 
    let l = Prefs.read lpref in 
      if l <> "" then 
        begin          
          match fst (lookup i env l) with
	    | StL (_, stl) ->
		( match Prefs.read cpref, Prefs.read apref with
		    | "","" -> bad_cmdline ()
		    | c,"" -> 
			let buf = Buffer.create 80 in 
			let wic = Wic.create buf (open_in_bin c) in
			let woc = Woc.t_of_channel (if o = "" then stdout else open_out_bin o) in
			  Rlenses.StLens.get wic woc stl
		    | "", a -> 
			let buf = Buffer.create 80 in 
			let wic = Wic.create buf (open_in_bin a) in
			let woc = Woc.t_of_channel (if o = "" then stdout else open_out_bin o) in
			  Rlenses.StLens.create wic woc stl
		    | c, a -> 
			let buf = Buffer.create 80 in 
			let wic = Wic.create buf (open_in_bin c) in
			let dict = Rlenses.StLens.parse wic stl in 
			let bufc = Buffer.create 80 in 			
			let wicc = Wic.create bufc (open_in_bin c) in
			let bufa = Buffer.create 80 in
			let wica = Wic.create bufa (open_in_bin a) in
			let woc = Woc.t_of_channel (if o = "" then stdout else open_out_bin o) in
			  Rlenses.StLens.put wicc wica woc dict stl)
	    | x ->
		(if o = "" then bad_cmdline ();
		 let dl = get_dl x i in     
		   match Prefs.read cpref, Prefs.read apref with
		     | "","" -> bad_cmdline ()
		     | c,"" -> 
			 write_result ((DL.get dl) (fs_of_file c))
		     | "",a -> 
			 write_result ((DL.rcreate_of_dl dl) (fs_of_file a))
		     | c,a  -> 
			 write_result ((DL.rput_of_dl dl) (fs_of_file a) (fs_of_file c)))       
	end;
    with 
      | Error.Harmony_error(thk) -> thk (); exit 1
      | x -> Erx.print_stat_trim(); raise x);
  Erx.print_stat_trim ();
  exit 0
