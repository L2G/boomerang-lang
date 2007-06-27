(* --- provenance lenses --- *)
(* Nate Foster <jnfoster@cis.upenn.edu> *)

open Rvalue 
module L = Rlenses
module SL = L.SLens
module RL = L.RLens
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
    SString ^> SCLens,
    mk_sfun (Info.M "ins built-in") (fun i s1 -> 
      CL(i,uid (),CL.const i (L.rx_epsilon) s1 RS.empty)))

  ; ("cp",
    SRegexp ^> SCLens,
    mk_rfun (Info.M "copy built-in") (fun i r1 -> 
      CL(i,uid (),CL.copy i r1)))

  ; ("const",
    SRegexp ^> SString ^> SString ^> SCLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 -> 
        mk_sfun i (fun i s2 -> 
        CL(i,uid (),CL.const i r1 s1 s2)))))

  ; ("del",
    SRegexp ^> SCLens,
    mk_rfun (Info.M "del built-in") (fun i r1 -> 
      CL(i,uid (),CL.const i r1 RS.empty (wrap_rep i "del built-in" r1))))

  ; ("default",
     SCLens ^> SString ^> SCLens,
    mk_clfun (Info.M "default built-in") (fun i cl1 -> 
      mk_sfun i (fun i def -> 
        CL(i,uid (),CL.default i def cl1))))

  ; ("default_klens",
    SKLens ^> SString ^> SKLens,
    mk_klfun (Info.M "default built-in") (fun i kl1 -> 
      mk_sfun i (fun i def -> 
        KL(i,uid (),KL.default i def kl1))))

  ; ("default_slens",
    SSLens ^> SString ^> SSLens,
    mk_slfun (Info.M "default built-in") (fun i sl1 -> 
      mk_sfun i (fun i def -> 
        SL(i,uid (),SL.default i def sl1))))

  ; ("default_rlens",
    SRLens ^> SString ^> SRLens,
    mk_rlfun (Info.M "default built-in") (fun i rl1 -> 
      mk_sfun i (fun i def -> 
        RL(i,uid (),RL.default i def rl1))))

  ; ("epsilon",
    SRegexp,
    R(Info.M "epsilon built-in",uid (),L.rx_epsilon))

  ; ("empty",
    SRegexp,
    R(Info.M "empty",uid (),L.rx_empty))

  ; ("key",
     SKLens ^> SKLens,
     mk_klfun (Info.M "key built-in") (fun i kl1 -> 
       KL(i,uid (),KL.addkey i kl1)))

  ; ("clens_of_klens",
     SKLens ^> SCLens,
     mk_klfun (Info.M "clens_of_klens built-in") (fun i kl1 -> 
       CL(i,uid (),KL.clens_of_t kl1)))

  ; ("probe",
    SString ^> SCLens ^> SCLens,
    mk_sfun (Info.M "probe built-in") (fun i s1 ->
      mk_clfun i (fun i cl1 ->
        CL(i,uid (),CL.probe i (RS.to_string s1) cl1)))) 

  ; ("probe_klens",
    SString ^> SKLens ^> SKLens,
    mk_sfun (Info.M "probe built-in") (fun i s1 ->
      mk_klfun i (fun i kl1 ->
        KL(i,uid (),KL.probe i (RS.to_string s1) kl1)))) 

  ; ("probe_slens",
    SString ^> SSLens ^> SSLens,
    mk_sfun (Info.M "probe built-in") (fun i s1 ->
      mk_slfun i (fun i sl1 ->
        SL(i,uid (),SL.probe i (RS.to_string s1) sl1)))) 

  ; ("probe_rlens",
    SString ^> SRLens ^> SRLens,
    mk_sfun (Info.M "probe built-in") (fun i s1 ->
      mk_rlfun i (fun i rl1 ->
        RL(i,uid (),RL.probe i (RS.to_string s1) rl1)))) 

  ; ("read",
     SString ^> SString,
     mk_sfun (Info.M "read built-in") (fun i s1 -> 
       S(i,uid (),RS.of_string (Misc.read (RS.to_string s1)))))

  ; ("tr",
    SString ^> SString ^> SCLens,
    mk_sfun (Info.M "translate built-in") (fun i s1 -> 
      mk_sfun i (fun i s2 -> 
        CL(i,uid (),CL.const i (L.rx_str false s1) s2 s1))))

  ; ("canonizer_of_clens",
    SCLens ^> SCanonizer,
    mk_clfun (Info.M "canonizer_of_clens built-in") (fun i cl1 -> 
      CN(i,uid(),CL.canonizer_of_t cl1)))

  ; ("qc", 
     SCanonizer ^> SCLens ^> SCLens, 
     mk_cnfun (Info.M "qc built-in") (fun i cn1 -> 
       mk_clfun i (fun i cl1 -> 
         CL(i,uid(),CL.quotient_c i cn1 cl1))))

  ; ("qa",
     SCanonizer ^> SCLens ^> SCLens, 
     mk_cnfun (Info.M "qa built-in") (fun i cn1 -> 
       mk_clfun i (fun i cl1 -> 
         CL(i,uid(),CL.quotient_a i cn1 cl1))))

  ; ("qc_klens",
     SCanonizer ^> SKLens ^> SKLens, 
     mk_cnfun (Info.M "qc built-in") (fun i cn1 -> 
       mk_klfun i (fun i kl1 -> 
         KL(i,uid(),KL.quotient_c i cn1 kl1))))

  ; ("qa_klens",
     SCanonizer ^> SKLens ^> SKLens, 
     mk_cnfun (Info.M "qa built-in") (fun i cn1 -> 
       mk_klfun i (fun i kl1 -> 
         KL(i,uid(),KL.quotient_a i cn1 kl1))))

  ; ("qc_slens",
     SCanonizer ^> SSLens ^> SSLens, 
     mk_cnfun (Info.M "qc built-in") (fun i cn1 -> 
       mk_slfun i (fun i sl1 -> 
         SL(i,uid(),SL.quotient_c i cn1 sl1))))

  ; ("qa_slens",
     SCanonizer ^> SSLens ^> SSLens, 
     mk_cnfun (Info.M "qa built-in") (fun i cn1 -> 
       mk_slfun i (fun i sl1 -> 
         SL(i,uid(),SL.quotient_a i cn1 sl1))))

  ; ("qc_rlens",
     SCanonizer ^> SRLens ^> SRLens, 
     mk_cnfun (Info.M "qc built-in") (fun i cn1 -> 
       mk_rlfun i (fun i rl1 -> 
         RL(i,uid(),RL.quotient_c i cn1 rl1))))

  ; ("qa_rlens",
     SCanonizer ^> SRLens ^> SRLens, 
     mk_cnfun (Info.M "qa built-in") (fun i cn1 -> 
       mk_rlfun i (fun i rl1 -> 
         RL(i,uid(),RL.quotient_a i cn1 rl1))))

  ; ("string_of_regexp",
     SRegexp ^> SString ,
     mk_rfun(Info.M "string_of_regexp built-in") (fun i r1 ->
      S(i,uid (), RS.of_string (L.string_of_r r1))))

  ; ("equal_rx",
     SRegexp ^> SRegexp ^> SString ,
     mk_rfun(Info.M "are_equal_regexp built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 S(i,uid (), RS.of_string(string_of_bool (L.rx_equiv r1 r2))))))

  ; ("set",
    SRegexp ^> SString ^> SCLens,
    mk_rfun (Info.M "set built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 ->         
        CL(i,uid (),CL.const i r1 s1 (wrap_rep i "set built-in" r1)))))
  
  ; ("filter",
     SRegexp ^> SRegexp ^> SCLens,
     mk_rfun(Info.M "filter built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 CL(i,uid (), CL.filter i r1 r2))))
  
  ; ("move_end",
     SRegexp ^> SRegexp ^> SCLens,
     mk_rfun(Info.M "move_end built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 CL(i,uid (), CL.move_end i r1 r2))))

  ; ("order",
     SRegexp ^> SRegexp ^> SCLens,
     mk_rfun(Info.M "order built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 CL(i,uid (), CL.order i r1 r2))))

  ; ("ignorecase", 
     SString ^> SRegexp,
    mk_sfun (Info.M "ignorecase built-in") (fun i s1 -> 
      R(i,uid(),L.rx_str true s1)))

  ; ("lowercase", 
     SRegexp ^> SCLens,
    mk_rfun (Info.M "lowercase built-in") (fun i r1 -> 
      CL(i,uid(), CL.lowercase i r1)))

  ; ("uppercase", 
     SRegexp ^> SCLens,
    mk_rfun (Info.M "uppercase built-in") (fun i r1 -> 
      CL(i,uid(), CL.uppercase i r1)))

  ; ("ctype",
     SCLens ^> SRegexp, 
     mk_clfun (Info.M "ctype built-in") (fun i cl ->
       R(i, uid (), CL.ctype cl)))

  ; ("ctype_klens",
     SKLens ^> SRegexp, 
     mk_clfun (Info.M "ctype built-in") (fun i cl ->
       R(i, uid (), CL.ctype cl)))

  ; ("atype",
     SCLens ^> SRegexp, 
     mk_clfun (Info.M "atype built-in") (fun i cl ->
       R(i, uid (), CL.atype cl)))

  ; ("atype_klens",
     SKLens ^> SRegexp, 
     mk_clfun (Info.M "atype built-in") (fun i cl ->
       R(i, uid (), CL.atype cl)))

  ; ("clens_of_rlens",
     SRLens ^> SCLens,
     mk_rlfun (Info.M "clens_of_rlens built-in") (fun i rl ->
       CL(i, uid(), RL.clens_of_t rl)))

  ; ("determinize",
     SCLens ^> SCLens,
     mk_clfun (Info.M "determinize built-in") (fun i cl ->
       CL(i, uid(), CL.determinize_clens cl)))
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
           Renv.update venv x (S(i,uid (),RS.of_string y),false)))
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
          if o = "" then bad_cmdline ();
          let rl = get_rl (fst (lookup i env l)) i in     
            match Prefs.read cpref, Prefs.read apref with
              | "","" -> bad_cmdline ()
              | c,"" -> 
                  write_result ((RL.get rl) (fs_of_file c))
              | "",a -> 
                  write_result ((RL.create rl) (fs_of_file a))
              | c,a  -> 
                  write_result ((RL.put rl) (fs_of_file a) (fs_of_file c))       
        end;
    with 
      | Error.Harmony_error(thk) -> thk (); exit 1
      | x -> Erx.print_stat_trim(); raise x);
  Erx.print_stat_trim ();
  exit 0
