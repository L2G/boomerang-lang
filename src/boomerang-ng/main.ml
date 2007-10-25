(* --- provenance lenses --- *)
(* Nate Foster <jnfoster@cis.upenn.edu> *)

open Rvalue 
open Rsyntax
module L = Rlenses.DLens
module R = Rlenses
module RS = Rstring
let (^) = Pervasives.(^)

let _ = Rdriver.init ()

let exit x = 
  Memo.format_stats ();
  exit x

let _ = Prefs.set Trace.logging false

let wrap_rep i msg r = 
  try 
    R.rep r 
  with Not_found -> 
    raise (Error.Harmony_error(fun () -> 
      Util.format "%s: %s: cannot calculate representative; %s is empty."
        (Info.string_of_t i)
        msg
        (R.string_of_r r)))

let prelude_spec =
  [ ("ins",
    SString ^> SLens,
    mk_sfun (Info.M "ins built-in") (fun i s1 -> 
      L(i,L.const i (R.rx_epsilon) s1 RS.empty)))

  ; ("cp",
    SRegexp ^> SLens,
    mk_rfun (Info.M "copy built-in") (fun i r1 -> 
      L(i,L.copy i r1)))

  ; ("const",
    SRegexp ^> SString ^> SString ^> SLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 -> 
        mk_sfun i (fun i s2 -> 
        L(i,L.const i r1 s1 s2)))))

  ; ("qconst",
    SRegexp ^> SRegexp ^> SString ^> SString ^> SLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_rfun i (fun i r2 ->
        mk_sfun i (fun i s1 -> 
          mk_sfun i (fun i s2 -> 
          L(i,L.qconst i r1 r2 s1 s2))))))

  ; ("del",
    SRegexp ^> SLens,
    mk_rfun (Info.M "del built-in") (fun i r1 -> 
      L(i,L.const i r1 RS.empty (wrap_rep i "del built-in" r1))))

  ; ("default",
     SLens ^> SString ^> SLens,
    mk_lfun (Info.M "default built-in") (fun i cl1 -> 
      mk_sfun i (fun i def -> 
        L(i,L.default i def cl1))))

  ; ("epsilon",
    SRegexp,
    R(Info.M "epsilon built-in",R.rx_epsilon))

  ; ("empty",
    SRegexp,
    R(Info.M "empty",R.rx_empty))

  ; ("key",
     SRegexp ^> SLens,
     mk_rfun (Info.M "key built-in") (fun i r -> 
       L(i,L.key i r)))

  ; ("read",
     SString ^> SString,
     mk_sfun (Info.M "read built-in") (fun i s1 -> 
       S(i,RS.of_string (Misc.read (RS.to_string s1)))))

  ; ("tr",
    SString ^> SString ^> SLens,
    mk_sfun (Info.M "translate built-in") (fun i s1 -> 
      mk_sfun i (fun i s2 -> 
        L(i,L.const i (R.rx_str false s1) s2 s1))))

  ; ("string_of_regexp",
     SRegexp ^> SString ,
     mk_rfun(Info.M "string_of_regexp built-in") (fun i r1 ->
      S(i, RS.of_string (R.string_of_r r1))))

  ; ("equal_rx",
     SRegexp ^> SRegexp ^> SString ,
     mk_rfun(Info.M "equal_rx built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 S(i, RS.of_string(string_of_bool (R.rx_equiv r1 r2))))))

  ; ("set",
    SRegexp ^> SString ^> SLens,
    mk_rfun (Info.M "set built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 ->         
        L(i,L.const i r1 s1 (wrap_rep i "set built-in" r1)))))

  ; ("ctype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "ctype built-in") (fun i cl ->
       R(i, L.ctype cl)))

  ; ("atype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "atype built-in") (fun i cl ->
       R(i, L.atype cl)))

  ; ("determinize",
     SLens ^> SLens,
     mk_lfun (Info.M "determinize built-in") (fun i cl ->
       L(i, L.determinize_dlens cl)))

  ; ("forgetkey",
     SLens ^> SLens,
     mk_lfun (Info.M "forgetkey built-in") (fun i cl ->
       L(i, L.forgetkey cl)))

  ; ("filter",
     SRegexp ^> SRegexp ^> SLens,
     mk_rfun(Info.M "filter built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
		    L(i,L.filter i r1 r2))))

  ; ("lowercase",
     SRegexp ^> SRegexp,
     mk_rfun(Info.M "lowercase built-in") (fun i r1 ->
       R(i, R.rx_lowercase r1)))
   ]
      
let () = 
  Safelist.iter 
    (fun (x,s,v) -> 
      Rregistry.register_native 
        (sprintf "Prelude.%s"  x) 
        s v)
    prelude_spec

let rest = Prefs.createStringList "rest" "*no docs needed" ""

let cpref = Prefs.createString "c" "" "concrete file" ""
let apref = Prefs.createString "a" "" "abtsract file" ""
let lpref = Prefs.createString "l" "" "R-lens identifier" ""
let opref = Prefs.createString "o" "" "output file" ""
let setpref = Prefs.createStringList "set" "command-line arguments" ""

let usageMsg = "usage: boomerang -l NAME -c FILE [-a FILE] [-o FILE -set x=y ...]\n"
 
let bad_cmdline () = 
  Prefs.printUsage usageMsg; 
  exit 2 

let _ =   
  (try 
    Prefs.parseCmdLine usageMsg;
    let i = Info.M "command-line argument string" in 
    let () = Safelist.iter 
      (fun arg -> 
        let eq = 
          try String.index arg '='
          with Not_found -> 
            Error.simple_error (sprintf "can't set %s" arg) in 
        let x = String.sub arg 0 eq in 
        let y = String.sub arg (succ eq) (String.length arg - eq -1) in 
        Rregistry.register_native x SString (S(i,RS.of_string y)))
      (Prefs.read setpref) in 
    let i = Info.M "Top-level loop" in 
    let fs_of_file f = RS.of_string (Misc.read f) in 
    let o = Prefs.read opref in 
    let write_result fs = 
      if o <> "" then Misc.write o (RS.to_string fs) 
      else Util.format "%s" (RS.to_string fs) in 
    let l_str = Prefs.read lpref in 
      if l_str <> "" then 
        begin          
          let lens = match Rregistry.lookup_library (Rvalue.parse_qid l_str) with
            | None -> Error.simple_error (sprintf "can't find %s" l_str)
            | Some rv -> Rvalue.get_l (Rregistry.value_of_rv rv) i in 
	  match Prefs.read cpref, Prefs.read apref with
	    | "","" -> bad_cmdline ()
	    | c,"" -> 
		write_result ((L.get lens) (fs_of_file c))
	    | "",a -> 
		write_result ((L.rcreate_of_dl lens) (fs_of_file a))
	    | c,a  -> 
		write_result ((L.rput_of_dl lens) (fs_of_file a) (fs_of_file c))
        end;
    with 
      | Error.Harmony_error(thk) -> thk (); exit 1
      | x -> Erx.print_stat_trim(); raise x);
  Erx.print_stat_trim ();
  exit 0
