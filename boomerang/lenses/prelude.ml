open Bvalue 
open Bsyntax
module L = Blenses.DLens
module R = Bregexp
module RS = Bstring
let (^) = Pervasives.(^)

let wrap_rep i msg r = 
  try 
    R.rep r 
  with Not_found -> 
    raise (Error.Harmony_error(fun () -> 
      Util.format "%s: %s: cannot calculate representative; %s is empty."
        (Info.string_of_t i)
        msg
        (R.string_of_t r)))

let prelude_spec =
  [ (mk_prelude_qid "get", 
    SLens ^> SString ^> SString,
    mk_lfun (Info.M "get built-in") (fun i l1 -> 
      mk_sfun i (fun i s1 -> 
        Str(i,L.get l1 s1))))

  ; (mk_prelude_qid "put", 
    SLens ^> SString ^> SString ^> SString,
    mk_lfun (Info.M "put built-in") (fun i l1 -> 
      mk_sfun i (fun i s1 -> 
        mk_sfun i (fun i s2 -> 
          Str(i,L.rput_of_dl l1 s1 s2)))))

  ; (mk_prelude_qid "create", 
    SLens ^> SString ^> SString,
    mk_lfun (Info.M "crt built-in") (fun i l1 -> 
      mk_sfun i (fun i s1 -> 
        Str(i,L.rcreate_of_dl l1 s1))))

  ; (mk_prelude_qid "invert",
     SLens ^> SLens,
     mk_lfun (Info.M "invert built-in") (fun i l1 -> 
       Lns(i,L.invert i l1)))
    
  ; (mk_prelude_qid "ins",
    SString ^> SLens,
    mk_sfun (Info.M "ins built-in") (fun i s1 -> 
      Lns(i,L.const i (R.epsilon) s1 RS.empty)))

  ; (mk_prelude_qid "cp",
    SRegexp ^> SLens,
    mk_rfun (Info.M "copy built-in") (fun i r1 -> 
      Lns(i,L.copy i r1)))

  ; (mk_prelude_qid "const",
    SRegexp ^> SString ^> SString ^> SLens,
    mk_rfun (Info.M "const built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 -> 
        mk_sfun i (fun i s2 -> 
        Lns(i,L.const i r1 s1 s2)))))

  ; (mk_prelude_qid "qconst",
    SRegexp ^> SRegexp ^> SString ^> SString ^> SLens,
    mk_rfun (Info.M "qconst built-in") (fun i r1 -> 
      mk_rfun i (fun i r2 ->
        mk_sfun i (fun i s1 -> 
          mk_sfun i (fun i s2 -> 
          Lns(i,L.qconst i r1 r2 s1 s2))))))

  ; (mk_prelude_qid "del",
    SRegexp ^> SLens,
    mk_rfun (Info.M "del built-in") (fun i r1 -> 
      Lns(i,L.const i r1 RS.empty (wrap_rep i "del built-in" r1))))

  ; (mk_prelude_qid "default",
     SLens ^> SString ^> SLens,
    mk_lfun (Info.M "default built-in") (fun i cl1 -> 
      mk_sfun i (fun i def -> 
        Lns(i,L.default i def cl1))))

  ; (mk_prelude_qid "epsilon",
    SRegexp,
    Rx(Info.M "epsilon built-in",R.epsilon))

  ; (mk_prelude_qid "empty",
    SRegexp,
    Rx(Info.M "empty",R.empty))

  ; (mk_prelude_qid "key",
     SRegexp ^> SLens,
     mk_rfun (Info.M "key built-in") (fun i r -> 
       Lns(i,L.key i r)))

  ; (mk_prelude_qid "read",
     SString ^> SString,
     mk_sfun (Info.M "read built-in") (fun i s1 -> 
       Str(i,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  ; (mk_prelude_qid "tr",
    SString ^> SString ^> SLens,
    mk_sfun (Info.M "translate built-in") (fun i s1 -> 
      mk_sfun i (fun i s2 -> 
        Lns(i,L.const i (R.str false s1) s2 s1))))

  ; (mk_prelude_qid "string_of_regexp",
     SRegexp ^> SString ,
     mk_rfun(Info.M "string_of_regexp built-in") (fun i r1 ->
      Str(i, RS.t_of_string (R.string_of_t r1))))

  ; (mk_prelude_qid "equal_rx",
     SRegexp ^> SRegexp ^> SString ,
     mk_rfun(Info.M "equal_rx built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 Str(i, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))

  ; (mk_prelude_qid "set",
    SRegexp ^> SString ^> SLens,
    mk_rfun (Info.M "set built-in") (fun i r1 -> 
      mk_sfun i (fun i s1 ->         
        Lns(i,L.const i r1 s1 (wrap_rep i "set built-in" r1)))))


  ; (mk_prelude_qid "swap",
    SLens ^> SLens ^> SLens,
    mk_lfun (Info.M "swap built-in") (fun i l1 -> 
      mk_lfun i (fun i l2 ->         
        Lns(i,L.swap i l1 l2))))

  ; (mk_prelude_qid "ctype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "ctype built-in") (fun i cl ->
       Rx(i, L.ctype cl)))

  ; (mk_prelude_qid "atype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "atype built-in") (fun i cl ->
       Rx(i, L.atype cl)))

  ; (mk_prelude_qid "assert",
     SRegexp ^> SRegexp ^> SLens ^> SLens,
     mk_rfun (Info.M "assert built-in") (fun i c -> 
       mk_rfun i (fun i a -> 
         mk_lfun i (fun i l -> 
           Lns(i,L.assert_lens_type i l (Some c) (Some a))))))

  ; (mk_prelude_qid "assert_ctype",
     SRegexp ^> SLens ^> SLens,
     mk_rfun (Info.M "assert_ctype built-in") (fun i c -> 
       mk_lfun i (fun i l -> 
         Lns(i,L.assert_lens_ctype i l c))))

  ; (mk_prelude_qid "assert_atype",
     SRegexp ^> SLens ^> SLens,
     mk_rfun (Info.M "assert_atype built-in") (fun i a -> 
       mk_lfun i (fun i l -> 
         Lns(i,L.assert_lens_atype i l a))))

  ; (mk_prelude_qid "determinize",
     SLens ^> SLens,
     mk_lfun (Info.M "determinize built-in") (fun i cl ->
       Lns(i, L.determinize_dlens cl)))

  ; (mk_prelude_qid "forgetkey",
     SLens ^> SLens,
     mk_lfun (Info.M "forgetkey built-in") (fun i cl ->
       Lns(i, L.forgetkey cl)))

  ; (mk_prelude_qid "filter",
     SRegexp ^> SRegexp ^> SLens,
     mk_rfun(Info.M "filter built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
         Lns(i,L.filter i r1 r2))))

  ; (mk_prelude_qid "lowercase",
     SRegexp ^> SRegexp,
     mk_rfun(Info.M "lowercase built-in") (fun i r1 ->
       Rx(i, R.lowercase r1)))

  ; (mk_prelude_qid "canonizer_of_lens",
     SLens ^> SCanonizer,
     mk_lfun (Info.M "canonizer_of_lens built-in") (fun i l1 -> 
       Can(i,L.canonizer_of_t i l1)))

  ; (mk_prelude_qid "left_quot",
     SCanonizer ^> SLens ^> SLens,
     mk_cfun (Info.M "left_quot built-in") (fun i c1 ->                                              
       mk_lfun i (fun i l1 -> 
         Lns(i,L.left_quot i c1 l1))))

  ; (mk_prelude_qid "right_quot",
     SLens ^> SCanonizer ^> SLens,
     mk_lfun (Info.M "right_quot built-in") (fun i l1 ->                                              
       mk_cfun i (fun i c1 -> 
         Lns(i,L.right_quot i l1 c1))))
   ]
      
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v)
    prelude_spec
