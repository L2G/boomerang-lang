open Bvalue 
open Bsyntax
module L = Blenses.DLens
module R = Bregexp
module RS = Bstring
let (^) = Pervasives.(^)

let wrap_rep i r = 
  try 
    R.rep r 
  with Not_found -> 
    raise (Error.Harmony_error(fun () -> 
      Util.format "%s: cannot calculate representative; %s is empty."
        (Info.string_of_t i)
        (R.string_of_t r)))

let prelude_spec =
  [(* lens operations *)
   (mk_native_prelude_qid "get", 
    SLens ^> SString ^> SString,
    mk_lfun (Info.M "get built-in") (SString ^> SString) (fun i l1 -> 
      mk_sfun i SString (fun i s1 -> 
        Str(i,L.get l1 s1))))

  ; (mk_native_prelude_qid "put", 
    SLens ^> SString ^> SString ^> SString,
    mk_lfun (Info.M "put built-in") (SString ^> SString ^> SString) (fun i l1 -> 
      mk_sfun i (SString ^> SString) (fun i s1 -> 
        mk_sfun i SString (fun i s2 -> 
          Str(i,L.rput_of_dl l1 s1 s2)))))

  ; (mk_native_prelude_qid "create", 
    SLens ^> SString ^> SString,
    mk_lfun (Info.M "crt built-in") (SString ^> SString) (fun i l1 -> 
      mk_sfun i SString (fun i s1 -> 
        Str(i,L.rcreate_of_dl l1 s1))))

  ; (mk_native_prelude_qid "invert",
     SLens ^> SLens,
     mk_lfun (Info.M "invert built-in") SLens (fun i l1 -> 
       Lns(i,L.invert i l1)))

  (* core lens combinators *)
  ; (mk_native_prelude_qid "copy",
    SRegexp ^> SLens,
    mk_rfun (Info.M "copy built-in") SLens (fun i r1 -> 
      Lns(i,L.copy i r1)))

  ; (mk_native_prelude_qid "const",
    SRegexp ^> SString ^> SString ^> SLens,
    mk_rfun (Info.M "const built-in") (SString ^> SString ^> SLens) (fun i r1 -> 
      mk_sfun i (SString ^> SLens) (fun i s1 -> 
        mk_sfun i SLens (fun i s2 -> 
        Lns(i,L.const i r1 s1 s2)))))

  ; (mk_native_prelude_qid "concat",
    SLens ^> SLens ^> SLens,
     mk_lfun (Info.M "concat built-in") (SLens ^> SLens) (fun i l1 -> 
       mk_lfun i SLens (fun i l2 ->         
         Lns(i,L.concat i l1 l2))))

  ; (mk_native_prelude_qid "union",
    SLens ^> SLens ^> SLens,
     mk_lfun (Info.M "union built-in") (SLens ^> SLens) (fun i l1 -> 
       mk_lfun i SLens (fun i l2 ->         
         Lns(i,L.union i l1 l2))))

  ; (mk_native_prelude_qid "star",
    SLens ^> SLens,
     mk_lfun (Info.M "star built-in") SLens (fun i l1 ->         
       Lns(i,L.star i l1)))

  (* extensions *)  
  ; (mk_native_prelude_qid "swap",
    SLens ^> SLens ^> SLens,
    mk_lfun (Info.M "swap built-in") (SLens ^> SLens) (fun i l1 -> 
      mk_lfun i SLens (fun i l2 ->         
        Lns(i,L.swap i l1 l2))))

  ; (mk_native_prelude_qid "compose",
    SLens ^> SLens ^> SLens,
    mk_lfun (Info.M "compose built-in") (SLens ^> SLens) (fun i l1 -> 
      mk_lfun i SLens (fun i l2 ->         
        Lns(i,L.compose i l1 l2))))

  ; (mk_native_prelude_qid "default",
     SLens ^> SString ^> SLens,
    mk_lfun (Info.M "default built-in") (SString ^> SLens) (fun i cl1 -> 
      mk_sfun i SLens (fun i def -> 
        Lns(i,L.default i def cl1))))

  ; (mk_native_prelude_qid "key",
     SRegexp ^> SLens,
     mk_rfun (Info.M "key built-in") SLens (fun i r -> 
       Lns(i,L.key i r)))

  ; (mk_native_prelude_qid "duplicate",
    SLens ^> SLens ^> SLens ^> SLens,
    mk_lfun (Info.M "duplicate built-in") (SLens ^> SLens ^> SLens) (fun i l1 -> 
      mk_lfun i (SLens ^> SLens) (fun i l2 ->
        mk_lfun i SLens (fun i l3 -> 
          Lns(i,L.duplicate i true l1 l2 l3)))))

  ; (mk_native_prelude_qid "duplicate_snd",
    SLens ^> SLens ^> SLens ^> SLens,
    mk_lfun (Info.M "duplicate_snd built-in") (SLens ^> SLens ^> SLens) (fun i l1 -> 
      mk_lfun i (SLens ^> SLens) (fun i l2 ->
        mk_lfun i SLens (fun i l3 -> 
          Lns(i,L.duplicate i false l1 l2 l3)))))

  ; (mk_native_prelude_qid "count",
     SRegexp ^> SLens,
     mk_rfun (Info.M "count built-in") SLens (fun i r -> 
       Lns(i,L.count i r)))

  ; (mk_native_prelude_qid "forgetkey",
     SLens ^> SLens,
     mk_lfun (Info.M "forgetkey built-in") SLens (fun i cl ->
       Lns(i, L.forgetkey cl)))

  ; (mk_native_prelude_qid "filter",
     SRegexp ^> SRegexp ^> SLens,
     mk_rfun(Info.M "filter built-in") (SRegexp ^> SLens) (fun i r1 ->
       mk_rfun i SLens (fun i r2 ->
         Lns(i,L.filter i r1 r2))))

  (* canonizer operations *)
  ; (mk_native_prelude_qid "cls",
     SCanonizer ^> SString ^> SString,
     mk_cfun (Info.M "cls built-in") (SString ^> SString) (fun i c1 -> 
       mk_sfun i SString (fun i s1 -> 
         Str(i,C.cls c1 s1))))                          

  ; (mk_native_prelude_qid "rep",
     SCanonizer ^> SString ^> SString,
     mk_cfun (Info.M "rep built-in") (SString ^> SString) (fun i c1 -> 
       mk_sfun i SString (fun i s1 -> 
         Str(i,C.rep c1 s1))))                          

  ; (mk_native_prelude_qid "left_quot",
     SCanonizer ^> SLens ^> SLens,
     mk_cfun (Info.M "left_quot built-in") (SLens ^> SLens) (fun i c1 ->                                              
       mk_lfun i SLens (fun i l1 -> 
         Lns(i,L.left_quot i c1 l1))))

  ; (mk_native_prelude_qid "right_quot",
     SLens ^> SCanonizer ^> SLens,
     mk_lfun (Info.M "right_quot built-in") (SCanonizer ^> SLens) (fun i l1 ->                                              
       mk_cfun i SLens (fun i c1 -> 
         Lns(i,L.right_quot i l1 c1))))

  ; (mk_native_prelude_qid "columnize",
     SString ^> SRegexp ^> SString ^> SString ^> SCanonizer,
     mk_sfun (Info.M "columnize built-in") (SRegexp ^> SString ^> SString ^> SCanonizer) (fun i k -> 
       mk_rfun (Info.M "columnize built-in") (SString ^> SString ^> SCanonizer) (fun i r -> 
         mk_sfun i (SString ^> SCanonizer) (fun i s -> 
           mk_sfun i SCanonizer (fun i nl -> 
             Can(i,C.columnize i k r s nl))))))

  (* string operations *)
  ; (mk_native_prelude_qid "append",
    SString ^> SString ^> SString,
     mk_sfun (Info.M "append built-in") (SString ^> SString) (fun i s1 -> 
       mk_sfun i SString (fun i s2 ->         
         Str(i,RS.append s1 s2))))

  ; (mk_native_prelude_qid "read",
     SString ^> SString,
     mk_sfun (Info.M "read built-in") SString (fun i s1 -> 
       Str(i,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  (* regexp operations *)
  ; (mk_native_prelude_qid "str",
    SString ^> SRegexp,
    mk_sfun (Info.M "str built-in") SRegexp (fun i s1 ->
      Rx(i,R.str false s1)))

  ; (mk_native_prelude_qid "empty",
    SRegexp,
      Rx(Info.M "empty",R.empty))

  ; (mk_native_prelude_qid "seq",
    SRegexp ^> SRegexp ^> SRegexp,
      mk_rfun (Info.M "seq built-in") (SRegexp ^> SRegexp) (fun i r1 -> 
        mk_rfun i SRegexp (fun i r2 ->         
          Rx(i,R.seq r1 r2))))

  ; (mk_native_prelude_qid "alt",
    SRegexp ^> SRegexp ^> SRegexp,
      mk_rfun (Info.M "alt built-in") (SRegexp ^> SRegexp) (fun i r1 -> 
        mk_rfun i SRegexp (fun i r2 ->         
          Rx(i,R.alt r1 r2))))

  ; (mk_native_prelude_qid "iter",
    SRegexp ^> SRegexp,
      mk_rfun (Info.M "iter built-in") SRegexp (fun i r1 ->         
         Rx(i,R.star r1)))

  ; (mk_native_prelude_qid "shortest",
     SRegexp ^> SString,
     mk_rfun (Info.M "shortest built-in") SString (fun i r1 -> 
       Str(i,wrap_rep i r1)))

  ; (mk_native_prelude_qid "equiv",
     SRegexp ^> SRegexp ^> SString ,
     mk_rfun(Info.M "equiv built-in") (SRegexp ^> SString) (fun i r1 ->
       mk_rfun i SString (fun i r2 ->
	 Str(i, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))

    (* run-time checking *)
    ; (mk_native_prelude_qid "assert",
      SRegexp ^> SRegexp ^> SLens ^> SLens,
      mk_rfun (Info.M "assert built-in") (SRegexp ^> SLens ^> SLens) (fun i c -> 
        mk_rfun i (SLens ^> SLens) (fun i a -> 
          mk_lfun i (SLens ^> SLens) (fun i l -> 
            Lns(i,L.assert_lens_type i l (Some c) (Some a))))))

    ; (mk_native_prelude_qid "assert_ctype",
      SRegexp ^> SLens ^> SLens,
      mk_rfun (Info.M "assert_ctype built-in") (SLens ^> SLens) (fun i c -> 
        mk_lfun i SLens (fun i l -> 
          Lns(i,L.assert_lens_ctype i l c))))

    ; (mk_native_prelude_qid "assert_atype",
      SRegexp ^> SLens ^> SLens,
      mk_rfun (Info.M "assert_atype built-in") (SLens ^> SLens) (fun i a -> 
        mk_lfun i SLens (fun i l -> 
          Lns(i,L.assert_lens_atype i l a))))

  (* coercions *)
  ; (mk_native_prelude_qid "ctype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "ctype built-in") SRegexp (fun i cl ->
       Rx(i, L.ctype cl)))

  ; (mk_native_prelude_qid "atype",
     SLens ^> SRegexp, 
     mk_lfun (Info.M "atype built-in") SRegexp (fun i cl ->
       Rx(i, L.atype cl)))

  ; (mk_native_prelude_qid "canonizer_of_lens",
     SLens ^> SCanonizer,
     mk_lfun (Info.M "canonizer_of_lens built-in") SCanonizer (fun i l1 -> 
       Can(i,L.canonizer_of_t i l1)))

  ; (mk_native_prelude_qid "string_of_regexp",
     SRegexp ^> SString ,
     mk_rfun(Info.M "string_of_regexp built-in") SString (fun i r1 ->
      Str(i, RS.t_of_string (R.string_of_t r1))))
  ]
      
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v)
    prelude_spec
