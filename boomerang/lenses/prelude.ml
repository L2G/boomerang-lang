open Bvalue 
module S = Bsyntax
module L = Blenses.DLens
module R = Bregexp
module RS = Bstring
let (^) = Pervasives.(^)
let (^>) = S.(^>)

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
   (S.mk_native_prelude_qid "get", 
    S.SLens ^> S.SString ^> S.SString,
    mk_lfun (Info.M "get built-in") (S.SString ^> S.SString) (fun i l1 -> 
      mk_sfun i S.SString (fun i s1 -> 
        Str(i,L.get l1 s1))))

  ; (S.mk_native_prelude_qid "put", 
    S.SLens ^> S.SString ^> S.SString ^> S.SString,
    mk_lfun (Info.M "put built-in") (S.SString ^> S.SString ^> S.SString) (fun i l1 -> 
      mk_sfun i (S.SString ^> S.SString) (fun i s1 -> 
        mk_sfun i S.SString (fun i s2 -> 
          Str(i,L.rput_of_dl l1 s1 s2)))))

  ; (S.mk_native_prelude_qid "create", 
    S.SLens ^> S.SString ^> S.SString,
    mk_lfun (Info.M "crt built-in") (S.SString ^> S.SString) (fun i l1 -> 
      mk_sfun i S.SString (fun i s1 -> 
        Str(i,L.rcreate_of_dl l1 s1))))

  ; (S.mk_native_prelude_qid "invert",
     S.SLens ^> S.SLens,
     mk_lfun (Info.M "invert built-in") S.SLens (fun i l1 -> 
       Lns(i,L.invert i l1)))

  (* core lens combinators *)
  ; (S.mk_native_prelude_qid "copy",
    S.SRegexp ^> S.SLens,
    mk_rfun (Info.M "copy built-in") S.SLens (fun i r1 -> 
      Lns(i,L.copy i r1)))

  ; (S.mk_native_prelude_qid "const",
    S.SRegexp ^> S.SString ^> S.SString ^> S.SLens,
    mk_rfun (Info.M "const built-in") (S.SString ^> S.SString ^> S.SLens) (fun i r1 -> 
      mk_sfun i (S.SString ^> S.SLens) (fun i s1 -> 
        mk_sfun i S.SLens (fun i s2 -> 
        Lns(i,L.const i r1 s1 s2)))))

  ; (S.mk_native_prelude_qid "concat",
     S.SLens ^> S.SLens ^> S.SLens,
     mk_lfun (Info.M "concat built-in") (S.SLens ^> S.SLens) (fun i l1 -> 
       mk_lfun i S.SLens (fun i l2 ->         
         Lns(i,L.concat i l1 l2))))

  ; (S.mk_native_prelude_qid "union",
    S.SLens ^> S.SLens ^> S.SLens,
     mk_lfun (Info.M "union built-in") (S.SLens ^> S.SLens) (fun i l1 -> 
       mk_lfun i S.SLens (fun i l2 ->         
         Lns(i,L.union i l1 l2))))

  ; (S.mk_native_prelude_qid "dmatch",
    S.SString ^> S.SLens ^> S.SLens,
     mk_sfun (Info.M "dmatch built-in") (S.SLens ^> S.SLens) (fun i s1 ->
       mk_lfun i S.SLens (fun i l1 -> 
         Lns(i,L.smatch i s1 l1))))

  ; (S.mk_native_prelude_qid "star",
    S.SLens ^> S.SLens,
     mk_lfun (Info.M "star built-in") S.SLens (fun i l1 ->         
       Lns(i,L.star i l1)))

  (* extensions *)  
  ; (S.mk_native_prelude_qid "swap",
    S.SLens ^> S.SLens ^> S.SLens,
    mk_lfun (Info.M "swap built-in") (S.SLens ^> S.SLens) (fun i l1 -> 
      mk_lfun i S.SLens (fun i l2 ->         
        Lns(i,L.swap i l1 l2))))

  ; (S.mk_native_prelude_qid "compose",
    S.SLens ^> S.SLens ^> S.SLens,
    mk_lfun (Info.M "compose built-in") (S.SLens ^> S.SLens) (fun i l1 -> 
      mk_lfun i S.SLens (fun i l2 ->         
        Lns(i,L.compose i l1 l2))))

  ; (S.mk_native_prelude_qid "default",
     S.SLens ^> S.SString ^> S.SLens,
    mk_lfun (Info.M "default built-in") (S.SString ^> S.SLens) (fun i cl1 -> 
      mk_sfun i S.SLens (fun i def -> 
        Lns(i,L.default i def cl1))))

  ; (S.mk_native_prelude_qid "key",
     S.SRegexp ^> S.SLens,
     mk_rfun (Info.M "key built-in") S.SLens (fun i r -> 
       Lns(i,L.key i r)))

  ; (S.mk_native_prelude_qid "duplicate",
    S.SLens ^> S.SLens ^> S.SLens ^> S.SLens,
    mk_lfun (Info.M "duplicate built-in") (S.SLens ^> S.SLens ^> S.SLens) (fun i l1 -> 
      mk_lfun i (S.SLens ^> S.SLens) (fun i l2 ->
        mk_lfun i S.SLens (fun i l3 -> 
          Lns(i,L.duplicate i true l1 l2 l3)))))

  ; (S.mk_native_prelude_qid "duplicate_snd",
    S.SLens ^> S.SLens ^> S.SLens ^> S.SLens,
    mk_lfun (Info.M "duplicate_snd built-in") (S.SLens ^> S.SLens ^> S.SLens) (fun i l1 -> 
      mk_lfun i (S.SLens ^> S.SLens) (fun i l2 ->
        mk_lfun i S.SLens (fun i l3 -> 
          Lns(i,L.duplicate i false l1 l2 l3)))))

  ; (S.mk_native_prelude_qid "count",
     S.SRegexp ^> S.SLens,
     mk_rfun (Info.M "count built-in") S.SLens (fun i r -> 
       Lns(i,L.count i r)))

  ; (S.mk_native_prelude_qid "forgetkey",
     S.SLens ^> S.SLens,
     mk_lfun (Info.M "forgetkey built-in") S.SLens (fun i cl ->
       Lns(i, L.forgetkey cl)))

  ; (S.mk_native_prelude_qid "filter",
     S.SRegexp ^> S.SRegexp ^> S.SLens,
     mk_rfun(Info.M "filter built-in") (S.SRegexp ^> S.SLens) (fun i r1 ->
       mk_rfun i S.SLens (fun i r2 ->
         Lns(i,L.filter i r1 r2))))

  (* canonizer operations *)
  ; (S.mk_native_prelude_qid "cls",
     S.SCanonizer ^> S.SString ^> S.SString,
     mk_cfun (Info.M "cls built-in") (S.SString ^> S.SString) (fun i c1 -> 
       mk_sfun i S.SString (fun i s1 -> 
         Str(i,C.cls c1 s1))))                          

  ; (S.mk_native_prelude_qid "rep",
     S.SCanonizer ^> S.SString ^> S.SString,
     mk_cfun (Info.M "rep built-in") (S.SString ^> S.SString) (fun i c1 -> 
       mk_sfun i S.SString (fun i s1 -> 
         Str(i,C.rep c1 s1))))                          

  ; (S.mk_native_prelude_qid "left_quot",
     S.SCanonizer ^> S.SLens ^> S.SLens,
     mk_cfun (Info.M "left_quot built-in") (S.SLens ^> S.SLens) (fun i c1 ->                                              
       mk_lfun i S.SLens (fun i l1 -> 
         Lns(i,L.left_quot i c1 l1))))

  ; (S.mk_native_prelude_qid "right_quot",
     S.SLens ^> S.SCanonizer ^> S.SLens,
     mk_lfun (Info.M "right_quot built-in") (S.SCanonizer ^> S.SLens) (fun i l1 ->                                              
       mk_cfun i S.SLens (fun i c1 -> 
         Lns(i,L.right_quot i l1 c1))))

  ; (S.mk_native_prelude_qid "columnize",
     S.SString ^> S.SRegexp ^> S.SString ^> S.SString ^> S.SCanonizer,
     mk_sfun (Info.M "columnize built-in") (S.SRegexp ^> S.SString ^> S.SString ^> S.SCanonizer) (fun i k -> 
       mk_rfun (Info.M "columnize built-in") (S.SString ^> S.SString ^> S.SCanonizer) (fun i r -> 
         mk_sfun i (S.SString ^> S.SCanonizer) (fun i s -> 
           mk_sfun i S.SCanonizer (fun i nl -> 
             Can(i,C.columnize i k r s nl))))))

  (* string operations *)
  ; (S.mk_native_prelude_qid "append",
    S.SString ^> S.SString ^> S.SString,
     mk_sfun (Info.M "append built-in") (S.SString ^> S.SString) (fun i s1 -> 
       mk_sfun i S.SString (fun i s2 ->         
         Str(i,RS.append s1 s2))))

  ; (S.mk_native_prelude_qid "read",
     S.SString ^> S.SString,
     mk_sfun (Info.M "read built-in") S.SString (fun i s1 -> 
       Str(i,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  (* regexp operations *)
  ; (S.mk_native_prelude_qid "str",
    S.SString ^> S.SRegexp,
    mk_sfun (Info.M "str built-in") S.SRegexp (fun i s1 ->
      Rx(i,R.str false s1)))

  ; (S.mk_native_prelude_qid "empty",
    S.SRegexp,
      Rx(Info.M "empty",R.empty))

  ; (S.mk_native_prelude_qid "seq",
    S.SRegexp ^> S.SRegexp ^> S.SRegexp,
      mk_rfun (Info.M "seq built-in") (S.SRegexp ^> S.SRegexp) (fun i r1 -> 
        mk_rfun i S.SRegexp (fun i r2 ->         
          Rx(i,R.seq r1 r2))))

  ; (S.mk_native_prelude_qid "alt",
    S.SRegexp ^> S.SRegexp ^> S.SRegexp,
      mk_rfun (Info.M "alt built-in") (S.SRegexp ^> S.SRegexp) (fun i r1 -> 
        mk_rfun i S.SRegexp (fun i r2 ->         
          Rx(i,R.alt r1 r2))))

  ; (S.mk_native_prelude_qid "diff",
    S.SRegexp ^> S.SRegexp ^> S.SRegexp,
      mk_rfun (Info.M "diff built-in") (S.SRegexp ^> S.SRegexp) (fun i r1 -> 
        mk_rfun i S.SRegexp (fun i r2 ->         
          Rx(i,R.diff r1 r2))))

  ; (S.mk_native_prelude_qid "inter",
    S.SRegexp ^> S.SRegexp ^> S.SRegexp,
      mk_rfun (Info.M "inter built-in") (S.SRegexp ^> S.SRegexp) (fun i r1 -> 
        mk_rfun i S.SRegexp (fun i r2 ->         
          Rx(i,R.inter r1 r2))))

  ; (S.mk_native_prelude_qid "iter",
    S.SRegexp ^> S.SRegexp,
      mk_rfun (Info.M "iter built-in") S.SRegexp (fun i r1 ->         
         Rx(i,R.star r1)))

  ; (S.mk_native_prelude_qid "shortest",
     S.SRegexp ^> S.SString,
     mk_rfun (Info.M "shortest built-in") S.SString (fun i r1 -> 
       Str(i,wrap_rep i r1)))

  ; (S.mk_native_prelude_qid "equiv",
     S.SRegexp ^> S.SRegexp ^> S.SString ,
     mk_rfun(Info.M "equiv built-in") (S.SRegexp ^> S.SString) (fun i r1 ->
       mk_rfun i S.SString (fun i r2 ->
	 Str(i, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))

    (* run-time checking *)
    ; (S.mk_native_prelude_qid "assert",
      S.SRegexp ^> S.SRegexp ^> S.SLens ^> S.SLens,
      mk_rfun (Info.M "assert built-in") (S.SRegexp ^> S.SLens ^> S.SLens) (fun i c -> 
        mk_rfun i (S.SLens ^> S.SLens) (fun i a -> 
          mk_lfun i (S.SLens ^> S.SLens) (fun i l -> 
            Lns(i,L.assert_lens_type i l (Some c) (Some a))))))

    ; (S.mk_native_prelude_qid "assert_ctype",
      S.SRegexp ^> S.SLens ^> S.SLens,
      mk_rfun (Info.M "assert_ctype built-in") (S.SLens ^> S.SLens) (fun i c -> 
        mk_lfun i S.SLens (fun i l -> 
          Lns(i,L.assert_lens_ctype i l c))))

    ; (S.mk_native_prelude_qid "assert_atype",
      S.SRegexp ^> S.SLens ^> S.SLens,
      mk_rfun (Info.M "assert_atype built-in") (S.SLens ^> S.SLens) (fun i a -> 
        mk_lfun i S.SLens (fun i l -> 
          Lns(i,L.assert_lens_atype i l a))))

  (* coercions *)
  ; (S.mk_native_prelude_qid "ctype",
     S.SLens ^> S.SRegexp, 
     mk_lfun (Info.M "ctype built-in") S.SRegexp (fun i cl ->
       Rx(i, L.ctype cl)))

  ; (S.mk_native_prelude_qid "atype",
     S.SLens ^> S.SRegexp, 
     mk_lfun (Info.M "atype built-in") S.SRegexp (fun i cl ->
       Rx(i, L.atype cl)))

  ; (S.mk_native_prelude_qid "canonizer_of_lens",
     S.SLens ^> S.SCanonizer,
     mk_lfun (Info.M "canonizer_of_lens built-in") S.SCanonizer (fun i l1 -> 
       Can(i,L.canonizer_of_t i l1)))

  ; (S.mk_native_prelude_qid "string_of_regexp",
     S.SRegexp ^> S.SString ,
     mk_rfun(Info.M "string_of_regexp built-in") S.SString (fun i r1 ->
      Str(i, RS.t_of_string (R.string_of_t r1))))
  ]
      
let () = 
  (* polymorphic operators *)
  let alpha = S.fresh_svar (S.Con S.Str) in 
  let a = S.SVar alpha in 
  let q = S.mk_native_prelude_qid "poly_concat" in 
  let s = S.SVSet.singleton alpha, S.SFunction(a,S.SFunction(a,a)) in 
  let i = Info.M "poly_concat built-in" in
  let f = 
    Fun(i,a,S.SFunction(a,a),
        (fun i v1 -> 
           Fun(i,a,a,
               (fun i v2 -> 
                  match v1,v2 with 
                    | Str _,Str _ -> Str(i,RS.append (get_s v1 i) (get_s v2 i))
                    | Rx _, Rx _  -> Rx(i,R.seq (get_r v1 i) (get_r v2 i))
                    | Lns _, Lns _ -> Lns(i,L.concat i (get_l v1 i) (get_l v2 i))
                    | _            -> Can(i,C.concat i (get_c v1 i) (get_c v2 i)))))) in 
  Bregistry.register_native_qid q s f;
  let alpha = S.fresh_svar (S.Con S.Reg) in 
  let a = S.SVar alpha in 
  let q = S.mk_native_prelude_qid "poly_union" in 
  let s = S.SVSet.singleton alpha, S.SFunction(a,S.SFunction(a,a)) in 
  let i = Info.M "poly_union built-in" in
  let f = 
    Fun(i,a,S.SFunction(a,a),
        (fun i v1 -> 
           Fun(i,a,a,
               (fun i v2 -> 
                  match v1,v2 with 
                    | Rx _, Rx _  -> Rx(i,R.alt (get_r v1 i) (get_r v2 i))
                    | Lns _, Lns _ -> Lns(i,L.union i (get_l v1 i) (get_l v2 i))
                    | _            -> Can(i,C.union i (get_c v1 i) (get_c v2 i)))))) in 
  Bregistry.register_native_qid q s f;
  (* iter *)
  let alpha = S.fresh_svar (S.Con S.Reg) in 
  let a = S.SVar alpha in 
  let q = S.mk_native_prelude_qid "poly_iter" in 
  let s = (S.SVSet.singleton alpha, a ^> (S.SString ^> (S.SString ^> a))) in
  let i = Info.M "poly_star built-in" in
  let get_int s = 
    try int_of_string (RS.string_of_t s) 
    with _ -> raise
      (Error.Harmony_error
         (fun () -> 
            Util.format "%s: expected string representing an integer, found %s."
              (Info.string_of_t i) (RS.string_of_t s))) in     
  let f = 
    Fun(i,a,S.SString ^> (S.SString ^> a), (fun i v1 -> 
      mk_sfun i (S.SString ^> a) (fun i s1 -> 
        mk_sfun i a (fun i s2 -> 
        let min = get_int s1 in
        let maxo = if RS.length s2 = 0 then None else Some(get_int s2) in 
          match v1 with 
            | Rx _  -> Rx(i,R.iter (get_r v1 i) min maxo)
            | Lns _ -> Lns(i,L.iter i (get_l v1 i) min maxo)
            | _     -> Can(i,C.iter i (get_c v1 i) min maxo))))) in 
    Bregistry.register_native_qid q s f;
  (* swap *)
  let alpha = S.fresh_svar (S.Con S.Lns) in 
  let a = S.SVar alpha in 
  let q = S.mk_native_prelude_qid "poly_swap" in 
  let s = S.SVSet.singleton alpha, S.SFunction(a,S.SFunction(a,a)) in 
  let i = Info.M "poly_swap built-in" in
  let f = 
    Fun(i,a,S.SFunction(a,a),
        (fun i v1 -> 
           Fun(i,a,a,
               (fun i v2 -> Lns(i,L.swap i (get_l v1 i) (get_l v2 i)))))) in 
  Bregistry.register_native_qid q s f;


    Safelist.iter 
      (fun (x,s,v) -> Bregistry.register_native_qid x (S.scheme_of_sort s) v)
      prelude_spec
