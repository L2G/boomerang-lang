(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007-2008                                                     *)
(* J. Nathan Foster, Alexandre Pilkiewicz, and Benjamin C. Pierce              *)
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
(* /boomerang/lenses/prelude.ml                                                *)
(* OCaml definitions of lens primitives                                        *)
(* $Id$ *)
(*******************************************************************************)

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

let poly_bin_error i se1 se2 v1 v2 = 
  Error.simple_error 
    (sprintf "%s: expected %s and %s but found %s and %s"
       (Info.string_of_t i)
       (Bprint.string_of_sort se1) (Bprint.string_of_sort se2)
       (sort_string_of_t v1) (sort_string_of_t v2))

let poly_error i se1 v1 = 
  Error.simple_error 
    (sprintf "%s: expected %s but found %s"
       (Info.string_of_t i)
       (Bprint.string_of_sort se1)
       (sort_string_of_t v1))

let prelude_spec =
  [(* lens operations *)
   (S.Qid.mk_native_prelude_t "get", 
    S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString),
    mk_lfun (Info.M "get built-in") (fun i l1 -> 
      mk_sfun i (fun i s1 -> 
        Str(i,L.get l1 s1))))

  ; (S.Qid.mk_native_prelude_t "put", 
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString ^> S.SString),
     mk_lfun (Info.M "put built-in") (fun i l1 -> 
       mk_sfun i (fun i s1 -> 
         mk_sfun i (fun i s2 -> 
           Str(i,L.rput_of_dl l1 s1 s2)))))

  ; (S.Qid.mk_native_prelude_t "create", 
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString),
     mk_lfun (Info.M "crt built-in") (fun i l1 -> 
       mk_sfun i (fun i s1 -> 
         Str(i,L.rcreate_of_dl l1 s1))))

  ; (S.Qid.mk_native_prelude_t "invert",
     S.scheme_of_sort (S.SLens ^> S.SLens),
     mk_lfun (Info.M "invert built-in") (fun i l1 -> 
       Lns(i,L.invert i l1)))

  (* core lens combinators *)
  ; (S.Qid.mk_native_prelude_t "concat",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "concat built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->         
         Lns(i,L.concat i l1 l2))))

  ; (S.Qid.mk_native_prelude_t "union",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "union built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->         
         Lns(i,L.union i l1 l2))))

  ; (S.Qid.mk_native_prelude_t "dmatch",
     S.scheme_of_sort (S.SString ^> S.SLens ^> S.SLens),
     mk_sfun (Info.M "dmatch built-in") (fun i s1 ->
       mk_lfun i (fun i l1 -> 
         Lns(i,L.dmatch i L.std_lookup s1 l1))))

  ; (S.Qid.mk_native_prelude_t "smatch",
     S.scheme_of_sort (S.SString ^> S.SString ^> S.SLens ^> S.SLens),
     mk_sfun (Info.M "smatch built-in") (fun i s1 ->
       let f = float_of_string (RS.string_of_t s1) in 
       mk_sfun i (fun i s2 ->                                            
         mk_lfun i (fun i l1 -> 
           Lns(i,L.dmatch i (L.sim_lookup f) s2 l1)))))

  ; (S.Qid.mk_native_prelude_t "star",
     S.scheme_of_sort (S.SLens ^> S.SLens),
     mk_lfun (Info.M "star built-in") (fun i l1 ->         
       Lns(i,L.star i l1)))

  (* extensions *)  
  ; (S.Qid.mk_native_prelude_t "swap",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "swap built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->         
         Lns(i,L.swap i l1 l2))))

  ; (S.Qid.mk_native_prelude_t "compose",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "compose built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->         
         Lns(i,L.compose i l1 l2))))

  ; (S.Qid.mk_native_prelude_t "default",
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SLens),
     mk_lfun (Info.M "default built-in") (fun i cl1 -> 
       mk_sfun i (fun i def -> 
         Lns(i,L.default i def cl1))))

  ; (S.Qid.mk_native_prelude_t "key",
     S.scheme_of_sort (S.SRegexp ^> S.SLens),
       mk_rfun (Info.M "key built-in") (fun i r -> 
         Lns(i,L.key i r)))

  ; (S.Qid.mk_native_prelude_t "duplicate",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "duplicate built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->
         mk_lfun i (fun i l3 -> 
           Lns(i,L.duplicate i true l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "duplicate_snd",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     mk_lfun (Info.M "duplicate_snd built-in") (fun i l1 -> 
       mk_lfun i (fun i l2 ->
         mk_lfun i (fun i l3 -> 
           Lns(i,L.duplicate i false l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "count",
     S.scheme_of_sort (S.SRegexp ^> S.SLens),
     mk_rfun (Info.M "count built-in") (fun i r -> 
       Lns(i,L.count i r)))

  ; (S.Qid.mk_native_prelude_t "forgetkey",
     S.scheme_of_sort (S.SLens ^> S.SLens),
     mk_lfun (Info.M "forgetkey built-in") (fun i cl ->
       Lns(i, L.forgetkey cl)))

  ; (S.Qid.mk_native_prelude_t "filter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SLens),
     mk_rfun(Info.M "filter built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
         Lns(i,L.filter i r1 r2))))

  (* canonizer operations *)
  ; (S.Qid.mk_native_prelude_t "cls",
     S.scheme_of_sort (S.SCanonizer ^> S.SString ^> S.SString),
     mk_cfun (Info.M "cls built-in") (fun i c1 -> 
       mk_sfun i (fun i s1 -> 
         Str(i,C.cls c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "rep",
     S.scheme_of_sort (S.SCanonizer ^> S.SString ^> S.SString),
     mk_cfun (Info.M "rep built-in") (fun i c1 -> 
       mk_sfun i (fun i s1 -> 
         Str(i,C.rep c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "left_quot",
     S.scheme_of_sort (S.SCanonizer ^> S.SLens ^> S.SLens),
     mk_cfun (Info.M "left_quot built-in") (fun i c1 ->                                              
       mk_lfun i (fun i l1 -> 
         Lns(i,L.left_quot i c1 l1))))

  ; (S.Qid.mk_native_prelude_t "right_quot",
     S.scheme_of_sort (S.SLens ^> S.SCanonizer ^> S.SLens),
     mk_lfun (Info.M "right_quot built-in") (fun i l1 ->                                              
       mk_cfun i (fun i c1 -> 
         Lns(i,L.right_quot i l1 c1))))

  ; (S.Qid.mk_native_prelude_t "columnize",
     S.scheme_of_sort (S.SString ^> S.SRegexp ^> S.SString ^> S.SString ^> S.SCanonizer),
     mk_sfun (Info.M "columnize built-in") (fun i k -> 
       mk_rfun i (fun i r -> 
         mk_sfun i (fun i s -> 
           mk_sfun i (fun i nl -> 
             Can(i,C.columnize i k r s nl))))))

  (* string operations *)
  ; (S.Qid.mk_native_prelude_t "append",
     S.scheme_of_sort (S.SString ^> S.SString ^> S.SString),
     mk_sfun (Info.M "append built-in") (fun i s1 -> 
       mk_sfun i (fun i s2 ->         
         Str(i,RS.append s1 s2))))

  ; (S.Qid.mk_native_prelude_t "read",
     S.scheme_of_sort (S.SString ^> S.SString),
     mk_sfun (Info.M "read built-in") (fun i s1 -> 
       Str(i,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  (* regexp operations *)
  ; (S.Qid.mk_native_prelude_t "str",
     S.scheme_of_sort (S.SString ^> S.SRegexp),
     mk_sfun (Info.M "str built-in") (fun i s1 ->
       Rx(i,R.str false s1)))

  ; (S.Qid.mk_native_prelude_t "empty",
     S.scheme_of_sort (S.SRegexp),
       Rx(Info.M "empty",R.empty))

  ; (S.Qid.mk_native_prelude_t "seq",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     mk_rfun (Info.M "seq built-in") (fun i r1 -> 
       mk_rfun i (fun i r2 ->         
         Rx(i,R.seq r1 r2))))

  ; (S.Qid.mk_native_prelude_t "alt",
    S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
      mk_rfun (Info.M "alt built-in") (fun i r1 -> 
        mk_rfun i (fun i r2 ->         
          Rx(i,R.alt r1 r2))))

  ; (S.Qid.mk_native_prelude_t "diff",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     mk_rfun (Info.M "diff built-in") (fun i r1 -> 
       mk_rfun i (fun i r2 ->         
         Rx(i,R.diff r1 r2))))

  ; (S.Qid.mk_native_prelude_t "inter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     mk_rfun (Info.M "inter built-in") (fun i r1 -> 
       mk_rfun i (fun i r2 ->         
         Rx(i,R.inter r1 r2))))

  ; (S.Qid.mk_native_prelude_t "iter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp),
     mk_rfun (Info.M "iter built-in") (fun i r1 ->         
       Rx(i,R.star r1)))

  ; (S.Qid.mk_native_prelude_t "equiv",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SString),
     mk_rfun(Info.M "equiv built-in") (fun i r1 ->
       mk_rfun i (fun i r2 ->
	 Str(i, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))
    
  (* run-time checking *)
  ; (S.Qid.mk_native_prelude_t "assert",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SLens ^> S.SLens),
     mk_rfun (Info.M "assert built-in") (fun i c -> 
       mk_rfun i (fun i a -> 
         mk_lfun i (fun i l -> 
           Lns(i,L.assert_lens_type i l (Some c) (Some a))))))

  ; (S.Qid.mk_native_prelude_t "assert_ctype",
     S.scheme_of_sort (S.SRegexp ^> S.SLens ^> S.SLens),
     mk_rfun (Info.M "assert_ctype built-in") (fun i c -> 
       mk_lfun i (fun i l -> 
         Lns(i,L.assert_lens_ctype i l c))))

  ; (S.Qid.mk_native_prelude_t "assert_atype",
     S.scheme_of_sort (S.SRegexp ^> S.SLens ^> S.SLens),
     mk_rfun (Info.M "assert_atype built-in") (fun i a -> 
       mk_lfun i (fun i l -> 
         Lns(i,L.assert_lens_atype i l a))))

  (* coercions *)
  ; (S.Qid.mk_native_prelude_t "ctype",
     S.scheme_of_sort (S.SLens ^> S.SRegexp),
     mk_lfun (Info.M "ctype built-in") (fun i cl ->
       Rx(i, L.ctype cl)))

  ; (S.Qid.mk_native_prelude_t "atype",
     S.scheme_of_sort (S.SLens ^> S.SRegexp),
     mk_lfun (Info.M "atype built-in") (fun i cl ->
       Rx(i, L.atype cl)))

  ; (S.Qid.mk_native_prelude_t "canonizer_of_lens",
     S.scheme_of_sort (S.SLens ^> S.SCanonizer),
     mk_lfun (Info.M "canonizer_of_lens built-in") (fun i l1 -> 
       Can(i,L.canonizer_of_t i l1)))

  ; (S.Qid.mk_native_prelude_t "string_of_regexp",
     S.scheme_of_sort (S.SRegexp ^> S.SString),
     mk_rfun (Info.M "string_of_regexp built-in") (fun i r1 ->
      Str(i, RS.t_of_string (R.string_of_t r1))))

  (* polymorphic operators *)
  ; begin
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "shortest",
     S.mk_scheme [alpha] (a ^> S.SString),
     mk_poly_fun (Info.M "shortest built-in") (fun i v1 -> 
       match v1 with 
         | Str _ -> Str(i,get_s v1 i)
         | Rx _  -> Str(i,wrap_rep i (get_r v1 i))
         | _     -> poly_error i a v1))
  end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "const",
     S.mk_scheme [alpha] (a ^> S.SString ^> S.SString ^> S.SLens),
     mk_poly_fun (Info.M "const built-in") (fun i v1 -> 
       mk_sfun i (fun i s1 -> 
         mk_sfun i (fun i s2 -> 
           match v1 with 
             | Str _ -> 
                 let r = R.str false (get_s v1 i) in                  
                 Lns(i,L.const i r s1 s2)
             | Rx _  -> Lns(i,L.const i (get_r v1 i) s1 s2)
             | _     -> poly_error i a v1))))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "copy",
     S.mk_scheme [alpha] (a ^> S.SLens),
     mk_poly_fun (Info.M "copy built-in") (fun i v1 -> 
         match v1 with
           | Str _ -> Lns(i,L.copy i (R.str false (get_s v1 i)))
           | Rx _  -> Lns(i,L.copy i (get_r v1 i))
           | _     -> poly_error i a v1))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Str;S.Reg;S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_concat",
     S.mk_scheme [alpha] (a ^> a ^> a),
     mk_poly_fun (Info.M "poly_concat built-in") (fun i v1 -> 
       mk_poly_fun i (fun i v2 ->
         match v1,v2 with
           | Str _,Str _  -> Str(i,RS.append (get_s v1 i) (get_s v2 i))
           | Rx _, Rx _   -> Rx(i,R.seq (get_r v1 i) (get_r v2 i))
           | Lns _, Lns _ -> Lns(i,L.concat i (get_l v1 i) (get_l v2 i))
           | Can _, Can _ -> Can(i,C.concat i (get_c v1 i) (get_c v2 i))
           | _            -> poly_bin_error i a a v1 v2)))
    end

  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Str;S.Reg;S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_union",
     S.mk_scheme [alpha] (a ^> a ^> a),
     mk_poly_fun (Info.M "poly_union built-in") (fun i v1 -> 
       mk_poly_fun i (fun i v2 ->     
         match v1,v2 with 
           | Rx _, Rx _  -> Rx(i,R.alt (get_r v1 i) (get_r v2 i))
           | Lns _, Lns _ -> Lns(i,L.union i (get_l v1 i) (get_l v2 i))
           | Can _, Can _ -> Can(i,C.union i (get_c v1 i) (get_c v2 i))
           | _            -> poly_bin_error i a a v1 v2)))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Reg;S.Lns;S.Can])) in 
    let a = S.SVar alpha in 
    let get_int i s = 
      try int_of_string (RS.string_of_t s) 
      with _ -> raise
        (Error.Harmony_error
           (fun () -> 
              Util.format "%s: expected string representing an integer, found %s."
                (Info.string_of_t i) (RS.string_of_t s))) in     
    (S.Qid.mk_native_prelude_t "poly_iter",
     S.mk_scheme [alpha] (a ^> S.SString ^> S.SString ^> a),
     mk_poly_fun (Info.M "poly_iter built-in") (fun i v1 -> 
       mk_sfun i (fun i s1 -> 
         mk_sfun i (fun i s2 -> 
           let min = get_int i s1 in
           let maxo = if RS.length s2 = 0 then None else Some(get_int i s2) in 
             match v1 with 
               | Rx _  -> Rx(i,R.iter (get_r v1 i) min maxo)
               | Lns _ -> Lns(i,L.iter i (get_l v1 i) min maxo)
               | Can _ -> Can(i,C.iter i (get_c v1 i) min maxo)
               | _            -> poly_error i a v1))))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (S.sbset_of_sl [S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_swap",
     S.mk_scheme [alpha] (a ^> a ^> a),
     mk_poly_fun (Info.M "poly_swap built-in") (fun i v1 -> 
       mk_poly_fun i (fun i v2 ->     
         match v1,v2 with 
           | Lns _, Lns _ -> Lns(i,L.swap i (get_l v1 i) (get_l v2 i))
           | _            -> poly_bin_error i a a v1 v2)))
    end
  ; begin 
    let nil = S.Qid.mk_list_t "Nil" in 
    let cons = S.Qid.mk_list_t "Cons" in 
    let alpha = Bunify.fresh_svar S.Fre in 
    let beta = Bunify.fresh_svar S.Fre in 
    let a = S.SVar alpha in 
    let b = S.SVar beta in 
    let list_b = S.SData([b],S.Qid.mk_list_t "t") in 
    (S.Qid.mk_native_prelude_t "fold_left",
     S.mk_scheme [alpha] ((a ^> b ^> a) ^> a ^> list_b ^> a),
     mk_poly_fun (Info.M "fold_left built-in") (fun i f0 -> 
       mk_poly_fun i (fun i acc0 -> 
         mk_poly_fun i (fun i l0 ->                                                   
           let f = get_f f0 i in 
           let rec aux l acc = 
             let lbl,vo = get_v l i in 
             if S.Qid.equal lbl nil then acc
             else if S.Qid.equal lbl cons then 
               let hd,tl = match vo with 
                 | None -> poly_error i list_b l
                 | Some v -> get_p v i in                
               let acc' = (get_f (f i hd) i) i acc in 
               aux tl acc'
             else poly_error i list_b l in 
           aux l0 acc0))))
  end ]  
    
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v) 
    prelude_spec
