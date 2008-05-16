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
module C = Blenses.Canonizer
module R = Bregexp
module RS = Bstring
let (^) = Pervasives.(^)
let sprintf = Printf.sprintf 
let (^>) = S.(^>)

let wrap_rep i r = 
  try 
    R.rep r 
  with Not_found -> 
    raise (Error.Harmony_error(fun () -> 
      Util.format "%s: cannot calculate representative; %s is empty."
        (Info.string_of_t i)
        (R.string_of_t r)))

let poly_error i se1 v1 = 
  Error.simple_error 
    (sprintf "%s: expected %s but found %s"
       (Info.string_of_t i)
       (Bprint.string_of_sort se1)
       (sort_string_of_t v1))

let mk_prelude_info s = Info.M (sprintf "%s built-in" s)

let prelude_spec =
  [(* lens operations *)
   (S.Qid.mk_native_prelude_t "get", 
     (S.SLens ^> S.SString ^> S.SString),
    let b = mk_prelude_info "get" in 
    mk_lfun b (fun l1 -> 
      mk_sfun b (fun s1 -> 
        Str(b,L.get l1 s1))))

  ; (S.Qid.mk_native_prelude_t "put", 
      (S.SLens ^> S.SString ^> S.SString ^> S.SString),
     let b = mk_prelude_info "put" in 
     mk_lfun b (fun l1 -> 
       mk_sfun b (fun s1 -> 
         mk_sfun b (fun s2 -> 
           Str(b,L.rput_of_dl l1 s1 s2)))))

  ; (S.Qid.mk_native_prelude_t "create", 
      (S.SLens ^> S.SString ^> S.SString),
     let b = mk_prelude_info "create" in 
     mk_lfun b (fun l1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,L.rcreate_of_dl l1 s1))))

  ; (S.Qid.mk_native_prelude_t "invert",
      (S.SLens ^> S.SLens), 
     let b = mk_prelude_info "invert" in 
     mk_lfun b (fun l1 -> 
      Lns(b,L.invert b l1)))

  (* core lens combinators *)
  ; (S.Qid.mk_native_prelude_t "lens_union",
      (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "lens_union" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 -> 
         Lns(b,L.union b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "lens_concat",
      (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "lens_concat" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.concat b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "lens_iter",
      (S.SLens ^> S.SInteger ^> S.SInteger ^> S.SLens),
     let b = mk_prelude_info "lens_iter" in 
     mk_lfun b (fun l1 ->
       mk_ifun b (fun i1 -> 
         mk_ifun b (fun i2 -> 
           Lns(b,L.iter b l1 i1 i2)))))

  ; (S.Qid.mk_native_prelude_t "lens_swap",
      (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "lens_swap" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.swap b l1 l2))))

  (* other lens operators *)
  ; (S.Qid.mk_native_prelude_t "copy",
      (S.SRegexp ^> S.SLens),
     let b = mk_prelude_info "copy" in 
     mk_rfun b (fun r1 -> 
       Lns(b,L.copy b r1)))

  ; (S.Qid.mk_native_prelude_t "const",
      (S.SRegexp ^> S.SString ^> S.SString ^> S.SLens),
     let b = mk_prelude_info "const" in 
     mk_rfun b (fun r1 -> 
       mk_sfun b (fun s1 -> 
         mk_sfun b (fun s2 -> 
           Lns(b,L.const b r1 s1 s2)))))

  ; (S.Qid.mk_native_prelude_t "dmatch",
      (S.SString ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "dmatch" in 
     mk_sfun b (fun s1 ->
       mk_lfun b (fun l1 -> 
         Lns(b,L.dmatch b L.std_lookup s1 l1))))

  ; (S.Qid.mk_native_prelude_t "smatch",
      (S.SString ^> S.SString ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "smatch" in 
     mk_sfun b (fun s1 ->
       let f = float_of_string (RS.string_of_t s1) in 
       mk_sfun b (fun s2 ->                                            
         mk_lfun b (fun l1 -> 
           Lns(b,L.dmatch b (L.sim_lookup f) s2 l1)))))

  ; (S.Qid.mk_native_prelude_t "compose",
      (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "compose" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.compose b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "default",
      (S.SLens ^> S.SString ^> S.SLens),
     let b = mk_prelude_info "default" in 
     mk_lfun b (fun cl1 -> 
       mk_sfun b (fun def -> 
         Lns(b,L.default b def cl1))))

  ; (S.Qid.mk_native_prelude_t "key",
      (S.SRegexp ^> S.SLens),
     let b = mk_prelude_info "key" in 
       mk_rfun b (fun r -> 
         Lns(b,L.key b r)))

  ; (S.Qid.mk_native_prelude_t "duplicate",
      (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "duplicate" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->
         mk_lfun b (fun l3 -> 
           Lns(b,L.duplicate b true l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "duplicate_snd",
      (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "duplicate_snd" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->
         mk_lfun b (fun l3 -> 
           Lns(b,L.duplicate b false l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "count",
      (S.SRegexp ^> S.SLens),
     let b = mk_prelude_info "count" in 
     mk_rfun b (fun r -> 
       Lns(b,L.count b r)))

  ; (S.Qid.mk_native_prelude_t "forgetkey",
      (S.SLens ^> S.SLens),
     let b = mk_prelude_info "forgetkey" in 
     mk_lfun b (fun cl ->
       Lns(b, L.forgetkey cl)))

  ; (S.Qid.mk_native_prelude_t "filter",
      (S.SRegexp ^> S.SRegexp ^> S.SLens),
     let b = mk_prelude_info "filter" in 
     mk_rfun b (fun r1 ->
       mk_rfun b (fun r2 ->
         Lns(b,L.filter b r1 r2))))

  (* canonizer operations *)
  ; (S.Qid.mk_native_prelude_t "canonizer_union",
      (S.SCanonizer ^> S.SCanonizer ^> S.SCanonizer),
     let b = mk_prelude_info "canonizer_union" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 -> 
         Lns(b,L.union b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "canonizer_concat",
      (S.SCanonizer ^> S.SCanonizer ^> S.SCanonizer),
     let b = mk_prelude_info "canonizer_concat" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.concat b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "canonizer_iter",
      (S.SCanonizer ^> S.SInteger ^> S.SInteger ^> S.SCanonizer),
     let b = mk_prelude_info "canonizer_iter" in 
     mk_lfun b (fun l1 ->
       mk_ifun b (fun i1 -> 
         mk_ifun b (fun i2 -> 
           Lns(b,L.iter b l1 i1 i2)))))

  ; (S.Qid.mk_native_prelude_t "canonizer_swap",
      (S.SCanonizer ^> S.SCanonizer ^> S.SCanonizer),
     let b = mk_prelude_info "canonizer_swap" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.swap b l1 l2))))

  ; (S.Qid.mk_native_prelude_t "cls",
      (S.SCanonizer ^> S.SString ^> S.SString),
     let b = mk_prelude_info "cls" in 
     mk_cfun b (fun c1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,C.cls c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "rep",
      (S.SCanonizer ^> S.SString ^> S.SString),
     let b = mk_prelude_info "rep" in 
     mk_cfun b (fun c1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,C.rep c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "left_quot",
      (S.SCanonizer ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "left_quot" in 
     mk_cfun b (fun c1 ->                                              
       mk_lfun b (fun l1 -> 
         Lns(b,L.left_quot b c1 l1))))

  ; (S.Qid.mk_native_prelude_t "right_quot",
      (S.SLens ^> S.SCanonizer ^> S.SLens),
     let b = mk_prelude_info "right_quot" in 
     mk_lfun b (fun l1 ->                                              
       mk_cfun b (fun c1 -> 
         Lns(b,L.right_quot b l1 c1))))

  ; (S.Qid.mk_native_prelude_t "columnize",
      (S.SString ^> S.SRegexp ^> S.SString ^> S.SString ^> S.SCanonizer),
     let b = mk_prelude_info "columnize" in 
     mk_sfun b (fun k -> 
       mk_rfun b (fun r -> 
         mk_sfun b (fun s -> 
           mk_sfun b (fun nl -> 
             Can(b,C.columnize b k r s nl))))))

  (* char operations *)
  ; (S.Qid.mk_native_prelude_t "string_of_char",
    (S.SChar ^> S.SString),
     let b = mk_prelude_info "string_of_char" in 
     mk_chfun b (fun c -> 
       Str(b,RS.make 1 c)))

  (* string operations *)
  ; (S.Qid.mk_native_prelude_t "string_concat",
      (S.SString ^> S.SString ^> S.SString),
     let b = mk_prelude_info "string_concat" in 
     mk_sfun b (fun s1 -> 
       mk_sfun b (fun s2 ->         
         Str(b,RS.append s1 s2))))

  ; (S.Qid.mk_native_prelude_t "read",
      (S.SString ^> S.SString),
     let b = mk_prelude_info "read" in 
     mk_sfun b (fun s1 -> 
       Str(b,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  (* regexp operations *)
  ; (S.Qid.mk_native_prelude_t "str",
      (S.SString ^> S.SRegexp),
     let b = mk_prelude_info "str" in 
     mk_sfun b (fun s1 ->
       Rx(b,R.str false s1)))

  ; (S.Qid.mk_native_prelude_t "empty",
      (S.SRegexp),
     let b = mk_prelude_info "empty" in 
       Rx(b,R.empty))

  ; (S.Qid.mk_native_prelude_t "regexp_concat",
      (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_info "regexp_concat" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.seq r1 r2))))

  ; (S.Qid.mk_native_prelude_t "regexp_union",
     (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_info "regexp_union" in 
      mk_rfun b (fun r1 -> 
        mk_rfun b (fun r2 ->         
          Rx(b,R.alt r1 r2))))

  ; (S.Qid.mk_native_prelude_t "diff",
      (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_info "diff" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.diff r1 r2))))

  ; (S.Qid.mk_native_prelude_t "inter",
      (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_info "inter" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.inter r1 r2))))

  ; (S.Qid.mk_native_prelude_t "regexp_iter",
      (S.SRegexp ^> S.SInteger ^> S.SInteger ^> S.SRegexp),
     let b = mk_prelude_info "regexp_iter" in 
     mk_rfun b (fun r1 ->         
       mk_ifun b (fun i1 -> 
         mk_ifun b (fun i2 -> 
           Rx(b,R.iter r1 i1 i2)))))

  ; (S.Qid.mk_native_prelude_t "equiv",
      (S.SRegexp ^> S.SRegexp ^> S.SString),
     let b = mk_prelude_info "equiv" in 
     mk_rfun b (fun r1 ->
       mk_rfun b (fun r2 ->
	 Str(b, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))
    
  ; (S.Qid.mk_native_prelude_t "shortest",
      (S.SRegexp ^> S.SString),
     let b = mk_prelude_info "shortest" in 
     mk_rfun b (fun r1 -> Str(b,wrap_rep b r1)))

  (* run-time checking *)
  ; (S.Qid.mk_native_prelude_t "assert",
      (S.SRegexp ^> S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "assert" in 
     mk_rfun b (fun c -> 
       mk_rfun b (fun a -> 
         mk_lfun b (fun l -> 
           Lns(b,L.assert_lens_type b l (Some c) (Some a))))))

  ; (S.Qid.mk_native_prelude_t "assert_ctype",
      (S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "assert_ctype" in 
     mk_rfun b (fun c -> 
       mk_lfun b (fun l -> 
         Lns(b,L.assert_lens_ctype b l c))))

  ; (S.Qid.mk_native_prelude_t "assert_atype",
      (S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_info "assert_atype" in 
     mk_rfun b (fun a -> 
       mk_lfun b (fun l -> 
         Lns(b,L.assert_lens_atype b l a))))

  (* coercions *)
  ; (S.Qid.mk_native_prelude_t "ctype",
      (S.SLens ^> S.SRegexp),
     let b = mk_prelude_info "ctype" in 
     mk_lfun b (fun cl ->
       Rx(b, L.ctype cl)))

  ; (S.Qid.mk_native_prelude_t "atype",
      (S.SLens ^> S.SRegexp),
     let b = mk_prelude_info "atype" in 
     mk_lfun b (fun cl ->
       Rx(b, L.atype cl)))

  ; (S.Qid.mk_native_prelude_t "canonizer_of_lens",
      (S.SLens ^> S.SCanonizer),
     let b = mk_prelude_info "canonizer_of_lens" in 
     mk_lfun b (fun l1 -> 
       Can(b,L.canonizer_of_t b l1)))

  ; (S.Qid.mk_native_prelude_t "string_of_regexp",
      (S.SRegexp ^> S.SString),
     let b = mk_prelude_info "string_of_regexp" in 
     mk_rfun b (fun r1 ->
      Str(b, RS.t_of_string (R.string_of_t r1))))

  ; begin 
    let i = Info.M "equal built-in" in 
    let a = S.Id.mk i "a" in 
    let a_sort = S.SVar a in 
    (S.Qid.mk_native_prelude_t "equals",
     S.SForall(a,a_sort ^> a_sort ^> S.SBool),
     mk_ufun i (fun () -> 
       mk_poly_fun i (fun v1 -> 
         mk_poly_fun i (fun v2 -> 
           Bol(i,equal v1 v2)))))
   end
 
  ; begin 
    let i = Info.M "blame built-in" in 
    let a = S.Id.mk i "a" in 
    let a_sort = S.SVar a in 
    (S.Qid.mk_native_prelude_t "blame",
    S.SForall(a,S.SString ^> a_sort),
    mk_ufun i (fun () -> 
      mk_sfun i (fun s1 -> 
        (raise(Error.Harmony_error 
          (fun () -> 
             Util.format "@[%s blamed!@]" (Bstring.string_of_t s1)))))))
  end

  ; begin 
    let i = Info.M "fold_left built-in" in 
    let nil = S.Id.mk (Info.M "Nil built-in") "Nil" in 
    let cons = S.Id.mk (Info.M "Cons built-in!") "Cons" in 
    let a = S.Id.mk i "a" in 
    let a_sort = S.SVar a in 
    let b = S.Id.mk i "b" in 
    let b_sort = S.SVar b in 
    let b_list_sort = S.SData([b_sort],S.Qid.mk_list_t "t") in      
    (S.Qid.mk_native_prelude_t "fold_left",
     S.SForall(a,S.SForall(b, ((a_sort ^> b_sort ^> a_sort) ^> a_sort ^> b_list_sort ^> a_sort))),
     let b = mk_prelude_info "fold_left" in 
     mk_poly_fun b (fun achk -> 
       mk_poly_fun b (fun bchk -> 
         mk_poly_fun b (fun f0 -> 
           mk_poly_fun b (fun acc0 -> 
             mk_poly_fun b (fun l0 ->                                                   
               let f = get_f f0 in 
               let rec aux l acc = 
               let lbl,vo = get_v l in 
               if S.Id.equal lbl nil then acc
               else if S.Id.equal lbl cons then 
                 let hd,tl = match vo with 
                   | None -> poly_error b b_list_sort l
                   | Some v -> get_p v in                
                 let acc' = (get_f (f acc)) hd in 
                   aux tl acc'
               else 
                 poly_error b b_list_sort l in 
                 aux l0 acc0))))))
  end ]
    
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v)
    prelude_spec
