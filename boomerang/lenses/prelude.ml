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
let sprintf = Printf.sprintf 
let (^>) = S.(^>)

(* sbset_of_sl: convert a list of base_sorts to a BSSet.t *)
let bsset_of_sl l = 
  Safelist.fold_left 
    (fun s si -> S.BSSet.add si s) 
    S.BSSet.empty l 


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

let mk_prelude_blame s = Bvalue.Pos (Info.M (sprintf "%s built-in" s))

let prelude_spec =
  [(* lens operations *)
   (S.Qid.mk_native_prelude_t "get", 
    S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString),
    let b = mk_prelude_blame "get" in 
    mk_lfun b (fun l1 -> 
      mk_sfun b (fun s1 -> 
        Str(b,L.get l1 s1))))

  ; (S.Qid.mk_native_prelude_t "put", 
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString ^> S.SString),
     let b = mk_prelude_blame "put" in 
     mk_lfun b (fun l1 -> 
       mk_sfun b (fun s1 -> 
         mk_sfun b (fun s2 -> 
           Str(b,L.rput_of_dl l1 s1 s2)))))

  ; (S.Qid.mk_native_prelude_t "create", 
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SString),
     let b = mk_prelude_blame "create" in 
     mk_lfun b (fun l1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,L.rcreate_of_dl l1 s1))))

  ; (S.Qid.mk_native_prelude_t "invert",
     S.scheme_of_sort (S.SLens ^> S.SLens), 
     let b = mk_prelude_blame "invert" in 
     mk_lfun b (fun l1 -> 
      Lns(b,L.invert(info_of_blame b) l1)))

  (* core lens combinators *)
  ; (S.Qid.mk_native_prelude_t "concat",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "concat" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.concat (info_of_blame b) l1 l2))))

  ; (S.Qid.mk_native_prelude_t "union",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "union" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.union (info_of_blame b) l1 l2))))

  ; (S.Qid.mk_native_prelude_t "dmatch",
     S.scheme_of_sort (S.SString ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "dmatch" in 
     mk_sfun b (fun s1 ->
       mk_lfun b (fun l1 -> 
         Lns(b,L.dmatch (info_of_blame b) L.std_lookup s1 l1))))

  ; (S.Qid.mk_native_prelude_t "smatch",
     S.scheme_of_sort (S.SString ^> S.SString ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "smatch" in 
     mk_sfun b (fun s1 ->
       let f = float_of_string (RS.string_of_t s1) in 
       mk_sfun b (fun s2 ->                                            
         mk_lfun b (fun l1 -> 
           Lns(b,L.dmatch (info_of_blame b) (L.sim_lookup f) s2 l1)))))

  ; (S.Qid.mk_native_prelude_t "star",
     S.scheme_of_sort (S.SLens ^> S.SLens),
     let b = mk_prelude_blame "star" in 
     mk_lfun b (fun l1 ->         
       Lns(b,L.star (info_of_blame b) l1)))

  (* extensions *)  
  ; (S.Qid.mk_native_prelude_t "swap",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "swap" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.swap (info_of_blame b) l1 l2))))

  ; (S.Qid.mk_native_prelude_t "compose",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "compose" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->         
         Lns(b,L.compose (info_of_blame b) l1 l2))))

  ; (S.Qid.mk_native_prelude_t "default",
     S.scheme_of_sort (S.SLens ^> S.SString ^> S.SLens),
     let b = mk_prelude_blame "default" in 
     mk_lfun b (fun cl1 -> 
       mk_sfun b (fun def -> 
         Lns(b,L.default (info_of_blame b) def cl1))))

  ; (S.Qid.mk_native_prelude_t "key",
     S.scheme_of_sort (S.SRegexp ^> S.SLens),
     let b = mk_prelude_blame "key" in 
       mk_rfun b (fun r -> 
         Lns(b,L.key (info_of_blame b) r)))

  ; (S.Qid.mk_native_prelude_t "duplicate",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "duplicate" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->
         mk_lfun b (fun l3 -> 
           Lns(b,L.duplicate (info_of_blame b) true l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "duplicate_snd",
     S.scheme_of_sort (S.SLens ^> S.SLens ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "duplicate_snd" in 
     mk_lfun b (fun l1 -> 
       mk_lfun b (fun l2 ->
         mk_lfun b (fun l3 -> 
           Lns(b,L.duplicate (info_of_blame b) false l1 l2 l3)))))

  ; (S.Qid.mk_native_prelude_t "count",
     S.scheme_of_sort (S.SRegexp ^> S.SLens),
     let b = mk_prelude_blame "count" in 
     mk_rfun b (fun r -> 
       Lns(b,L.count (info_of_blame b) r)))

  ; (S.Qid.mk_native_prelude_t "forgetkey",
     S.scheme_of_sort (S.SLens ^> S.SLens),
     let b = mk_prelude_blame "forgetkey" in 
     mk_lfun b (fun cl ->
       Lns(b, L.forgetkey cl)))

  ; (S.Qid.mk_native_prelude_t "filter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SLens),
     let b = mk_prelude_blame "filter" in 
     mk_rfun b (fun r1 ->
       mk_rfun b (fun r2 ->
         Lns(b,L.filter (info_of_blame b) r1 r2))))

  (* canonizer operations *)
  ; (S.Qid.mk_native_prelude_t "cls",
     S.scheme_of_sort (S.SCanonizer ^> S.SString ^> S.SString),
     let b = mk_prelude_blame "cls" in 
     mk_cfun b (fun c1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,C.cls c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "rep",
     S.scheme_of_sort (S.SCanonizer ^> S.SString ^> S.SString),
     let b = mk_prelude_blame "rep" in 
     mk_cfun b (fun c1 -> 
       mk_sfun b (fun s1 -> 
         Str(b,C.rep c1 s1))))                          

  ; (S.Qid.mk_native_prelude_t "left_quot",
     S.scheme_of_sort (S.SCanonizer ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "left_quot" in 
     mk_cfun b (fun c1 ->                                              
       mk_lfun b (fun l1 -> 
         Lns(b,L.left_quot (info_of_blame b) c1 l1))))

  ; (S.Qid.mk_native_prelude_t "right_quot",
     S.scheme_of_sort (S.SLens ^> S.SCanonizer ^> S.SLens),
     let b = mk_prelude_blame "right_quot" in 
     mk_lfun b (fun l1 ->                                              
       mk_cfun b (fun c1 -> 
         Lns(b,L.right_quot (info_of_blame b) l1 c1))))

  ; (S.Qid.mk_native_prelude_t "columnize",
     S.scheme_of_sort (S.SString ^> S.SRegexp ^> S.SString ^> S.SString ^> S.SCanonizer),
     let b = mk_prelude_blame "columnize" in 
     mk_sfun b (fun k -> 
       mk_rfun b (fun r -> 
         mk_sfun b (fun s -> 
           mk_sfun b (fun nl -> 
             Can(b,C.columnize (info_of_blame b) k r s nl))))))

  (* string operations *)
  ; (S.Qid.mk_native_prelude_t "append",
     S.scheme_of_sort (S.SString ^> S.SString ^> S.SString),
     let b = mk_prelude_blame "append" in 
     mk_sfun b (fun s1 -> 
       mk_sfun b (fun s2 ->         
         Str(b,RS.append s1 s2))))

  ; (S.Qid.mk_native_prelude_t "read",
     S.scheme_of_sort (S.SString ^> S.SString),
     let b = mk_prelude_blame "read" in 
     mk_sfun b (fun s1 -> 
       Str(b,RS.t_of_string (Misc.read (RS.string_of_t s1)))))

  (* regexp operations *)
  ; (S.Qid.mk_native_prelude_t "str",
     S.scheme_of_sort (S.SString ^> S.SRegexp),
     let b = mk_prelude_blame "str" in 
     mk_sfun b (fun s1 ->
       Rx(b,R.str false s1)))

  ; (S.Qid.mk_native_prelude_t "empty",
     S.scheme_of_sort (S.SRegexp),
     let b = mk_prelude_blame "empty" in 
       Rx(b,R.empty))

  ; (S.Qid.mk_native_prelude_t "seq",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_blame "seq" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.seq r1 r2))))

  ; (S.Qid.mk_native_prelude_t "alt",
    S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_blame "alt" in 
      mk_rfun b (fun r1 -> 
        mk_rfun b (fun r2 ->         
          Rx(b,R.alt r1 r2))))

  ; (S.Qid.mk_native_prelude_t "diff",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_blame "diff" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.diff r1 r2))))

  ; (S.Qid.mk_native_prelude_t "inter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_blame "inter" in 
     mk_rfun b (fun r1 -> 
       mk_rfun b (fun r2 ->         
         Rx(b,R.inter r1 r2))))

  ; (S.Qid.mk_native_prelude_t "iter",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp),
     let b = mk_prelude_blame "inter" in 
     mk_rfun b (fun r1 ->         
       Rx(b,R.star r1)))

  ; (S.Qid.mk_native_prelude_t "equiv",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SString),
     let b = mk_prelude_blame "equiv" in 
     mk_rfun b (fun r1 ->
       mk_rfun b (fun r2 ->
	 Str(b, RS.t_of_string(string_of_bool (R.equiv r1 r2))))))
    
  (* run-time checking *)
  ; (S.Qid.mk_native_prelude_t "assert",
     S.scheme_of_sort (S.SRegexp ^> S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "assert" in 
     mk_rfun b (fun c -> 
       mk_rfun b (fun a -> 
         mk_lfun b (fun l -> 
           Lns(b,L.assert_lens_type (info_of_blame b) l (Some c) (Some a))))))

  ; (S.Qid.mk_native_prelude_t "assert_ctype",
     S.scheme_of_sort (S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "assert_ctype" in 
     mk_rfun b (fun c -> 
       mk_lfun b (fun l -> 
         Lns(b,L.assert_lens_ctype (info_of_blame b) l c))))

  ; (S.Qid.mk_native_prelude_t "assert_atype",
     S.scheme_of_sort (S.SRegexp ^> S.SLens ^> S.SLens),
     let b = mk_prelude_blame "assert_atype" in 
     mk_rfun b (fun a -> 
       mk_lfun b (fun l -> 
         Lns(b,L.assert_lens_atype (info_of_blame b) l a))))

  (* coercions *)
  ; (S.Qid.mk_native_prelude_t "ctype",
     S.scheme_of_sort (S.SLens ^> S.SRegexp),
     let b = mk_prelude_blame "ctype" in 
     mk_lfun b (fun cl ->
       Rx(b, L.ctype cl)))

  ; (S.Qid.mk_native_prelude_t "atype",
     S.scheme_of_sort (S.SLens ^> S.SRegexp),
     let b = mk_prelude_blame "atype" in 
     mk_lfun b (fun cl ->
       Rx(b, L.atype cl)))

  ; (S.Qid.mk_native_prelude_t "canonizer_of_lens",
     S.scheme_of_sort (S.SLens ^> S.SCanonizer),
     let b = mk_prelude_blame "canonizer_of_lens" in 
     mk_lfun b (fun l1 -> 
       Can(b,L.canonizer_of_t (info_of_blame b) l1)))

  ; (S.Qid.mk_native_prelude_t "string_of_regexp",
     S.scheme_of_sort (S.SRegexp ^> S.SString),
     let b = mk_prelude_blame "string_of_regexp" in 
     mk_rfun b (fun r1 ->
      Str(b, RS.t_of_string (R.string_of_t r1))))

  (* polymorphic operators *)
  ; begin
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "shortest",
     S.mk_scheme [alpha] (a ^> S.SString),
     let b = mk_prelude_blame "shortest" in 
     mk_poly_fun b (fun v1 -> 
       match v1 with 
         | Str _ -> Str(b,get_s v1)
         | Rx _  -> Str(b,wrap_rep (info_of_blame b) (get_r v1))
         | _     -> poly_error (info_of_blame b) a v1))
  end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "const",
     S.mk_scheme [alpha] (a ^> S.SString ^> S.SString ^> S.SLens),
     let b = mk_prelude_blame "const" in 
     mk_poly_fun b (fun v1 -> 
       mk_sfun b (fun s1 -> 
         mk_sfun b (fun s2 -> 
           match v1 with 
             | Str _ -> 
                 let r = R.str false (get_s v1) in                  
                 Lns(b,L.const (info_of_blame b) r s1 s2)
             | Rx _  -> Lns(b,L.const (info_of_blame b) (get_r v1) s1 s2)
             | _     -> poly_error (info_of_blame b) a v1))))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Str;S.Reg])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "copy",
     S.mk_scheme [alpha] (a ^> S.SLens),
     let b = mk_prelude_blame "copy" in 
     mk_poly_fun b (fun v1 -> 
         match v1 with
           | Str _ -> Lns(b,L.copy (info_of_blame b) (R.str false (get_s v1)))
           | Rx _  -> Lns(b,L.copy (info_of_blame b) (get_r v1))
           | _     -> poly_error (info_of_blame b) a v1))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Str;S.Reg;S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_concat",
     S.mk_scheme [alpha] (a ^> a ^> a),
     let b = mk_prelude_blame "poly_concat" in 
     mk_poly_fun b (fun v1 -> 
       mk_poly_fun b (fun v2 ->
         match v1,v2 with
           | Str _,Str _  -> Str(b,RS.append (get_s v1) (get_s v2))
           | Rx _, Rx _   -> Rx(b,R.seq (get_r v1) (get_r v2))
           | Lns _, Lns _ -> Lns(b,L.concat (info_of_blame b) (get_l v1) (get_l v2))
           | Can _, Can _ -> Can(b,C.concat (info_of_blame b) (get_c v1) (get_c v2))
           | _            -> poly_bin_error (info_of_blame b) a a v1 v2)))
    end

  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Str;S.Reg;S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_union",
     S.mk_scheme [alpha] (a ^> a ^> a),
     let b = mk_prelude_blame "poly_union" in 
     mk_poly_fun b (fun v1 -> 
       mk_poly_fun b (fun v2 ->     
         match v1,v2 with 
           | Rx _, Rx _  -> Rx(b,R.alt (get_r v1) (get_r v2))
           | Lns _, Lns _ -> Lns(b,L.union (info_of_blame b) (get_l v1) (get_l v2))
           | Can _, Can _ -> Can(b,C.union (info_of_blame b) (get_c v1) (get_c v2))
           | _            -> poly_bin_error (info_of_blame b) a a v1 v2)))
  end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Reg;S.Lns;S.Can])) in 
    let a = S.SVar alpha in 
    (*** TODO: Kill this hack as soon as we add integers! *)
    let get_int s = 
      try int_of_string (RS.string_of_t s) 
      with _ -> raise
        (Error.Harmony_error
           (fun () -> 
              Util.format "expected string representing an integer, found %s."
                (RS.string_of_t s))) in     
    (S.Qid.mk_native_prelude_t "poly_iter",
     S.mk_scheme [alpha] (a ^> S.SString ^> S.SString ^> a),
     let b = mk_prelude_blame "union" in 
     mk_poly_fun b (fun v1 -> 
       mk_sfun b (fun s1 -> 
         mk_sfun b (fun s2 -> 
           let min = get_int s1 in
           let maxo = if RS.length s2 = 0 then None else Some(get_int s2) in 
             match v1 with 
               | Rx _  -> Rx(b,R.iter (get_r v1) min maxo)
               | Lns _ -> Lns(b,L.iter (info_of_blame b) (get_l v1) min maxo)
               | Can _ -> Can(b,C.iter (info_of_blame b) (get_c v1) min maxo)
               | _            -> poly_error (info_of_blame b) a v1))))
    end
  ; begin 
    let alpha = Bunify.fresh_svar (S.Con (bsset_of_sl [S.Lns])) in 
    let a = S.SVar alpha in 
    (S.Qid.mk_native_prelude_t "poly_swap",
     S.mk_scheme [alpha] (a ^> a ^> a),
     let b = mk_prelude_blame "poly_swap" in 
     mk_poly_fun b (fun v1 -> 
       mk_poly_fun b (fun v2 ->     
         match v1,v2 with 
           | Lns _, Lns _ -> Lns(b,L.swap (info_of_blame b) (get_l v1) (get_l v2))
           | _            -> poly_bin_error (info_of_blame b) a a v1 v2)))
    end
  ; begin 
    let nil = S.Id.mk (Info.M "Nil built-in") "Nil" in 
    let cons = S.Id.mk (Info.M "Cons built-in") "Cons" in 
    let alpha = Bunify.fresh_svar S.Fre in 
    let beta = Bunify.fresh_svar S.Fre in 
    let al = S.SVar alpha in 
    let bt = S.SVar beta in 
    let list_bt = S.SData([bt],S.Qid.mk_list_t "t") in      
    (S.Qid.mk_native_prelude_t "fold_left",
     S.mk_scheme [alpha] ((al ^> bt ^> al) ^> al ^> list_bt ^> al),
     let b = mk_prelude_blame "fold_left" in 
     mk_poly_fun b (fun f0 -> 
       mk_poly_fun b (fun acc0 -> 
         mk_poly_fun b (fun l0 ->                                                   
           let f = get_f f0 in 
           let rec aux l acc = 
             let lbl,vo = get_v l in 
             if S.Id.equal lbl nil then acc
             else if S.Id.equal lbl cons then 
               let hd,tl = match vo with 
                 | None -> poly_error (info_of_blame b) list_bt l
                 | Some v -> get_p v in                
               let acc' = (get_f (f hd)) acc in 
               aux tl acc'
             else 
               poly_error (info_of_blame b) list_bt l in 
           aux l0 acc0))))
  end ]  
    
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v) 
    prelude_spec
