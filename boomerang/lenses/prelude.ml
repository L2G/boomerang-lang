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

let lift0 s1 mk1 x v1 = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 in  
  let i = mk_prelude_info x in 
  let v = mk1 i v1 in 
  (q,s,v)

let lift s1 mk1 s2 mk2 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (f i v1)) in 
  (q,s,f)

let lift2 s1 mk1 s2 mk2 s3 mk3 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (f i v1 v2))) in 
  (q,s,f)

let lift3 s1 mk1 s2 mk2 s3 mk3 s4 mk4 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 ^> s4 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (f i v1 v2 v3)))) in 
  (q,s,f)

let lift4 s1 mk1 s2 mk2 s3 mk3 s4 mk4 s5 mk5 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 ^> s4 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (fun v4 -> mk5 i (f i v1 v2 v3 v4))))) in 
  (q,s,f)

let lift_bb = lift S.SBool mk_bfun S.SBool mk_b
let lift_bbb = lift2 S.SBool mk_bfun S.SBool mk_bfun S.SBool mk_b 
let lift_qqq = lift2 S.SCanonizer mk_qfun S.SCanonizer mk_qfun S.SCanonizer mk_q 
let lift_qq = lift S.SCanonizer mk_qfun S.SCanonizer mk_q 
let lift_qiiq = lift3 S.SCanonizer mk_qfun S.SInteger mk_ifun S.SInteger mk_ifun S.SCanonizer mk_q
let lift_riir = lift3 S.SRegexp mk_rfun S.SInteger mk_ifun S.SInteger mk_ifun S.SRegexp mk_r
let lift_qll = lift2 S.SCanonizer mk_qfun S.SLens mk_lfun S.SLens mk_l
let lift_qss = lift2 S.SCanonizer mk_qfun S.SString mk_sfun S.SString mk_s
let lift_lss = lift2 S.SLens mk_lfun S.SString mk_sfun S.SString mk_s
let lift_sss = lift2 S.SString mk_sfun S.SString mk_sfun S.SString mk_s
let lift_ss = lift S.SString mk_sfun S.SString mk_s
let lift_sr = lift S.SString mk_sfun S.SRegexp mk_r
let lift_lsss = lift3 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_s 
let lift_lql = lift2 S.SLens mk_lfun S.SCanonizer mk_qfun S.SLens mk_l
let lift_ll = lift S.SLens mk_lfun S.SLens mk_l
let lift_lr = lift S.SLens mk_lfun S.SRegexp mk_r
let lift_lq = lift S.SLens mk_lfun S.SCanonizer mk_q
let lift_cs = lift S.SChar mk_cfun S.SString mk_s
let lift_rl = lift S.SRegexp mk_rfun S.SLens mk_l
let lift_rb = lift S.SRegexp mk_rfun S.SBool mk_b
let lift_rrl = lift2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SLens mk_l
let lift_rrb = lift2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let lift_rs = lift S.SRegexp mk_rfun S.SString mk_s
let lift_rsb = lift2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_b
let lift_rrr = lift2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SRegexp mk_r
let lift_lll = lift2 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let lift_llll = lift3 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let lift_lrrb = lift3 S.SLens mk_lfun S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let lift_liil = lift3 S.SLens mk_lfun S.SInteger mk_ifun S.SInteger mk_ifun S.SLens mk_l
let lift_rssl = lift3 S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SLens mk_l
let lift_sll = lift2 S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let lift_lsl = lift2 S.SLens mk_lfun S.SString mk_sfun S.SLens mk_l
let lift_ssll = lift3 S.SString mk_sfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let lift_srssq = lift4 S.SString mk_sfun S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SCanonizer mk_q

let lift_0r = lift0 S.SRegexp mk_r

let prelude_spec =
  [ (* lens operations *)
    lift_lss  "get"    (fun _ -> L.get)
  ; lift_lsss "put"    (fun _ -> L.rput_of_dl)
  ; lift_lss  "create" (fun _ -> L.rcreate_of_dl)
  ; lift_ll   "invert" L.invert

  (* core lens combinators *)
  ; lift_rl   "copy"           L.copy
  ; lift_rssl "const"          L.const
  ; lift_lll  "lens_union"     L.union
  ; lift_lll  "lens_concat"    L.concat
  ; lift_liil "lens_iter"      L.iter
  ; lift_lll  "lens_swap"      L.swap
  ; lift_sll  "dmatch"         (fun i -> L.dmatch i L.std_lookup)
  ; lift_ssll "smatch"         (fun i s -> 
                                  let f = float_of_string (RS.string_of_t s) in 
                                  L.dmatch i (L.sim_lookup f))
  ; lift_lll  "compose"        L.compose
  ; lift_sll  "default"        L.default
  ; lift_rl   "key"            L.key
  ; lift_llll  "duplicate"     (fun i -> L.duplicate i true)
  ; lift_llll  "duplicate_snd" (fun i -> L.duplicate i false)
  ; lift_rl   "count"          L.count 
  ; lift_ll   "forgetkey"      (fun i -> L.forgetkey)
  ; lift_rrl  "filter"         L.filter

  (* canonizer operations *)
  ; lift_qqq   "canonizer_union" C.union
  ; lift_qqq   "canonizer_concat" C.concat
  ; lift_qiiq  "canonizer_iter"   C.iter
  ; lift_qss   "canonize"         (fun _ -> C.cls)
  ; lift_qss   "choose"           (fun _ -> C.rep)
  ; lift_qll   "left_quot"        L.left_quot
  ; lift_lql   "right_quot"       L.right_quot
  ; lift_srssq "columnize"        C.columnize

  (* char operations *)
  ; lift_cs    "string_of_char"   (fun _ -> RS.make 1)

  (* string operations *)
  ; lift_sss   "string_concat"    (fun _ -> RS.append)
  ; lift_ss    "read"             (fun _ fn -> RS.t_of_string (Misc.read (RS.string_of_t fn)))

  (* regexp operations *)
  ; lift_sr    "str"              (fun _ -> R.str false)
  ; lift_0r    "empty"            R.empty
  ; lift_rb    "is_empty"         (fun _ -> R.is_empty)
  ; lift_rrr   "regexp_concat"    (fun _ -> R.seq)
  ; lift_rrr   "regexp_union"     (fun _ -> R.alt)
  ; lift_rrr   "regexp_diff"      (fun _ -> R.diff)
  ; lift_rrr   "regexp_inter"     (fun _ -> R.inter)
  ; lift_riir  "regexp_iter"      (fun _ -> R.iter)
  ; lift_rrb   "equiv"            (fun _ -> R.equiv)
  ; lift_rs    "shortest"         wrap_rep
  ; lift_rsb   "matches"          (fun _ -> R.match_str)

  (* boolean operations *)
  ; lift_bbb   "land"             (fun _ -> (&&))
  ; lift_bbb   "lor"              (fun _ -> (||))
  ; lift_bb    "not"              (fun _ -> not)

  (* run-time checking *)
  ; lift_lrrb  "has_lens_type"     (fun _ l c a -> R.equiv (L.ctype l) c && R.equiv (L.atype l) a)
  ; lift_lr    "ctype"             (fun _ -> L.ctype)
  ; lift_lr    "atype"             (fun _ -> L.atype)
  ; lift_lq    "canonizer_of_lens" L.canonizer_of_t
  ; lift_rs    "string_of_regexp"  (fun _ r1 -> (RS.t_of_string (R.string_of_t r1)))

  (* polymorphic ugliness *)
  ; begin 
    let i = Info.M "equal built-in" in 
    let a = S.Id.mk i "a" in 
    let a_sort = S.SVar a in 
    (S.Qid.mk_native_prelude_t "equals",
     S.SForall(a,a_sort ^> a_sort ^> S.SBool),
     mk_ufun i (fun () -> 
       mk_f i (fun v1 -> 
         mk_f i (fun v2 -> 
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
     mk_f b (fun achk -> 
       mk_f b (fun bchk -> 
         mk_f b (fun f0 -> 
           mk_f b (fun acc0 -> 
             mk_f b (fun l0 ->                                                   
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
