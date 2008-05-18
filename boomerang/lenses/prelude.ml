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

(* The next several helpers construct run-time values; each constructs
   a triple consisting of its name (a Bsyntax.Qid.t), Bsyntax.sort,
   and Bvalue.t *)
let pmk0 s1 mk1 x v1 = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 in  
  let i = mk_prelude_info x in 
  let v = mk1 i v1 in 
    (q,s,v)

let pmk1 s1 mk1 s2 mk2 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (f i v1)) in 
    (q,s,f)

let pmk2 s1 mk1 s2 mk2 s3 mk3 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (f i v1 v2))) in 
    (q,s,f)

let pmk3 s1 mk1 s2 mk2 s3 mk3 s4 mk4 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 ^> s4 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (f i v1 v2 v3)))) in 
    (q,s,f)

let pmk4 s1 mk1 s2 mk2 s3 mk3 s4 mk4 s5 mk5 x f = 
  let q = S.Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 ^> s4 ^> s5 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (fun v4 -> mk5 i (f i v1 v2 v3 v4))))) in 
    (q,s,f)

(* The helpers lift OCaml functions to work with run-time values. They
   perform unboxing of the arguments, and box the result up as a
   Bvalue.t. Each letter after the underscore indicates a sort---e.g.,
   pmk_bb lifts a (Info.t -> bool -> bool) function to a Bvalue.t Fun
   that maps a Bol to a Bol. The following legend gives the mapping
   between letters and boxed run-time values:

     u : Unt
     b : Bol
     i : Int
     c : Chr
     s : Str
     r : Rx
     l : Lns
     q : Can
     p : Par
     v : Vnt (unused)
     f : Fun (unused)
*)

let pmk_cs    = pmk1 S.SChar mk_cfun S.SString mk_s

let pmk_bb    = pmk1 S.SBool mk_bfun S.SBool mk_b
let pmk_bbb   = pmk2 S.SBool mk_bfun S.SBool mk_bfun S.SBool mk_b 

let pmk_iib   = pmk2 S.SInteger mk_ifun  S.SInteger mk_ifun S.SBool mk_b
let pmk_iii   = pmk2 S.SInteger mk_ifun S.SInteger mk_ifun S.SInteger mk_i 

let pmk_ss    = pmk1 S.SString mk_sfun S.SString mk_s
let pmk_sss   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SString mk_s
let pmk_sr    = pmk1 S.SString mk_sfun S.SRegexp mk_r
let pmk_sll   = pmk2 S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_ssll  = pmk3 S.SString mk_sfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_srssq = pmk4 S.SString mk_sfun S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SCanonizer mk_q

let pmk_r     = pmk0 S.SRegexp mk_r 
let pmk_rrr   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SRegexp mk_r
let pmk_riir  = pmk3 S.SRegexp mk_rfun S.SInteger mk_ifun S.SInteger mk_ifun S.SRegexp mk_r
let pmk_rb    = pmk1 S.SRegexp mk_rfun S.SBool mk_b
let pmk_rrb    = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rs    = pmk1 S.SRegexp mk_rfun S.SString mk_s
let pmk_rl    = pmk1 S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrl   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrb   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rsb   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_b
let pmk_rssl  = pmk3 S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SLens mk_l

let pmk_ll    = pmk1 S.SLens mk_lfun S.SLens mk_l
let pmk_lll   = pmk2 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_llll  = pmk3 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_liil  = pmk3 S.SLens mk_lfun S.SInteger mk_ifun S.SInteger mk_ifun S.SLens mk_l
let pmk_lr    = pmk1 S.SLens mk_lfun S.SRegexp mk_r
let pmk_lss   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SString mk_s
let pmk_lsl   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SLens mk_l
let pmk_lsss  = pmk3 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_s 
let pmk_lq    = pmk1 S.SLens mk_lfun S.SCanonizer mk_q
let pmk_lql   = pmk2 S.SLens mk_lfun S.SCanonizer mk_qfun S.SLens mk_l
let pmk_lrrb  = pmk3 S.SLens mk_lfun S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b

let pmk_qq    = pmk1 S.SCanonizer mk_qfun S.SCanonizer mk_q 
let pmk_qqq   = pmk2 S.SCanonizer mk_qfun S.SCanonizer mk_qfun S.SCanonizer mk_q 
let pmk_qss   = pmk2 S.SCanonizer mk_qfun S.SString mk_sfun S.SString mk_s
let pmk_qll   = pmk2 S.SCanonizer mk_qfun S.SLens mk_lfun S.SLens mk_l
let pmk_qiiq  = pmk3 S.SCanonizer mk_qfun S.SInteger mk_ifun S.SInteger mk_ifun S.SCanonizer mk_q


let prelude_spec =
  [ (* lens operations *)
    pmk_lss    "unsafe_rget"            (fun _ -> L.unsafe_rget)
  ; pmk_lsss   "unsafe_rput"            (fun _ -> L.unsafe_rput)
(*    pmk_lss    "rget"                   (fun _ -> L.rget)
  ; pmk_lsss   "rput"                   (fun _ -> L.rput)
  ; pmk_lss    "rcreate"                (fun _ -> L.rcreate) *)
  ; pmk_lss    "unsafe_rcreate"         (fun _ -> L.unsafe_rcreate)
  ; pmk_ll     "invert"                 L.invert
                                        
  (* core lens combinators *)           
  ; pmk_rl     "copy"                   L.copy
  ; pmk_rssl   "const"                  L.const
  ; pmk_lll    "lens_union"             L.union
  ; pmk_lll    "lens_concat"            L.concat
  ; pmk_liil   "lens_iter"              L.iter
  ; pmk_lll    "lens_swap"              L.swap
  ; pmk_sll    "dmatch"                 (fun i -> L.dmatch i L.std_lookup)
  ; pmk_ssll   "smatch"                 (fun i s -> 
                                            let f = float_of_string (RS.string_of_t s) in 
                                            L.dmatch i (L.sim_lookup f))
  ; pmk_lll    "compose"                L.compose
  ; pmk_lsl    "default"                (fun i l1 s2 -> L.default i s2 l1)
  ; pmk_rl     "key"                    L.key
  ; pmk_llll   "duplicate"              (fun i -> L.duplicate i true)
  ; pmk_llll   "duplicate_snd"          (fun i -> L.duplicate i false)
  ; pmk_rl     "count"                  L.count 
  ; pmk_ll     "forgetkey"              (fun i -> L.forgetkey)
  ; pmk_rrl    "filter"                 L.filter
                                        
  (* canonizer operations *)            
  ; pmk_qqq    "canonizer_union"        C.union
  ; pmk_qqq    "canonizer_concat"       C.concat
  ; pmk_qiiq   "canonizer_iter"         C.iter
  ; pmk_qss    "canonize"               (fun _ -> C.cls)
  ; pmk_qss    "choose"                 (fun _ -> C.rep)
  ; pmk_qll    "left_quot"              L.left_quot
  ; pmk_lql    "right_quot"             L.right_quot
  ; pmk_srssq  "columnize"              C.columnize
                                            
  (* char operations *)                 
  ; pmk_cs     "string_of_char"         (fun _ -> RS.make 1)

  (* char operations *)                 
  ; pmk_iib     "gt"                    (fun _ -> (>))
  ; pmk_iib     "lt"                    (fun _ -> (<))
  ; pmk_iib     "geq"                   (fun _ -> (>=))
  ; pmk_iib     "leq"                   (fun _ -> (<=))
  ; pmk_iii    "plus"                   (fun _ -> (+))
  ; pmk_iii    "minus"                  (fun _ -> (-))
  ; pmk_iii    "times"                  (fun _ x y -> x * y)
  ; pmk_iii    "div"                    (fun _ x y -> x / y)
  ; pmk_iii    "mod"                    (fun _ x y -> x mod y)
                                    
  (* string operations *)               
  ; pmk_sss    "string_concat"          (fun _ -> RS.append)
  ; pmk_ss     "read"                   (fun _ fn -> RS.t_of_string (Misc.read (RS.string_of_t fn)))
                                        
  (* regexp operations *)               
  ; pmk_sr     "str"                    (fun _ -> R.str false)
  ; pmk_r      "empty"                  R.empty
  ; pmk_rb     "is_empty"               (fun _ -> R.is_empty)
  ; pmk_rrr    "regexp_concat"          (fun _ -> R.seq)
  ; pmk_rrr    "regexp_union"           (fun _ -> R.alt)
  ; pmk_rrr    "diff"                   (fun _ -> R.diff)
  ; pmk_rrr    "inter"                  (fun _ -> R.inter)
  ; pmk_riir   "regexp_iter"            (fun _ -> R.iter)
  ; pmk_rrb    "equiv"                  (fun _ -> R.equiv)
  ; pmk_rs     "shortest"               wrap_rep
  ; pmk_rsb    "matches"                (fun _ -> R.match_str)
  ; pmk_rrb    "splittable"             R.splittable
  ; pmk_rb     "iterable"               R.iterable

                                        
  (* boolean operations *)              
  ; pmk_bbb    "land"                   (fun _ -> (&&))
  ; pmk_bbb    "lor"                    (fun _ -> (||))
  ; pmk_bb     "not"                    (fun _ -> not)
                                        
  (* run-time checking *)               
  ; pmk_lrrb   "in_lens_type"           (fun _ l c a -> R.equiv (L.ctype l) c && R.equiv (L.atype l) a)
  ; pmk_lr     "ctype"                  (fun _ -> L.ctype)
  ; pmk_lr     "atype"                  (fun _ -> L.atype)
  ; pmk_lq     "canonizer_of_lens"      L.canonizer_of_t
  ; pmk_rs     "string_of_regexp"       (fun _ r1 -> (RS.t_of_string (R.string_of_t r1)))

  (* polymorphic functions *)
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
