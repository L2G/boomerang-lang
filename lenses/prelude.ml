(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008                                                         *)
(* J. Nathan Foster, and Benjamin C. Pierce                                   *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/lenses/prelude.ml                                               *)
(* OCaml definitions of lens primitives                                       *)
(* $Id$ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Bvalue 
open Bident
module S = Bsyntax
module L = Blenses.DLens
module C = Blenses.Canonizer
module P = Blenses.Permutations
let (^) = Pervasives.(^)
let sprintf = Printf.sprintf 
let msg = Util.format
let (^>) = S.(^>)
let (^*) = S.(^*)

(* ----- helpers ----- *)
let wrap_rep i r = 
  match Brx.representative r with
    | None -> 
        raise (Error.Harmony_error
                 (fun () -> 
                    msg "%s: cannot calculate representative; %s is empty."
                      (Info.string_of_t i)
                      (Brx.string_of_t r)))
    | Some w -> w

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
  let q = Qid.mk_native_prelude_t x in 
  let s = s1 in  
  let i = mk_prelude_info x in 
  let v = mk1 i v1 in 
  (q,s,v)

let pmk1 s1 mk1 s2 mk2 x f = 
  let q = Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (f i v1)) in 
  (q,s,f)
      
let pmk2 s1 mk1 s2 mk2 s3 mk3 x f = 
  let q = Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (f i v1 v2))) in 
  (q,s,f)
      
let pmk3 s1 mk1 s2 mk2 s3 mk3 s4 mk4 x f = 
  let q = Qid.mk_native_prelude_t x in 
  let s = s1 ^> s2 ^> s3 ^> s4 in 
  let i = mk_prelude_info x in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (f i v1 v2 v3)))) in 
  (q,s,f)

let pmk4 s1 mk1 s2 mk2 s3 mk3 s4 mk4 s5 mk5 x f = 
  let q = Qid.mk_native_prelude_t x in 
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
     b : Bol (using bools)
     x : Bol (using string option)
     i : Int
     c : Chr
     s : Str
     r : Rx
     l : Lns
     q : Can
     e : Rel
     p : Par
     v : Vnt (unused)
     f : Fun (unused)
*)

let pmk_cs    = pmk1 S.SChar mk_cfun S.SString mk_s
let pmk_sic   = pmk2 S.SString mk_sfun S.SInteger mk_ifun S.SChar mk_c
let pmk_ci    = pmk1 S.SChar mk_cfun S.SInteger mk_i
let pmk_ic    = pmk1 S.SInteger mk_ifun S.SChar mk_c

let pmk_bb    = pmk1 S.SBool mk_bfun S.SBool mk_b
let pmk_xxx   = pmk2 S.SBool mk_xfun S.SBool mk_xfun S.SBool mk_x 

let pmk_is    = pmk1 S.SInteger mk_ifun S.SString mk_s
let pmk_iib   = pmk2 S.SInteger mk_ifun  S.SInteger mk_ifun S.SBool mk_b
let pmk_iii   = pmk2 S.SInteger mk_ifun S.SInteger mk_ifun S.SInteger mk_i 

let pmk_ss    = pmk1 S.SString mk_sfun S.SString mk_s
let pmk_si    = pmk1 S.SString mk_sfun S.SInteger mk_i
let pmk_sss   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SString mk_s
let pmk_ssu   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SUnit mk_u
let pmk_sr    = pmk1 S.SString mk_sfun S.SRegexp mk_r
let pmk_sll   = pmk2 S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_ssll  = 
  pmk3 S.SString mk_sfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_ircsq = 
  pmk4 S.SInteger mk_ifun S.SRegexp mk_rfun S.SChar mk_cfun S.SString mk_sfun 
    S.SCanonizer mk_q

let pmk_r     = pmk0 S.SRegexp mk_r 
let pmk_rr    = pmk1 S.SRegexp mk_rfun S.SRegexp mk_r
let pmk_rrr   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SRegexp mk_r
let pmk_riir  = 
  pmk3 S.SRegexp mk_rfun S.SInteger mk_ifun S.SInteger mk_ifun S.SRegexp mk_r
let pmk_rb    = pmk1 S.SRegexp mk_rfun S.SBool mk_b
let pmk_rrb   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rs    = pmk1 S.SRegexp mk_rfun S.SString mk_s
let pmk_rx    = pmk1 S.SRegexp mk_rfun S.SBool mk_x
let pmk_rl    = pmk1 S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrl   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrb   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rrs   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SString mk_s
let pmk_rrx   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_x
let pmk_rsx   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_x
let pmk_rsb   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_b
let pmk_rsr   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SRegexp mk_r
let pmk_rsi   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SInteger mk_i
let pmk_rssl  = pmk3 S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SLens mk_l

let pmk_ll    = pmk1 S.SLens mk_lfun S.SLens mk_l
let pmk_lll   = pmk2 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_llll  = 
  pmk3 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_liil  = 
  pmk3 S.SLens mk_lfun S.SInteger mk_ifun S.SInteger mk_ifun S.SLens mk_l
let pmk_lu    = pmk1 S.SLens mk_lfun S.SUnit mk_u
let pmk_lb    = pmk1 S.SLens mk_lfun S.SBool mk_b
let pmk_lr    = pmk1 S.SLens mk_lfun S.SRegexp mk_r
let pmk_lss   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SString mk_s
let pmk_lsl   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SLens mk_l
let pmk_lsss  =
  pmk3 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_s 
let pmk_lq    = pmk1 S.SLens mk_lfun S.SCanonizer mk_q
let pmk_lql   = pmk2 S.SLens mk_lfun S.SCanonizer mk_qfun S.SLens mk_l
let pmk_lrrb  = 
  pmk3 S.SLens mk_lfun S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b

let pmk_qq    = pmk1 S.SCanonizer mk_qfun S.SCanonizer mk_q 
let pmk_qb    = pmk1 S.SCanonizer mk_qfun S.SBool mk_b 
let pmk_qqq   = pmk2 S.SCanonizer mk_qfun S.SCanonizer mk_qfun S.SCanonizer mk_q
let pmk_qr    = pmk1 S.SCanonizer mk_qfun S.SRegexp mk_r
let pmk_rq    = pmk1 S.SRegexp mk_rfun S.SCanonizer mk_q
let pmk_qss   = pmk2 S.SCanonizer mk_qfun S.SString mk_sfun S.SString mk_s
let pmk_qll   = pmk2 S.SCanonizer mk_qfun S.SLens mk_lfun S.SLens mk_l
let pmk_qiiq  = 
  pmk3 S.SCanonizer mk_qfun S.SInteger mk_ifun S.SInteger mk_ifun 
    S.SCanonizer mk_q
let pmk_rrfq  = 
  pmk3 S.SRegexp mk_rfun S.SRegexp mk_rfun (S.SString ^> S.SString) mk_ffun 
    S.SCanonizer mk_q

let pmk_sync  = 
  pmk4 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_sfun 
    (((S.SString ^* S.SString) ^* S.SString) ^* S.SString) (fun i x -> x)

let pmk_dup1  = 
  pmk3 S.SLens mk_lfun (S.SString ^> S.SString) mk_ffun S.SRegexp mk_rfun 
    S.SLens mk_l

let pmk_dup2  = 
  pmk3 (S.SString ^> S.SString) mk_ffun S.SRegexp mk_rfun S.SLens mk_lfun 
    S.SLens mk_l

let pmk_izlzl = 
  pmk2 (S.SData([S.SInteger],list_qid)) mk_listfun 
    (S.SData([S.SLens],list_qid)) mk_listfun S.SLens mk_l 

let pmk_rzq = 
  pmk1 (S.SData([S.SRegexp],list_qid)) mk_listfun S.SCanonizer mk_q

let pmk_izzi =
  pmk1 S.SInteger mk_ifun 
    (S.SData([S.SData([S.SInteger],list_qid)],list_qid)) mk_list

let pmk_zizi =
  pmk1 (S.SData([S.SInteger],list_qid)) mk_listfun 
    (S.SData([S.SInteger],list_qid)) mk_list

let prelude_spec =
  [ (* lens operations *)
    pmk_lss    "get"                  (fun _ -> L.rget)
  ; pmk_lsss   "put"                  (fun _ -> L.rput)
  ; pmk_lss    "create"               (fun _ -> L.rcreate)
  ; pmk_ll     "invert"               L.invert
                                        
  (* core lens combinators *)           
  ; pmk_rl     "copy"                 L.copy
  ; pmk_rssl   "const"                L.const
  ; pmk_lll    "lens_union"           L.union
  ; pmk_lll    "lens_concat"          L.concat
  ; pmk_lll    "lens_swap"            (fun i l1 l2 -> L.permute i [1;0] [l1;l2])
  ; pmk_liil   "lens_iter"            L.iter
  ; pmk_ll     "lens_star"            (fun i l -> L.iter i l 0 (-1))
  ; pmk_ll     "lens_plus"            (fun i l -> L.iter i l 1 (-1))
  ; pmk_ll     "lens_option"          (fun i l -> L.iter i l 0 1)
  ; pmk_izlzl  "lens_permute"         (fun i is ls -> 
                                         L.permute i 
                                           (Safelist.map get_i is) 
                                           (Safelist.map get_l ls))
  ; pmk_sll    "dmatch"               (fun i t l -> L.dmatch i t l)
  ; pmk_ssll   "smatch"               (fun i f t l -> 
                                         L.smatch i (float_of_string f) t l)
  ; pmk_lll    "compose"              L.compose
  ; pmk_lsl    "default"              L.default
  ; pmk_rl     "key"                  L.key
  ; pmk_ll     "forgetkey"            L.forgetkey
  ; pmk_rrl    "filter"               L.filter
  ; pmk_dup1   "dup1"                 (fun i l f fat -> 
                                         L.dup1 i l 
                                           (fun s -> get_s (f (mk_s i s))) fat)
  ; pmk_dup2   "dup2"                 (fun i f fat l -> 
                                         L.dup2 i 
                                           (fun s -> get_s (f (mk_s i s))) fat 
                                           l)
                                      
  (* canonizer operations *)          
  ; pmk_rq     "canonizer_copy"       C.copy
  ; pmk_qqq    "canonizer_union"      C.union
  ; pmk_qqq    "canonizer_concat"     C.concat
  ; pmk_qiiq   "canonizer_iter"       C.iter
  ; pmk_qss    "canonize"             (fun _ -> C.canonize)
  ; pmk_qss    "choose"               (fun _ -> C.choose)
  ; pmk_qll    "left_quot"            L.left_quot
  ; pmk_lql    "right_quot"           L.right_quot
  ; pmk_ircsq  "columnize"            C.columnize
  ; pmk_rrfq   "normalize"            (fun i ct ct0 f -> 
                                         C.normalize i ct ct0 
                                           (fun s -> get_s (f (mk_s i s))))
  ; pmk_rzq    "sort"                 (fun i rl -> C.sort i 
                                         (Safelist.map get_r rl))          
  ; pmk_qr     "uncanonized_type"     (fun _ -> C.uncanonized_type)
  ; pmk_qr     "canonized_type"       (fun _ -> C.canonized_type)
                                
  (* char operations *)               
  ; pmk_cs     "string_of_char"       (fun _ -> String.make 1)
  ; pmk_ic     "chr"                  (fun _ -> Char.chr)
  ; pmk_ci     "code"                 (fun _ -> Char.code)

  (* int operations *)
  ; pmk_is     "string_of_int"        (fun _ -> string_of_int)
  ; pmk_izzi   "permutations"         (fun i k -> 
                                         Safelist.map 
                                           (fun l -> mk_list i 
                                              (Safelist.map (mk_i i) l))
                                           (P.permutations k))
  ; pmk_zizi   "invert_permutation"   (fun i sigma -> 
                                         Safelist.map (mk_i i) 
                                           (P.invert_permutation 
                                              (Safelist.map get_i sigma)))

  (* arithmetic operations *)               
  ; pmk_iib     "gt"                  (fun _ -> (>))
  ; pmk_iib     "lt"                  (fun _ -> (<))
  ; pmk_iib     "geq"                 (fun _ -> (>=))
  ; pmk_iib     "leq"                 (fun _ -> (<=))
  ; pmk_iii    "plus"                 (fun _ -> (+))
  ; pmk_iii    "minus"                (fun _ -> (-))
  ; pmk_iii    "times"                (fun _ x y -> x * y)
  ; pmk_iii    "div"                  (fun _ x y -> x / y)
  ; pmk_iii    "mod"                  (fun _ x y -> x mod y)
                                    
  (* string operations *)             
  ; pmk_sss    "string_concat"        (fun _ -> (^))    
  ; pmk_si     "length"               (fun _ -> String.length)
  ; pmk_sic    "get_char"             (fun _ -> String.get)
  ; pmk_ss     "read"                 (fun _ fn -> Misc.read fn)
  ; pmk_ssu    "write"                (fun _ fn s -> Misc.write fn s)
                                      
  (* regexp operations *)             
  ; pmk_sr     "str"                  (fun _ -> Brx.mk_string)
  ; pmk_r      "empty"                Brx.empty
  ; pmk_rb     "is_empty"             (fun _ -> Brx.is_empty)
  ; pmk_rrr    "regexp_concat"        (fun _ -> Brx.mk_seq)
  ; pmk_rrr    "regexp_union"         (fun _ -> Brx.mk_alt)
  ; pmk_rrr    "diff"                 (fun _ -> Brx.mk_diff)
  ; pmk_rrr    "inter"                (fun _ -> Brx.mk_inter)
  ; pmk_riir   "regexp_iter"          (fun i -> Brx.mk_iter)
  ; pmk_rrb    "equiv"                (fun _ -> Brx.equiv)
  ; pmk_rrx    "equiv_cex"            (fun _ t1 t2 -> 
                                         if Brx.equiv t1 t2 then None
                                         else match Brx.representative (Brx.mk_diff t1 t2) with 
                                           | Some w1 -> Some(sprintf "%s and %s are not equivalent; [%s] is in the first but not the second" 
                                                               (Brx.string_of_t t1) (Brx.string_of_t t2) w1)
                                           | None -> match Brx.representative (Brx.mk_diff t2 t1) with 
                                               | Some w2 -> Some(sprintf "%s and %s are not equivalent; [%s] is in the second but not the first" 
                                                                   (Brx.string_of_t t1) (Brx.string_of_t t2) w2)
                                               | None -> raise (Error.Harmony_error (fun () -> msg "equiv_cex: cannot calculate representative")))
  ; pmk_rs     "representative"       wrap_rep
  ; pmk_rrx    "fast_splittable"      (fun _ t1 t2 ->
                                         match Brx.splittable_cex t1 t2 with
                                           | Misc.Left(w1,w2,w1',w2') -> 
                                               Some (sprintf "%s is ambiguously splittable into [%s] [%s] and [%s] [%s]" 
                                                       (w1^w2) w1 w2 w1' w2')
                                         | _ -> None)
  ; pmk_rr     "reverse"              (fun _ -> Brx.mk_reverse)
  ; pmk_rsi    "count"                (fun i r s -> 
                                         Safelist.length (Brx.star_split r s))
  ; pmk_rsx    "matches_cex"          (fun _ r s -> 
                                         if Brx.match_string r s then None
                                         else 
                                           let s1,s2 = Brx.split_bad_prefix r s in 
                                           Some (sprintf "string does not match %s [%s] AROUND HERE [%s]" (Brx.string_of_t r) s1 s2))
  ; pmk_rsb    "matches"              (fun _ r s -> Brx.match_string r s)
  ; pmk_rrb    "splittable"           (fun _ -> Brx.splittable)
  ; pmk_rrx    "splittable_cex"       (fun _ t1 t2 ->
                                         match Brx.splittable_cex t1 t2 with
                                           | Misc.Left(w1,w2,w1',w2') -> 
                                               Some (sprintf "%s is ambiguously splittable into [%s] [%s] and [%s] [%s]" 
                                                       (w1^w2) w1 w2 w1' w2')
                                         | _ -> None)
  ; pmk_rb     "iterable"             (fun _ -> Brx.iterable)
  ; pmk_rx     "iterable_cex"         (fun _ t -> match Brx.iterable_cex t with
                                         | Misc.Left(w1,w2,w1',w2') -> 
                                             Some (sprintf "%s is ambiguously iterable: splits into [%s] [%s] and [%s] [%s]" 
                                                     (w1^w2) w1 w2 w1' w2')
                                         | _ -> None)
  ; pmk_rrb    "disjoint"             (fun _ -> Brx.disjoint)
  ; pmk_rrx    "disjoint_cex"         (fun _ t1 t2 -> 
                                         match Brx.disjoint_cex t1 t2 with
					   | Some(cex) ->   
					       Some (sprintf "%s and %s are not disjoint: %s is in the intersection" 
                                                       (Brx.string_of_t t1) 
                                                       (Brx.string_of_t t2) cex)
					   | None -> None)
  ; pmk_rsr    "derivative"           (fun _ t1 s -> Brx.derivative t1 s)
    
  (* boolean operations *)            
  ; pmk_xxx    "land"                 (fun _ x1 x2 -> 
					 match x1,x2 with
					     None,None -> None
					   | None,Some cex
					   | Some cex,None -> Some cex
					   | Some cex1,Some cex2 -> 
                                               Some(sprintf "[%s] and [%s]"
                                                      cex1 cex2))
  ; pmk_xxx    "lor"                  (fun _ x1 x2 ->
				         match x1,x2 with
					   | Some cex1,Some cex2 -> 
                                               Some (sprintf "[%s] and [%s]"
                                                       cex1 cex2)
                                           | _ -> None)
  ; pmk_bb     "not"                  (fun _ -> not)
                                      
  (* run-time checking *)             
  ; pmk_lr     "ctype"                (fun _ -> L.ctype)
  ; pmk_lr     "atype"                (fun _ -> L.atype)
  ; pmk_lb     "bij"                  (fun _ -> L.bij)

  ; pmk_lq     "canonizer_of_lens"    L.canonizer_of_t
  ; pmk_lb     "crel_identity"        (fun _ -> L.crel_identity)
  ; pmk_lb     "arel_identity"        (fun _ -> L.arel_identity)
  ; pmk_qb     "cnrel_identity"       (fun _ -> C.cnrel_identity)

  ; pmk_rs     "string_of_regexp"     (fun _ r1 -> Brx.string_of_t r1)

  (* sync *)
  ; pmk_sync   "sync"                 (fun i l o a b -> 
                                         let mk_s s = Bvalue.Str(i,s) in 
                                         let mk_p v1 v2 = Bvalue.Par(i,v1,v2) in
                                         let xt = match L.xtype l with 
                                           | Some xt -> xt
                                           | None -> 
                                               raise 
                                                 (Error.Harmony_error
                                                    (fun () -> 
                                                       msg "%s: cannot synchronize with %s."
                                                         (Info.string_of_t i)
                                                         (L.string l))) in 
                                         let acts,o',a',b' = 
                                           Bsync.sync xt 
                                             (L.rget l o) 
                                             (L.rget l a) 
                                             (L.rget l b) in 
                                         let s_acts = mk_s acts in
                                         let s_o = mk_s (L.rput l o' o) in 
                                         let s_a = mk_s (L.rput l a' a) in 
                                         let s_b = mk_s (L.rput l b' b) in 
                                         mk_p (mk_p (mk_p s_acts s_o) s_a) s_b)

  (* polymorphic functions *)
  ; begin 
    let i = Info.M "poly_equal built-in" in 
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    (Qid.mk_native_prelude_t "poly_equal",
     S.SForall(a,a_sort ^> a_sort ^> S.SBool),
     mk_ufun i (fun () -> 
       mk_f i (fun v1 -> 
         mk_f i (fun v2 -> 
           mk_b i (equal v1 v2)))))
   end
 
  ; begin 
    let i = Info.M "fold_left built-in" in 
    let nil = Id.mk (Info.M "Nil built-in") "Nil" in 
    let cons = Id.mk (Info.M "Cons built-in!") "Cons" in 
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    let b = Id.mk i "b" in 
    let b_sort = S.SVar b in 
    let b_list_sort = S.SData([b_sort],Qid.mk_list_t "t") in      
    (Qid.mk_native_prelude_t "fold_left",
     S.SForall(a,S.SForall(b, ((a_sort ^> b_sort ^> a_sort) ^> 
                                 a_sort ^> b_list_sort ^> a_sort))),
     let b = mk_prelude_info "fold_left" in 
     mk_f b (fun achk -> 
       mk_f b (fun bchk -> 
         mk_f b (fun f0 -> 
           mk_f b (fun acc0 -> 
             mk_f b (fun l0 ->
               let f = get_f f0 in 
               let rec aux l acc = 
               let lbl,vo = get_v l in 
               if Id.equal lbl nil then acc
               else if Id.equal lbl cons then 
                 let hd,tl = match vo with 
                   | None -> poly_error b b_list_sort l
                   | Some v -> get_p v in                
                 let acc' = (get_f (f acc)) hd in 
                   aux tl acc'
               else 
                 poly_error b b_list_sort l in 
                 aux l0 acc0))))))
  end 

  ; begin
    let i = mk_prelude_info "valid_permutation" in 
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    let int_list_sort = S.SData([S.SInteger],Qid.mk_list_t "t") in
    let a_list_sort = S.SData([a_sort],Qid.mk_list_t "t") in      
    (Qid.mk_native_prelude_t "valid_permutation",
     S.SForall(a,int_list_sort ^> a_list_sort ^> S.SBool),
     mk_f i (fun achk -> 
       mk_f i (fun sigma -> 
           mk_f i (fun l0 -> 
	     let sigma = Safelist.map get_i (get_list sigma) in
	       mk_b i (P.valid_permutation sigma 
                         (get_list l0))))))
  end

  ; begin
    let i = mk_prelude_info "list_permute" in
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    let int_list_sort = S.SData([S.SInteger],Qid.mk_list_t "t") in
    let a_list_sort = S.SData([a_sort],Qid.mk_list_t "t") in
    (Qid.mk_native_prelude_t "list_permute",
     S.SForall(a,int_list_sort ^> a_list_sort ^> a_list_sort),
     mk_f i (fun achk -> 
       mk_f i (fun sigma -> 
           mk_f i (fun l0 -> 
	     let sigma = Safelist.map get_i (get_list sigma) in
	       mk_list i (P.permute_list sigma 
                            (get_list l0))))))
  end
  ]
    
let () = 
  Safelist.iter 
    (fun (x,s,v) -> Bregistry.register_native_qid x s v)
    prelude_spec
