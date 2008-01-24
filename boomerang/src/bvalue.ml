(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
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
(* /boomerang/src/bvalue.ml                                                    *)
(* Boomerang run-time values                                                   *)
(* $Id$ *)
(*******************************************************************************)

(* module imports and abbreviations *)
module S = Bsyntax
module R = Bregexp
module L = Blenses.DLens
module C = Blenses.Canonizer
module RS = Bstring

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* run-time values; correspond to each sort *)
type t = 
    | Str of Info.t * RS.t 
    | Rx of Info.t * R.t
    | Lns of Info.t * L.t
    | Can of Info.t * C.t
    | Fun of Info.t * S.sort * (Info.t -> t -> t)    
    | Unt of Info.t
    | Par of Info.t * t * t
    | Vnt of Info.t * S.qid * string * t option

let rec equal v1 v2 = match v1,v2 with
  | Str(_,s1), Str(_,s2) -> 
      RS.equal s1 s2
  | Rx(_,r1), Rx(_,r2) -> 
      R.equiv r1 r2
  | Lns _, Lns _ -> 
      Error.simple_error (sprintf "Cannot test equality of lenses.")
  | Can _, Can _ -> 
      Error.simple_error (sprintf "Cannot test equality of canonizers.")
  | Fun _, Fun _ -> 
      Error.simple_error (sprintf "Cannot test equality of functions.")
  | Unt _, Unt _ -> true
  | Par(_,v1,v2),Par(_,v1',v2') -> 
      (equal v1 v1') && (equal v2 v2')
  | Vnt(_,_,l,None), Vnt(_,_,l',None) -> l=l'
  | Vnt(_,_,l,Some v), Vnt(_,_,l',Some v') ->
      (l=l') && (equal v v')
  | Vnt _,Vnt _ -> false
  | _, _ -> 
      Error.simple_error (sprintf "Cannot test equality of values with different sorts.")

let rec format = function
  | Str(_,rs)    -> Util.format "%s" (RS.string_of_t rs)
  | Rx(_,r)      -> Util.format "%s" (R.string_of_t r)
  | Lns(_,l)     -> Util.format "%s" (L.string l)
  | Can(_,c)     -> Util.format "%s" (C.string c)
  | Fun(_,_,f)   -> Util.format "<function>"
  | Unt(_)       -> Util.format "()"
  | Par(_,v1,v2) -> 
      Util.format "@[(";
      format v1;
      Util.format ",@ ";
      format v2;
      Util.format ")@]"
  | Vnt(_,_,l,vo)    -> 
      Util.format "@[(%s@ " l;
      (match vo with None -> () | Some v -> format v);
      Util.format "@]"        

(* mk_dummy: s -> t
 * 
 * make a dummy run-time value from a sort.
 * 
 * note that in the function case, the constructed function is a
 * constant raising an exception when the result type is not known 
 *)
let rec mk_dummy = 
  let i = Info.M "dummy" in 
  function
    | S.SString          -> Str(i,RS.empty)
    | S.SRegexp          -> Rx(i,R.str false RS.empty) 
    | S.SLens            -> Lns(i,(L.copy i (R.epsilon)))
    | S.SCanonizer       -> Can(i,L.canonizer_of_t i (L.copy i R.epsilon))
    | S.SFunction(s1,s2) -> Fun(i,s1,(fun _ _ -> mk_dummy s2))
    | S.SProduct(s1,s2)  -> Par(i,mk_dummy s1,mk_dummy s2)
    | S.SUnit            -> Unt i
    | S.SSum(vl)         -> 
        Error.simple_error (sprintf "Cannot make dummy for sum")
    | S.SVar(_)          -> 
        Error.simple_error (sprintf "Cannot make dummy for type variable")
        
(* info_of_t : t -> Info.t 
 *
 * [info_of_t t] returns the lexing info associated to a run-time value 
 *)
let info_of_t = function 
  | Str(i,_)     -> i
  | Rx(i,_)      -> i
  | Lns(i,_)     -> i
  | Can(i,_)     -> i
  | Fun(i,_,_)   -> i
  | Unt(i)       -> i
  | Par(i,_,_)   -> i
  | Vnt(i,_,_,_) -> i

(* sort_of_t : t -> s
 * 
 * [sort_of_t t] returns the sort of a run-time value; note that for
 * functions, we only compute the argument type 
 *)
let rec sort_of_t = function
  | Str(_) -> S.SString
  | Rx(_)  -> S.SRegexp
  | Lns(_) -> S.SLens
  | Can(_) -> S.SCanonizer
  | Fun(i,s1,f) -> 
    (* DANGER! Only safe because lambda language is terminating! *)
    S.SFunction(s1, sort_of_t (f i (mk_dummy s1)))
  | Unt(_)        -> S.SUnit
  | Par(_,v1,v2)  -> S.SProduct(sort_of_t v1,sort_of_t v2)
  | Vnt(_,q,_,_) -> S.SVar(q)

(* --------- conversions between run-time values ---------- *)
let conversion_error i s1 v1 = 
  Error.simple_error 
    (sprintf "%s: expected %s, but found %s" 
        (Info.string_of_t i) 
        (S.string_of_sort s1)
        (S.string_of_sort (sort_of_t v1)))

(* get_s: t -> Info.t -> string
 * 
 * [get_s v i] returns the string that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.
*)
let get_s v i = match v with
  | Str(_,s) -> s
  | _ -> conversion_error i S.SString v

(* get_r: t -> Info.t -> L.rx.t
 * 
 * [get_r v i] returns the regexp that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_r v i = match v with
    Rx(_,r)  -> r
  | Str(_,s) -> R.str false s
  | _ -> conversion_error i S.SRegexp v

(* get_l: t -> Info.t -> L.DLens.t
 * 
 * [get_l v i] returns the D-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_l v i = match v with 
  | Str(_,s) -> L.copy i (R.str false s)
  | Rx(_,r)  -> L.copy i r
  | Lns(_,l) -> l
  | _ -> conversion_error i S.SLens v

(* get_c: t -> Info.t -> C.t
 * 
 * [get_dl v i] returns the canonizer that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors. *)
let get_c v i = match v with 
  | Can(_,c) -> c
  | _ -> conversion_error i S.SCanonizer v

(* get_f: t -> Info.t -> (t -> t) 
 * 
 * [get_f v i] returns the [t -> t] function that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_f v i = match v with
  | Fun(_,_,f) -> f
  | _ ->
      Error.simple_error 
        (sprintf "%s: expected function, but found %s" 
            (Info.string_of_t i) 
            (S.string_of_sort (sort_of_t v)))
        
(* --------- constructors for functions on run-time values ---------- *)

(* mk_sfun: Info.t -> (RS.t -> t) -> (t -> t)
 * 
 * [mk_sfun i f] takes a [RS.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SString]. [i] is
 * used to report errors when the argument has a different sort. 
 *)
let mk_sfun i f = Fun(i,S.SString,(fun i v -> f i (get_s v i)))

(* mk_rfun: Info.t -> (L.rx.t -> t) -> (t -> t)
 * 
 * [mk_rfun i f] takes a [L.rx.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be a [SRegexp]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_rfun i f = Fun(i,S.SRegexp,(fun i v -> f i (get_r v i)))

(* mk_lfun: Info.t -> (L.Lens.t -> t) -> (t -> t)
 * 
 * [mk_lfun i f] takes a [L.Lens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_lfun i f = Fun(i,S.SLens,(fun i v -> f i (get_l v i)))

(* mk_cfun: Info.t -> (C.t -> t) -> (t -> t)
 * 
 * [mk_cfun i f] takes a [C.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SCanonizer]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_cfun i f = Fun(i,S.SCanonizer,(fun i v -> f i (get_c v i)))

let parse_qid s = 
  let lexbuf = Lexing.from_string s in
    Blexer.setup "qid constant";
    let q = 
      try Bparser.qid Blexer.main lexbuf
      with 
        | _ -> 
            raise 
              (Error.Harmony_error
                  (fun () -> 
                    Util.format "%s: syntax error in qualfied identifier %s." 
                      (Info.string_of_t (Blexer.info lexbuf))
                      s)) in 
      Blexer.finish ();                    
      q
