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
(* $Id$                                                                        *)
(*******************************************************************************)

(* module imports and abbreviations *)
module S = Bsyntax
module L = Blenses.DLens
module R = Bregexp
module RS = Bstring

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* run-time values; correspond to each sort *)
type t = 
    | S of Info.t * RS.t 
    | R of Info.t * R.t
    | L of Info.t * L.t
    | F of Info.t * S.sort * (Info.t -> t -> t)

let equal v1 v2 = match v1,v2 with
  | S(_,s1), S(_,s2) -> 
      RS.equal s1 s2
  | R(_,r1), R(_,r2) -> 
      R.equiv r1 r2
  | L _, L _ -> 
      Error.simple_error (sprintf "Cannot test equality of lenses.")
  | F _, F _ -> 
      Error.simple_error (sprintf "Cannot test equality of functions.")
  | _, _ -> 
      Error.simple_error (sprintf "Cannot test equality of values with different sorts.")

let format = function
  | S(_,rs) -> Util.format "%s" (RS.string_of_t rs)
  | R(_,r)  -> Util.format "%s" (R.string_of_t r)
  | L(_,l)  -> Util.format "%s" (L.string l)
  | F(_,_,f)  -> Util.format "<function>"

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
    | S.SString -> S(i,RS.empty)
    | S.SRegexp -> R(i,R.str false RS.empty) 
    | S.SLens -> L(i,(L.copy i (R.epsilon)))
    | S.SFunction(s1,s2) -> F(i,s1,(fun _ _ -> mk_dummy s2))

(* info_of_t : t -> Info.t 
 *
 * [info_of_t t] returns the lexing info associated to a run-time value 
 *)
let info_of_t = function 
  | S(i,_) -> i
  | R(i,_) -> i
  | L(i,_) -> i
  | F(i,_,_) -> i

(* sort_of_t : t -> s
 * 
 * [sort_of_t t] returns the sort of a run-time value; note that for
 * functions, we only compute the argument type 
 *)
let rec sort_of_t = function
  | S(_) -> S.SString
  | R(_) -> S.SRegexp
  | L(_) -> S.SLens
  | F(i,s1,f) -> 
    (* DANGER! Only safe because lambda language is terminating! *)
    S.SFunction(s1, sort_of_t (f i (mk_dummy s1)))


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
  | S(_,s) -> s
  | _ -> conversion_error i S.SString v

(* get_r: t -> Info.t -> L.rx.t
 * 
 * [get_r v i] returns the regexp that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_r v i = match v with
    R(_,r) -> r
  | S(_,s) -> R.str false s
  | _ -> conversion_error i S.SRegexp v

(* get_dl: t -> Info.t -> L.DLens.t
 * 
 * [get_dl v i] returns the D-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_l v i = match v with 
  | S(_,s) -> L.copy i (R.str false s)
  | R(_,r) -> L.copy i r
  | L(_,l) -> l
  | _ -> conversion_error i S.SLens v

(* get_f: t -> Info.t -> (t -> t) 
 * 
 * [get_f v i] returns the [t -> t] function that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_f v i = match v with
  | F(_,_,f) -> f
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
let mk_sfun i f = F(i,S.SString,(fun i v -> f i (get_s v i)))

(* mk_rfun: Info.t -> (L.rx.t -> t) -> (t -> t)
 * 
 * [mk_rfun i f] takes a [L.rx.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be a [SRegexp]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_rfun i f = F(i,S.SRegexp,(fun i v -> f i (get_r v i)))

(* mk_lfun: Info.t -> (L.Lens.t -> t) -> (t -> t)
 * 
 * [mk_lfun i f] takes a [L.Lens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_lfun i f = F(i,S.SLens,(fun i v -> f i (get_l v i)))

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
