(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
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
(* /boomerang/src/bvalue.ml                                                   *)
(* Boomerang run-time values                                                  *)
(* $Id$ *)
(******************************************************************************)

(* module imports and abbreviations *)
open Bident
module L = Blenses.DLens
module C = Blenses.Canonizer

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* run-time values; correspond to each sort *)
type t = 
  | Unt of Info.t 
  | Bol of Info.t * string option (* None = true ; 
                                     Some s = false, w/ counterexample s *)
  | Int of Info.t * int
  | Chr of Info.t * char
  | Str of Info.t * string
  | Rx  of Info.t * Brx.t
  | Lns of Info.t * L.t
  | Can of Info.t * C.t
  | Fun of Info.t * (t -> t)
  | Par of Info.t * t * t
  | Vnt of Info.t * Qid.t * Id.t * t option

let info_of_t = function
  | Unt(i)       -> i
  | Int(i,_)     -> i
  | Bol(i,_)     -> i
  | Chr(i,_)     -> i
  | Str(i,_)     -> i
  |  Rx(i,_)     -> i
  | Lns(i,_)     -> i
  | Can(i,_)     -> i
  | Fun(i,_)     -> i
  | Par(i,_,_)   -> i
  | Vnt(i,_,_,_) -> i         

let rec equal v1 v2 = match v1,v2 with
  | Unt _, Unt _ -> 
      true
  | Bol(_,b1), Bol(_,b2) -> 
      begin match b1,b2 with
	| None,None -> true
	| Some _, Some _ -> true
	| _ -> false
      end
  | Int(_,n1), Int(_,n2) -> 
      n1=n2
  | Chr(_,c1), Chr(_,c2) -> 
      c1=c2
  | Chr(_,c), Str(_,s) | Str(_,s), Chr(_,c) -> 
      (String.make 1 c) = s
  | Chr(_,c), Rx(_,r) | Rx(_,r), Chr(_,c) -> 
      Brx.equiv (Brx.mk_string (String.make 1 c)) r
  | Str(_,s1), Str(_,s2) -> 
      s1 = s2  
  | Str(_,s), Rx(_,r) | Rx(_,r), Str(_,s) -> 
      Brx.equiv (Brx.mk_string s) r
  | Rx(_,r1), Rx(_,r2) -> 
      Brx.equiv r1 r2
  | Lns _, Lns _ -> 
      Error.simple_error (sprintf "Cannot test equality of lenses.")
  | Can _, Can _ -> 
      Error.simple_error (sprintf "Cannot test equality of canonizers.")
  | Fun _, Fun _ -> 
      Error.simple_error (sprintf "Cannot test equality of functions.")
  | Par(_,v1,v2),Par(_,v1',v2') -> 
      (equal v1 v1') && (equal v2 v2')
  | Vnt(_,qx,l,None), Vnt(_,qx',l',None) -> 
      Qid.equal qx qx' && Id.equal l l'
  | Vnt(_,qx,l,Some v), Vnt(_,qx',l',Some v') ->
      (Qid.equal qx qx') && (Id.equal l l') && (equal v v')
  | Vnt _,Vnt _ -> false
  | _, _ -> 
      format v1; Util.format "@\n"; 
      format v2; Util.format "@\n";
      Error.simple_error (sprintf "Cannot test equality of values with different sorts.")

and format = function
  | Unt(_)       -> Util.format "()"
  | Int(_,n)     -> Util.format "%d" n
  | Bol(_,None)  -> Util.format "true"
  | Bol(_,Some "") ->
      Util.format "false"
  | Bol(_,Some s) -> 
      Util.format "false (with counterexample: %s)" s
  | Chr(_,c)     -> Util.format "'%s'" (Char.escaped c)
  | Str(_,rs)    -> Util.format "\"%s\"" rs
  | Rx(_,r)      -> Util.format "%s" (Brx.string_of_t r)
  | Lns(_,l)     -> Util.format "%s (lens)" (L.string l)
  | Can(_,c)     -> Util.format "%s (canonizer)" (C.string c)
  | Fun(_,f)     -> Util.format "<function>"
  | Par(_,v1,v2) -> 
      Util.format "@[(";
      format v1;
      Util.format ",@ ";
      format v2;
      Util.format ")@]"
  | Vnt(_,_,l,None) -> Util.format "%s" (Id.string_of_t l)
  | Vnt(_,_,l,Some v) ->  
      Util.format "@[(%s@ " (Id.string_of_t l);
      format v;
      Util.format ")@]"        

let string_of_t v = Util.format_to_string (fun () -> format v)
        
let rec sort_string_of_t = function
  | Unt _ -> "unit"
  | Bol _ -> "bool" 
  | Int _ -> "int" 
  | Chr _ -> "char"
  | Str _ -> "string"
  | Rx _  -> "regexp"
  | Lns _ -> "lens"
  | Can _ -> "canonizer"
  | Fun _ -> "<function>"
  | Par(_,v1,v2) -> sprintf "(%s,%s)" (sort_string_of_t v1) (sort_string_of_t v2)
  | Vnt(_,qx,_,_) -> sprintf "%s" (Qid.string_of_t qx)

(* --------- conversions between run-time values ---------- *)
let conversion_error s1 v1 = 
  Error.simple_error 
    (sprintf "%s: Conversion error; expected %s, but found %s" 
        (Info.string_of_t (info_of_t v1)) 
        s1
        (string_of_t v1))

let get_u v = match v with
  | Unt(_) -> ()
  | _ -> conversion_error "unit" v

let get_b v = match v with 
  | Bol(_,None) -> true
  | Bol(_,Some _) -> false
  | _ -> conversion_error "boolean" v

let get_x v = match v with
  | Bol(_,None) -> None
  | Bol(_,Some s) -> Some s
  | _ -> conversion_error "boolean" v

let get_i v = match v with
  | Int(_,n) -> n
  | _ -> conversion_error "integer" v

let get_c v = match v with
  | Chr(b,c) -> c
  | _ -> conversion_error "char" v

let get_s v = match v with
  | Str(_,s) -> s
  | _ -> conversion_error "string" v

let get_r v = match v with
    Rx(_,r)  -> r
  | _ -> conversion_error "regexp" v
      
let get_l v = match v with 
  | Lns(_,l) -> l
  | _ -> conversion_error "lens" v

let get_q v = match v with 
  | Can(_,q) -> q
  | _ -> conversion_error "canonizer" v

let get_p v = match v with
  | Par(_,v1,v2) -> (v1,v2)
  | _ -> conversion_error "pair" v

let get_v v = match v with
  | Vnt(_,_,l,v) -> (l,v)
  | _ -> conversion_error "variant" v

let get_f v = match v with
  | Fun(_,f) -> f
  | _ -> conversion_error "function" v

(* --------- constructors for functions on run-time values ---------- *)
let mk_u i u = Unt(i)
let mk_b i b = if b then Bol(i,None) else Bol(i,Some "")
let mk_x i x = Bol(i,x)
let mk_i i n = Int(i,n)
let mk_c i c = Chr(i,c)
let mk_s i s = Str(i,s)
let mk_r i r = Rx(i,r)
let mk_l i l = Lns(i,l)
let mk_q i q = Can(i,q)
let mk_p i (p1,p2) = Par(i,p1,p2)
let mk_f i f = Fun(i,f)

let mk_ufun i f = Fun(i,(fun v -> f (get_u v)))
let mk_bfun i f = Fun(i,(fun v -> f (get_b v)))
let mk_xfun i f = Fun(i,(fun v -> f (get_x v)))
let mk_ifun i f = Fun(i,(fun v -> f (get_i v)))
let mk_cfun i f = Fun(i,(fun v -> f (get_c v)))
let mk_sfun i f = Fun(i,(fun v -> f (get_s v)))
let mk_rfun i f = Fun(i,(fun v -> f (get_r v)))
let mk_lfun i f = Fun(i,(fun v -> f (get_l v)))
let mk_qfun i f = Fun(i,(fun v -> f (get_q v)))
let mk_pfun i f = Fun(i,(fun v -> f (get_p v)))
let mk_vfun i f = Fun(i,(fun v -> f (get_v v)))
let mk_ffun i f = Fun(i,(fun v -> f (get_f v)))

let string_of_t v = Util.format_to_string (fun () -> format v)

(* list utilities *)
let nil = Id.mk (Info.M "Nil built-in") "Nil"
let cons = Id.mk (Info.M "Cons built-in") "Cons"
let list_qid =
  let i = Info.M "List.t built-in" in
  Qid.mk [Id.mk i "List"] (Id.mk i "t")
 
let get_list v = 
  let rec aux v acc = 
    let li,vio = get_v v in 
    if Id.equal li nil then Safelist.rev acc 
    else if Id.equal li cons then 
      let vh,vrest = match vio with 
        | None -> conversion_error "list" v
        | Some vi -> get_p vi in
      aux vrest (vh::acc)
    else conversion_error "list" v in 
  aux v []

let mk_list i l = 
  let rec aux l v = match l with
    | [] -> v 
    | h::rest -> aux rest (Vnt(i,list_qid,cons,Some (mk_p i (h,v)))) in 
  aux (Safelist.rev l) (Vnt(i,list_qid,nil,None))
        

let mk_listfun i f = 
  Fun(i,(fun v -> f (get_list v)))
