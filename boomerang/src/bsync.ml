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
(* /boomerang/src/bsync.ml                                                     *)
(* Boomerang synchronization                                                   *)
(* $Id$ *)
(*******************************************************************************)

module Diff3 : sig 
type 'a elt = string * 'a
type 'a seq = ('a elt) list
  
type 'a chunk = 
  | Stable of 'a seq * 'a seq * 'a seq
  | Ins of 'a seq
  | ADel of 'a seq * 'a seq
  | BDel of 'a seq * 'a seq
  | Conflict of 'a seq * 'a seq * 'a seq
val parse : 'a seq -> 'a seq -> 'a seq -> 'a chunk list
end = struct
 type 'a elt = string * 'a
 type 'a seq = ('a elt) list  
 type 'a chunk = 
  | Stable of 'a seq * 'a seq * 'a seq
  | Ins of 'a seq
  | ADel of 'a seq * 'a seq
  | BDel of 'a seq * 'a seq
  | Conflict of 'a seq * 'a seq * 'a seq
 let parse ol al bl =
   let keys l = Safelist.map fst l in
   let ko = keys ol in
   let ka = keys al in
   let kb = keys bl in 
   let res =
     if ko = ka && ka = kb then Stable(ol,al,bl)
     else if ko = ka && kb = [] then BDel(ol,al)
     else if ko = kb && kb = [] then ADel(ol,bl)     
     else if ko = ka && ka = [] then Ins(bl)
     else if ko = kb && kb = [] then Ins(al)
     else Conflict(ol,al,bl) in
  [res]
end

(* move these to Safelist. *)
let rec zip3 =
  let rec aux acc l1 l2 l3 = 
  match l1,l2,l3 with
  | [],[],[] -> Safelist.rev acc
  | (h1::t1),(h2::t2),(h3::t3) -> aux ((h1,h2,h3)::acc) t1 t2 t3
  | _ -> raise (Invalid_argument "zip3") in
  aux []

let unzip3 =
  let rec aux (a1,a2,a3) = function
    | [] -> (Safelist.rev a1,Safelist.rev a2,Safelist.rev a3)
    | (h1,h2,h3)::t -> aux (h1::a1,h2::a2,h3::a3) t in
  aux ([],[],[])

type dir = Left | Right | Both

let atomic_sync o a b = 
  if a=b then Some(a,Both)
  else if o=b then Some(a,Left)
  else if o=a then Some(b,Right)
  else None

let rec isync ty o a b = 
  let (spo,tmo) as sko = Erx.parse ty o in 
  let (spa,tma) as ska = Erx.parse ty a in
  let (spb,tmb) as skb = Erx.parse ty b in 
  begin match atomic_sync spo spa spb with
  | None -> (o,a,b)
  | Some(s,dir) -> begin
      let s_tags = Erx.spine_tags s in
      let del_mod_conflict r tmr =
        Erx.TagSet.exists
          (fun ti -> Erx.TagMap.find ti tmr <> Erx.TagMap.find ti tmo)
          (Erx.TagSet.diff (Erx.spine_tags r) s_tags) in
      if  (dir = Left && del_mod_conflict spb tmb)
       || (dir = Right && del_mod_conflict spa tma)
      then (o,a,b)
      else       
        let tmo,tma,tmb =
          Erx.TagSet.fold
            (fun t (tmo,tma,tmb) ->
               let ot = Erx.box_content sko t in 
               let at = Erx.box_content ska t in 
               let bt = Erx.box_content skb t in 
               let chunks = Diff3.parse ot at bt in
             let ot',at',bt' =
               Safelist.fold_left
                 (fun (ot',at',bt') chunk ->
                    match chunk with
                    | Diff3.Stable(oti,ati,bti) ->
                        let oti',ati',bti' =
                          let res =
                            Safelist.map
                              (fun ((k,oi),(_,ai),(_,bi)) ->
                                 let ty_t = match Erx.box_type ty t with 
                                   | None -> assert false
                                   | Some ty' -> ty' in 
                                 let oi',ai',bi' = isync ty_t oi ai bi in
                                 (k,oi'),(k,ai'),(k,bi'))
                              (zip3 oti ati bti) in
                              unzip3 res in
                              (ot'@oti',at'@ati',bt'@bti')
                    | Diff3.Ins ins_ti ->
                        (ot'@ins_ti,at'@ins_ti,bt'@ins_ti)
                    | Diff3.ADel(oti,bti) ->
                        if oti = bti then (ot',at',bt')
                        else (ot'@oti,at',bt'@bti)
                    | Diff3.BDel(oti,ati) ->
                        if oti = ati then (ot',at',bt')
                        else (ot'@oti,at'@ati,bt')
                    | Diff3.Conflict(oti,ati,bti) ->
                        if ati=bti then (ot'@ati,at'@ati,bt'@ati)
                        else (ot'@oti,at'@ati,bt'@bti))
                 ([],[],[]) chunks in
                 (Erx.TagMap.add t ot' tmo,
                  Erx.TagMap.add t at' tma,
                  Erx.TagMap.add t bt' tmb))
          (Erx.spine_tags s) (Erx.TagMap.empty,Erx.TagMap.empty,Erx.TagMap.empty) in
        (* fill in spines with tag maps *)
        (Erx.unparse (s,tmo),
         Erx.unparse (s,tma),
         Erx.unparse (s,tmb))
    end                   
  end
let sync t oo ao bo = match oo,ao,bo with
  | None,_,_
  | _,None,_
  | _,_,None ->
      begin match atomic_sync oo ao bo with
        | None       -> (oo,ao,bo)
        | Some (r,_) -> r,r,r
      end
  | Some o,Some a,Some b ->
      let err s = Berror.run_error (Info.M "sync") (fun () -> Util.format "%s" s) in 
      match Erx.match_string t o,
        Erx.match_string t a,
        Erx.match_string t b 
      with
        | true,true,true -> 
            let o',a',b' = isync t o a b in
            (Some o',Some a',Some b')
        | false,_,_ -> err ("o invalid " ^ (Erx.string_of_t t))
        | _,false,_ -> err ("a invalid " ^ (Erx.string_of_t t))
        | _,_,false -> err ("b invalid " ^ (Erx.string_of_t t))

