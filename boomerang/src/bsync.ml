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

module Diff3 = Bdiff3.Make(
  struct
    type elt = string * string
    let eqv (k1,_) (k2,_) = k1 = k2
    let format (k1,v1) =
      Util.format "%s = %s" (Misc.whack k1) (Misc.whack v1)
  end)
  
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
  Util.format "SKEL_A: ";
  Erx.format_skeleton ska;
  Util.format "@\n";
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
                    | Diff3.Stable((k,oi),(_,ai),(_,bi)) -> 
                       let ty_t = match Erx.box_type ty t with 
                        | None -> assert false
                        | Some ty' -> ty' in 
                       let oi',ai',bi' = isync ty_t oi ai bi in
                       (ot'@[k,oi'],at'@[k,ai'],bt'@[k,bi'])                 
                    | Diff3.AChange(oti,ati,bti) ->
                        if oti = bti then
                          (ot'@ati,at'@ati,bt'@ati)
                        else (ot'@oti,at'@ati,bt'@bti)
                    | Diff3.BChange(oti,ati,bti) ->
                        if oti = ati then
                          (ot'@bti,at'@bti,bt'@bti)
                        else (ot'@oti,at'@ati,bt'@bti)
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
      let erased_t = Erx.erase t in 
      let err_o = 
        if Brx.match_string erased_t o then None 
        else Some ("Archive:\n" ^ Berror.type_error_string (Brx.split_bad_prefix erased_t o)) in 
      let err_a = 
        if Brx.match_string erased_t a then None
        else Some ("Replica A:\n" ^ Berror.type_error_string (Brx.split_bad_prefix erased_t a)) in
      let err_b = 
        if Brx.match_string erased_t b then None
        else Some ("Replica B:\n" ^ Berror.type_error_string (Brx.split_bad_prefix erased_t b)) in
      match err_o,err_a,err_b with
        | None,None,None -> 
            let o',a',b' = isync t o a b in
            (Some o',Some a',Some b')
        | _ -> 
            let get_l = function None -> [] | Some s -> [s] in 
            Berror.run_error (Info.M "In sync") 
              (fun () ->
                 Util.format "@\n";
                 Misc.format_list "@\n@\n" 
                   Berror.nlify
                   (get_l err_o @ get_l err_a @ get_l err_b))
