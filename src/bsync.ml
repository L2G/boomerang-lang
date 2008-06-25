(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                  *)
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

(* --- imports --- *)
let msg = Util.format

module Diff3 = Bdiff3.Make(
  struct
    type elt = string * string
    let equal (k1,_) (k2,_) = k1 = k2
  end)
  
let debug thk = Trace.debug "sync+" thk  
let report thk = thk ()

type act = Added | Deleted 

type dir = A | B | Both

let strings_of_dir = function
  | A  -> "the archive and replica B", "A"
  | B -> "the archive and replica A", "B"
  | Both  -> "replicas A and B", "both"   

type path_elt = Tag of Erx.tag | Key of Erx.key 

let string_of_path_elt = function
  | Tag t -> if t <> "" then ":" ^ (Misc.whack t) else (Misc.whack t)
  | Key k -> "/" ^ (Misc.whack k)
let root = []

let string_of_path p = 
  if p = [] then "." 
  else Safelist.fold_left (fun acc pi -> string_of_path_elt pi ^ acc) "" p

let keys_string kl = Misc.concat_list "," (Safelist.map (fun (ki,_) -> Misc.whack ki) kl)
let tags_string ts = Misc.concat_list "," (Erx.TagSet.fold (fun ti acc -> Misc.whack ti::acc) ts [])

let string_of_dir = function
  | A     -> "A    "
  | B     -> "B    "
  | Both  -> "A & B" 

let string_of_act = function
  | Added   -> "added  "
  | Deleted -> "deleted"

(* reporting *)
let report_spine_conflict spo spa spb p = 
  report 
    (fun () -> 
       msg "@[Conflict on spines [%s] [%s] [%s] at [%s]@]@\n" 
         (Erx.string_of_spine spo) (Erx.string_of_spine spa) (Erx.string_of_spine spb) (string_of_path p))

let report_tags_action r a ts p = 
  Erx.TagSet.iter 
    (fun ti -> 
       report 
         (fun () -> 
            msg "@[%s %s tag at [%s]@]@\n" (string_of_dir r) (string_of_act a) (string_of_path (Tag ti::p))))
    ts

let report_tags_del_mod r rb tso tsr tsrb p = 
  report 
    (fun () -> 
       msg "@[Delete(%s) / modify (%s) conflict on tags {%s} {%s} {%s} at [%s].@]@\n"
         r rb (tags_string tso) (tags_string tsr) (tags_string tsrb) (string_of_path p))

let report_keys_action r a kl p = 
  Safelist.iter 
    (fun (ki,_) -> 
       report 
         (fun () -> 
            msg "@[%s %s key at [%s]@]@\n" (string_of_dir r) (string_of_act a) (string_of_path (Key ki::p))))
    kl

let report_keys_del_mod r rb okl rkl rbkl p = 
  report 
    (fun () -> 
       msg "@[Delete(%s) / modify (%s) conflict on keys [%s] [%s] [%s] at [%s].@]@\n"
         r rb (keys_string okl) (keys_string rkl) (keys_string rbkl) (string_of_path p))

let report_keys_conflict okl akl bkl p = 
  report 
    (fun () -> 
       msg "@[Conflict on keys [%s] [%s] [%s] at [%s].@]@\n"
       (keys_string okl) (keys_string akl) (keys_string bkl) (string_of_path p))

let atomic_sync o a b = 
  if a=b then Some(a,Both)
  else if o=b then Some(a,A)
  else if o=a then Some(b,B)
  else None

let rec isync p ty o a b = 
  let (spo,tmo) as sko = Erx.parse ty o in 
  let (spa,tma) as ska = Erx.parse ty a in
  let (spb,tmb) as skb = Erx.parse ty b in 
  let o',a',b' = 
    begin match atomic_sync spo spa spb with
      | None -> 
          report_spine_conflict spo spa spb p;
          (o,a,b)
      | Some(s,dir) -> begin
          let s_tags = Erx.spine_tags s in
          let o_tags = Erx.spine_tags spo in 
          let a_tags = Erx.spine_tags spa in 
          let b_tags = Erx.spine_tags spb in 
          let r,r_tags,tmr,rb,rb_tags,tmrb = 
            if dir = A then "A",a_tags,tma,"B",b_tags,tmb
            else "B",b_tags,tmb,"A",a_tags,tma in
          (* compute deletes / modified tags *)
          let del_tags = Erx.TagSet.diff rb_tags s_tags in  
          let add_tags = Erx.TagSet.diff r_tags s_tags in 
          let mod_tags = 
            Erx.TagSet.filter 
              (fun ti -> Erx.TagMap.safe_find ti tmrb [] <> Erx.TagMap.safe_find ti tmo [])
              rb_tags in
          let del_mod_tags = Erx.TagSet.inter del_tags mod_tags in 
          (* check for delete / modify conflict *)
            if not (Erx.TagSet.is_empty del_mod_tags) then 
              begin 
                report_tags_del_mod r rb o_tags r_tags rb_tags p;
                (o,a,b)
              end
            else
              begin
                report_tags_action dir Deleted del_tags p;
                report_tags_action dir Added add_tags p;
                let tmo,tma,tmb =
                  Erx.TagSet.fold
                    (fun t (tmo,tma,tmb) ->
                       let pt = Tag t::p in 
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
                                    let oi',ai',bi' = isync (Key k::pt) ty_t oi ai bi in
                                      (ot'@[k,oi'],at'@[k,ai'],bt'@[k,bi'])
                                | Diff3.AChange(oti,ati,bti) ->
                                    if oti = bti then 
                                      begin
                                        report_keys_action A Deleted oti pt;
                                        report_keys_action A Added ati pt;
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end                            
                                    else 
                                      begin                                         
                                        report_keys_del_mod r rb oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.BChange(oti,ati,bti) ->
                                    if oti = ati then 
                                      begin
                                        report_keys_action B Deleted oti pt;
                                        report_keys_action B Added bti pt;
                                        (ot'@bti,at'@bti,bt'@bti)
                                      end                            
                                    else 
                                      begin                                         
                                        report_keys_del_mod r rb oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.Conflict(oti,ati,bti) ->
                                    if ati=bti then 
                                      begin 
                                        report_keys_action Both Deleted oti pt;
                                        report_keys_action Both Added   ati pt;
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end
                                    else
                                      begin
                                        report_keys_conflict oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end)
                           ([],[],[]) chunks in
                         (Erx.TagMap.add t ot' tmo,
                          Erx.TagMap.add t at' tma,
                          Erx.TagMap.add t bt' tmb))
                    (s_tags) (Erx.TagMap.empty,Erx.TagMap.empty,Erx.TagMap.empty) in
                  (* fill in spines with tag maps *)
                  (Erx.unparse (s,tmo),
                   Erx.unparse (s,tma),
                   Erx.unparse (s,tmb))
              end                   
        end 
    end in 
  (o',a',b')
      
let sync t oo ao bo = 
  (* validate archive / replicas *)
  let bare_t = Erx.bare t in 
  let chk m = function
    | None -> None
    | Some w -> 
        if Brx.match_string bare_t w then None
        else Some(m ^ ":\n" 
                    ^ Berror.type_error_string (Brx.split_bad_prefix bare_t w)) in
  let get_l = function None -> [] | Some s -> [s] in 
  let () = match chk "Archive" oo, chk "Replica A" ao, chk "Replica B" bo with
    | None,None,None -> ()
    | err_o,err_a,err_b -> 
        raise (Error.Harmony_error(
          (fun () ->
             msg "@[<2>cannot synchronize invalid structures:@\n";
             Misc.format_list "@\n@\n" Berror.nlify
               (get_l err_o @ get_l err_a @ get_l err_b);
             msg "@]"))) in 
  (* if valid, then sync *)
  match oo,ao,bo with
    | None,_,_
    | _,None,_
    | _,_,None ->
        begin match atomic_sync oo ao bo with
          | None       -> 
              report 
                (fun () -> 
                   msg "@[Conflict at [%s].@]@\n"
                     (string_of_path root));
              (oo,ao,bo)
          | Some (r,d) -> 
              (r,r,r)
        end
    | Some o,Some a,Some b ->
        let o',a',b' = isync root t o a b in
        (Some o',Some a',Some b')
