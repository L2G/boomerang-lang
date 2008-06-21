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
    let eqv (k1,_) (k2,_) = k1 = k2
    let format (k1,v1) =
      msg "%s = %s" (Misc.whack k1) (Misc.whack v1)
  end)
  
let debug thk = Trace.debug "sync+" thk  
let report thk = Trace.debug "sync" thk

type dir = Left | Right | Both

let strings_of_dir = function
  | Left  -> "the archive and replica B", "A"
  | Right -> "the archive and replica A", "B"
  | Both  -> "replicas A and B", "both"   

let atomic_sync o a b = 
  if a=b then Some(a,Both)
  else if o=b then Some(a,Left)
  else if o=a then Some(b,Right)
  else None

let rec isync p ty o a b = 
  let string_of_path p = Misc.concat_list "" (Safelist.rev p) in
  let (spo,tmo) as sko = Erx.parse ty o in 
  let (spa,tma) as ska = Erx.parse ty a in
  let (spb,tmb) as skb = Erx.parse ty b in 
  let o',a',b' = 
    begin match atomic_sync spo spa spb with
      | None -> 
          report 
            (fun () -> 
               msg "@[Conflict at [%s]:" (string_of_path p);
               msg "@ returning the archive and replicas unchanged.@]@\n");
          (o,a,b)
      | Some(s,dir) -> begin
          let s_tags = Erx.spine_tags s in
          let a_tags = Erx.spine_tags spa in 
          let b_tags = Erx.spine_tags spb in 
          let r,r_tags,tmr,rb,rb_tags,tmrb = 
            if dir = Left then "A",a_tags,tma,"B",b_tags,tmb
            else "B",b_tags,tmb,"A",a_tags,tma in
          let del_tags = Erx.TagSet.diff r_tags s_tags in 
          let mod_tags = 
            Erx.TagSet.filter 
              (fun ti -> Erx.TagMap.find ti tmrb <> Erx.TagMap.find ti tmo)
              rb_tags in
          let del_mod_tags = Erx.TagSet.inter del_tags mod_tags in 
          (* check for delete / modify conflict *)
            if not (Erx.TagSet.is_empty del_mod_tags) then 
              begin 
                let ti = Erx.TagSet.choose del_mod_tags in 
                report 
                  (fun () -> 
                     msg "@[Delete (%s) / modify (%s) conflict on chunks in %s at [%s]:" r rb ti (string_of_path p);
                     msg "@ returning the archive and replicas unchanged.@]@\n");
                (o,a,b)
              end
            else       
              if Erx.TagSet.is_empty s_tags then 
                begin
                  let s1,s2 = strings_of_dir dir in 
                  report (fun () -> msg "At [%s]: %s are identical; propagating %s.@\n" (string_of_path p) s1 s2);
                  let w = Erx.unparse (s,Erx.TagMap.empty) in
                  (w,w,w)
                end
              else
                let tmo,tma,tmb =
                  Erx.TagSet.fold
                    (fun t (tmo,tma,tmb) ->
                       let pt = (":" ^ Misc.whack t)::p in 
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
                                    let oi',ai',bi' = isync (("/" ^ Misc.whack k)::pt) ty_t oi ai bi in
                                    (ot'@[k,oi'],at'@[k,ai'],bt'@[k,bi'])
                                | Diff3.AChange(oti,ati,bti) ->
                                    if oti = bti then 
                                      begin
                                        report 
                                          (fun () -> 
                                             msg "@[At [%s] the archive and replica B are identical;" (string_of_path pt);
                                             msg "@ propagating A.@]@\n");
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end
                                    else 
                                      begin 
                                        report 
                                          (fun () -> 
                                             msg "@[Conflict at [%s]: the archive and replica B are not identical;" (string_of_path pt);
                                             msg "@ returning the archive and replicas unchanged.@]@\n");
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.BChange(oti,ati,bti) ->
                                    if oti = ati then 
                                      begin
                                        report 
                                          (fun () -> 
                                             msg "@[At [%s] the archive and replica B are identical;" (string_of_path pt);
                                             msg "@ propagating A.@]@\n");
                                        (ot'@bti,at'@bti,bt'@bti)
                                      end
                                    else 
                                      begin 
                                        report 
                                          (fun () -> 
                                             msg "@[Conflict at [%s]: the archive and replica B are not identical;" (string_of_path pt);
                                             msg "@ returning the archive and replicas unchanged.@]@\n");
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.Conflict(oti,ati,bti) ->
                                    if ati=bti then 
                                      begin 
                                        report 
                                          (fun () -> 
                                             msg "@[At [%s] replicas A and B are identical;" (string_of_path pt);
                                             msg "@ propagating both.@]@\n");
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end
                                    else
                                      begin
                                        report 
                                          (fun () -> 
                                             let koi = Misc.concat_list "," (Safelist.map fst oti) in 
                                             let kai = Misc.concat_list "," (Safelist.map fst ati) in 
                                             let kbi = Misc.concat_list "," (Safelist.map fst bti) in 
                                               msg "@[Conflict at [%s] on keys [%s] [%s] [%s];" 
                                                 (string_of_path pt) koi kai kbi;
                                               msg "@ returning the archive and replicas unchanged.@]@\n");                                      
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
              report (fun () -> msg "Conflict at root; returning the archive and replicas unchanged.@\n");
              (oo,ao,bo)
          | Some (r,d) -> 
              let s1,s2 = strings_of_dir d in 
              report (fun () -> msg "At root: %s are identical; propagating %s.@\n" s1 s2);
              (r,r,r)
        end
    | Some o,Some a,Some b ->
        let o',a',b' = isync ["."] t o a b in
        (Some o',Some a',Some b')
