(******************************************************************************)
(* Copyright (C) 2007-2008                                                    *)
(* J. Nathan Foster and Benjamin C. Pierce                                    *)
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
(* /boomerang/src/bheap.ml                                                    *)
(* Global Boomerang heap                                                      *)
(* $Id$ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
module V = Bvalue
open Berror
open Bsyntax
open Bident
open Bprint
let msg = Util.format 

(* ----- types ----- *)
type cell = Term of exp | Value of V.t

module H = Hashtbl.Make
  (struct 
     type t = int
     let equal i1 i2 = (i1 = i2)
     let hash i = i
   end)

(* ----- gensym ----- *)
let next_loc = ref 0
let fresh_loc () =
  let loc = !next_loc in
    incr next_loc;
    loc

(* ------ THE global heap ----- *)
let the_heap = ref (H.create 137)   

let init () = ()

(* ----- freshen expressions, sorts, patterns ------ *)
let rec freshen_exp lmap e0 = match e0 with
  | EApp(i,e1,e2) ->
      let new_e1 = freshen_exp lmap e1 in
      let new_e2 = freshen_exp lmap e2 in
      EApp(i,new_e1,new_e2)
  | EOver(i,o,el) ->
      let new_el = Safelist.map (freshen_exp lmap) el in
      EOver(i,o,new_el)
  | EFun(i,Param(ip,x1,s2),so3,e4) ->
      let new_s2 = freshen_sort lmap s2 in
      let new_so3 = Misc.map_option (freshen_sort lmap) so3 in
      let new_e4 = freshen_exp lmap e4 in
      EFun(i,Param(ip,x1,new_s2),new_so3,new_e4)
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let new_p1 = freshen_pat lmap p1 in
      let new_so2 = Misc.map_option (freshen_sort lmap) so2 in
      let new_e3 = freshen_exp lmap e3 in
      let new_e4 = freshen_exp lmap e4 in
      ELet(i,Bind(ib,new_p1,new_so2,new_e3),new_e4)
  | ETyFun(i,a,e1) ->
      let new_e1 = freshen_exp lmap e1 in
      ETyFun(i,a,new_e1)
  | ETyApp(i,e1,s2) ->
      let new_e1 = freshen_exp lmap e1 in
      let new_s2 = freshen_sort lmap s2 in
      ETyApp(i,new_e1,new_s2)
  | ECast(i,s1,s2,b,e3) ->
      let new_s1 = freshen_sort lmap s1 in
      let new_s2 = freshen_sort lmap s2 in
      let new_e3 = freshen_exp lmap e3 in
      ECast(i,new_s1,new_s2,b,new_e3)
  | EPair(i,e1,e2) ->
      let new_e1 = freshen_exp lmap e1 in
      let new_e2 = freshen_exp lmap e2 in
      EPair(i,new_e1,new_e2)
  | ECase(i,e1,cl,s3) ->
      let new_e1 = freshen_exp lmap e1 in
      let new_cl = 
	Safelist.map 
	  (fun (pi,ei) -> (freshen_pat lmap pi,freshen_exp lmap ei))
	  cl in
      let new_s3 = freshen_sort lmap s3 in
      ECase(i,new_e1,new_cl,new_s3)
  | ELoc(i,l) ->
      begin
	try ELoc(i,List.assoc l lmap)
	with Not_found -> ELoc(i,l)
      end
  | EAlloc(i,ls,e1) ->
      let (lmap',new_ls_r) = Safelist.fold_left
	(fun (lmap,ls) (li,ei) ->
	   let lmap' = Safelist.remove_assoc li lmap in
	   let new_ei = freshen_exp lmap' ei in
	     (lmap',(li,new_ei)::ls))
	(lmap,[]) ls in
      let new_e1 = freshen_exp lmap' e1 in
      EAlloc(i,Safelist.rev new_ls_r,new_e1)
  | EVar _ | EUnit _ | EBoolean _ | EInteger _ 
  | EChar _ | EString _ | ECSet _  -> e0

and freshen_sort lmap s0 = match s0 with
  | SFunction(x,s1,ls,s2) ->
      let new_s1 = freshen_sort lmap s1 in
      let (lmap',new_ls_r) = Safelist.fold_left
	(fun (lmap,ls) (li,ei) ->
	   let lmap' = Safelist.remove_assoc li lmap in
	   let new_ei = freshen_exp lmap' ei in
	     (lmap',(li,new_ei)::ls))
	(lmap,[]) ls in
      let new_s2 = freshen_sort lmap' s2 in
      SFunction(x,new_s1,Safelist.rev new_ls_r,new_s2)
  | SProduct(s1,s2) ->
      let new_s1 = freshen_sort lmap s1 in
      let new_s2 = freshen_sort lmap s2 in
      SProduct(new_s1,new_s2)
  | SData(sl,qx) ->
      let new_sl = Safelist.map (freshen_sort lmap) sl in
      SData(new_sl,qx)
  | SRefine(x,s1,e2) ->
      let new_s1 = freshen_sort lmap s1 in
      let new_e2 = freshen_exp lmap e2 in
      SRefine(x,new_s1,new_e2)
  | SForall(a,s1) ->
      let new_s1 = freshen_sort lmap s1 in
      SForall(a,new_s1)
  | SUnit | SBool | SInteger | SChar | SString 
  | SRegexp | SLens | SCanonizer | SVar _ -> s0

and freshen_pat lmap p0 = match p0 with
  | PVar(i,x,so1) ->
      let new_so1 = Misc.map_option (freshen_sort lmap) so1 in
      PVar(i,x,new_so1)
  | PVnt(i,qx,po1) ->
      let new_po1 = Misc.map_option (freshen_pat lmap) po1 in
      PVnt(i,qx,new_po1)
  | PPar(i,p1,p2) ->
      let new_p1 = freshen_pat lmap p1 in
      let new_p2 = freshen_pat lmap p2 in
      PPar(i,new_p1,new_p2)
  | PCex(i,p) ->
      let new_p = freshen_pat lmap p in
      PCex(i,new_p)
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> p0

(* ----- heap operations ----- *)
let alloc ls e1 =
  let lmap = 
    Safelist.fold_left
      (fun lmap (li,ei) ->
         let fi = fresh_loc () in
         let ci = Term (freshen_exp lmap ei) in
	 H.add !the_heap fi ci;
	 (li,fi)::lmap)
      [] ls in
  freshen_exp lmap e1

let get i l =
  try H.find !the_heap l
  with Not_found -> 
    run_error i (fun () -> msg "@[no@ such@ location@ %d@]" l)

let update l v = 
  H.replace !the_heap l (Value v)
