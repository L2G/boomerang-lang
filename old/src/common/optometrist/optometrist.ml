type type_desc = string list
type feature_tree_type = (Syntax.ctx * Syntax.typ)

exception Bad_lens of string

(* view_type_as_string : view_type -> string *)
let type_desc_as_string vt = "[" ^ (String.concat "; " vt) ^ "]"

(* view_type_list_as_string : view_type list -> string *)
let type_desc_list_as_string vts = 
  "{" ^ (String.concat ", " (Safelist.map type_desc_as_string vts)) ^ "}"

(* prefix operator, returns true if vt1 is a prefix of vt2 *)
let rec (<:) vt1 vt2 =
  match vt1, vt2 with
    [], _ -> true
  | h1::t1, h2::t2 -> if h1 = h2 then t1 <: t2 else false
  | _ -> false

(* difference operator, returns the part of vt2 which is not the same as vt1 *)
let rec difference vt1 vt2 =
  match vt1, vt2 with
    [], d -> d
  | hs::ts, hl::tl when hs = hl -> difference ts tl
  | l1, l2 -> raise (Misc.Bad ("difference called on a non-subtype. l1:"^
                               (type_desc_as_string l1)^" l2:"^
                               (type_desc_as_string l2)))

let apply_type curr ctype atype = atype @ (difference ctype curr)

(* A type_desc list keyed map. *)
module VTKey : Mapplus.OrderedType with type t = type_desc =
  struct
    type t = type_desc
    let compare = Pervasives.compare
    let to_string vt = "[" ^ (String.concat "; " vt) ^ "]"
  end
module VTMP = Mapplus.Make (VTKey)
module VTMap = VTMP.Map
module VTSet = VTMP.KeySet

module MetaLensKey : Map.OrderedType
                     with type t = (type_desc * Lens.t * feature_tree_type * type_desc) =
  struct
    type t = type_desc * Lens.t * feature_tree_type * type_desc
    (* we only compare the view types *)
    let compare (ctype1,_,_,atype1) (ctype2,_,_,atype2) =
      Pervasives.compare (ctype1, atype1) (ctype2, atype2)
  end
module MetaLensSet = Set.Make (MetaLensKey)

(* lenses is the set of lenses the optometrist knows about *)
let lenses = ref MetaLensSet.empty

(** can_sync_as_single : type_desc -> VTMap
   [can_sync_as_single sp] returns a map which has entries for every reachable
   type_desc, each of which points to the list of lenses which get you there
   (starting from a view of type [sp]). *)
(* sp means "starting point" *)
let can_sync_as_single sp =
  (* v is the set of view types we have already expanded.
     v \subseteq dom lmap.
     vt => l is an entry of lmap if l transforms views of type sp to views
        of type vt. *)
  let rec can_sync_as_single_aux lmap v =
    if VTSet.equal (VTMap.domain lmap) v then lmap
    else
      (* pick an element of lmap we haven't expanded yet. *)
      let curr = VTSet.choose (VTSet.diff (VTMap.domain lmap) v) in
      (* lenses' is the set of all the lenses which start from curr *)
      let lenses' =
        MetaLensSet.filter (function (ctype,_,_,_) -> ctype <: curr) !lenses in
      (* update lmap to contain entries for everything that we can reach from
         curr: *)
      let lmap =
        MetaLensSet.fold
          (fun (ctype, lens, ftt, atype) lmap ->
             (* add the new lens to the lens map keyed by the abstract type *)
             (* not optimized at all *)
	     let lens' = (fst (VTMap.find curr lmap)) @ [lens] in
             let lmap = VTMap.add (apply_type curr ctype atype) (lens',ftt) lmap in
               lmap)
          lenses'
          lmap
      in
	can_sync_as_single_aux lmap (VTSet.add curr v)
  in
  let emptyType = ([],Syntax.TEmpty Error.bogusInfo) in
  let lmap = VTMap.add sp ([], emptyType) VTMap.empty in
  let v = VTSet.empty in
    can_sync_as_single_aux lmap v
      
(** can_sync_as : type_desc list -> (type_desc * Lens.t list) list *)
let can_sync_as sps =
  let maps = Safelist.map can_sync_as_single sps in
  let vtsets = Safelist.map VTMap.domain maps in
  let common_vts =
    match vtsets with
      [] -> assert false
    | [s] -> s
    | h::t -> Safelist.fold_left VTSet.inter h t
  in
  VTSet.fold (fun cvt acc -> (cvt, Safelist.map (VTMap.find cvt) maps) :: acc)
             common_vts
             []

(* can_sync_as1 : type_desc -> (type_desc * Lens.t list) list *)
let can_sync_as1 sp =
  Safelist.map (fun vtl ->
              match vtl with
                (vt, [l]) -> (vt, l)
              | _ -> assert false)
           (can_sync_as [sp])

(*
  can_sync_as2 : type_desc * type_desc -> 
                 (type_desc * (Lens.t list * Lens.t list)) list
*)
let can_sync_as2 (sp1, sp2) =
  Safelist.map (fun vtl ->
              match vtl with
                (vt, [l1; l2]) -> (vt, (l1, l2))
              | _ -> assert false)
           (can_sync_as [sp1; sp2])

(** throws Bad_lens when the lens introduces an ambiguity *)
let register_lens ctype atype ftt lens =
  lenses := MetaLensSet.add (ctype, lens, ftt, atype) !lenses
  (* TODO: check for ambiguities after each time we add *)
  (*print_endline ("checking lens with type: " ^ (type_desc_as_string ctype) ^ " -> " ^
                 ((type_desc_as_string atype));
  let _ = can_sync_as_single ctype in
  ()*)
