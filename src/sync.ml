(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.ml - Core synchronizer                                  *)
(****************************************************************)
(* $Id *)

(* BCP: We package the body of this module in another module, Internal,
   so that we can make it recursive with the D3 module defined at the end of
   the file (e.g., so that the action type and the D3.action type can refer to
   each other recursively). *)
module rec Internal :
  sig
    type action
    val equal: action
    val has_conflict: action -> bool
    val format_action: action -> unit
    val sync : Treeschema.t
            -> (Tree.t option * Tree.t option * Tree.t option)
            -> action * Tree.t option * Tree.t option * Tree.t option
  end = struct

let debug = Trace.debug "sync"

type copy_value =
  | Adding of Tree.t
  | Deleting of Tree.t
  | Replacing of Tree.t * Tree.t

type action =
  | SchemaConflict of Treeschema.t * Tree.t * Tree.t 
  | MarkEqual
  | DeleteConflict of Tree.t * Tree.t
  | CopyLeftToRight of copy_value
  | CopyRightToLeft of copy_value
  | ListSync of D3.action
  | GoDown of action Name.Map.t

let equal = MarkEqual

let rec has_conflict a = 
  match a with
      SchemaConflict _ | DeleteConflict _  -> true
    | MarkEqual | CopyLeftToRight _ | CopyRightToLeft _ -> false
    | ListSync a -> D3.has_conflict a 
    | GoDown(m) -> Name.Map.fold 
        (fun k a' c -> c || has_conflict a')
        m false

let format_copy s = function
  | Adding v ->
     V.format_msg [`Open_box; `String "Add ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree v; `Close_box]
  | Deleting v ->
     V.format_msg [`Open_box; `String "Delete ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree v; `Close_box]
  | Replacing (vold,vnew) ->
     V.format_msg [`Open_box; `String "Replace ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree vold; `Space; `String "with";
                   `SpaceOrIndent; `Tree vnew; `Close_box]

let format_schema_conflict t lv rv =
  V.format_msg ([`Open_vbox
                ; `String "[SchemaConflict] at type "
                ; `Prim (fun () -> Treeschema.format_t t)
                ; `Break; `Tree lv; `Break; `Tree rv
                ; `Close_box] )

let rec format_raw = function
  | SchemaConflict (t,lv,rv) ->
      format_schema_conflict t lv rv
  | GoDown(m) ->
      Name.Map.dump (fun ks -> ks) Misc.whack
        (fun x -> format_raw x)
        (fun _ -> false)
        m
  | MarkEqual -> Util.format "EQUAL"
  | DeleteConflict (v0,v) ->
      Util.format "DELETE CONFLICT@,  @[";
      Tree.show_diffs v0 v;
      Util.format "@]@,"
  | ListSync a -> D3.format_action a
  | CopyLeftToRight c -> format_copy "-->" c
  | CopyRightToLeft c -> format_copy "<--" c

let is_cons m = 
  let dom_m = Name.Map.domain m in
     Name.Set.mem Tree.hd_tag dom_m 
  || Name.Set.mem Tree.tl_tag dom_m 

let list_tags =
  Safelist.fold_right
    Name.Set.add
    [Tree.hd_tag; Tree.tl_tag; Tree.nil_tag]
    Name.Set.empty

let rec format_pretty = function
  | SchemaConflict (t,lv,rv) ->
      format_schema_conflict t lv rv
  | GoDown(m) ->
      if is_cons m then begin
        (* Special case for lists *)
        Util.format "[@[<hv0>";
        format_cons 0 m
      end else begin
        (* Default case *)
        let prch (n,ch) = 
          let prf() =
              Util.format "@["; format_pretty ch; Util.format "@]" in
          Util.format "@[<hv1>%s =@ " (Misc.whack n);
          prf();
          Util.format "@]" in
        Util.format "{@[<hv0>";
        let binds = Safelist.map (fun k -> (k, Name.Map.find k m))
                      (Name.Set.elements (Name.Map.domain m)) in
        let binds = Safelist.filter (fun (k,e) -> e <> MarkEqual) binds in
        (* Here, we should check for a replacement and treat it special! *)
        Misc.iter_with_sep
          prch
          (fun()-> Util.format ",@;<1 0>")
          binds;
        Util.format "@]}"
      end 
  | MarkEqual ->
      (* By construction, this case can only be invoked at the root *)
      Util.format "EQUAL"
  | DeleteConflict (v0,v) ->
      Util.format "DELETE CONFLICT@,  @[";
      Tree.show_diffs v0 v;
      Util.format "@]@,"
  | CopyLeftToRight c -> format_copy "-->" c
  | CopyRightToLeft c -> format_copy "<--" c
  | ListSync a -> D3.format_action a

(* BCP: This can be deleted after we commit to the new list sync stuff *)
and format_cons equal_hd_count m =
  let dump_hd_count n =
    if n = 0 then ()
    else if n = 1 then Util.format "..." 
    else Util.format "...(%d)..." n in
  let hd_action = (try Name.Map.find Tree.hd_tag m with Not_found -> MarkEqual) in
  let tl_tag =
    begin
      try Name.Set.choose (Name.Set.diff (Name.Map.domain m) list_tags)
      with Not_found -> Tree.tl_tag 
    end in
  let tl_action = (try Name.Map.find tl_tag m with Not_found -> MarkEqual) in
  let hd_interesting = (hd_action <> MarkEqual) in
  let tl_interesting =
    (match tl_action with GoDown m -> not (is_cons m)
                        | _ -> true) in
  begin (* format the head and/or an appropriate separator, as needed *)
    match (hd_interesting, tl_interesting) with
    | true,true -> if equal_hd_count > 0 then (dump_hd_count equal_hd_count; Util.format ",@ ");
                   format_pretty hd_action; Util.format ";@ "
    | false,true -> dump_hd_count (equal_hd_count+1); Util.format ";@ ";
    | true,false -> if equal_hd_count > 0 then (dump_hd_count equal_hd_count; Util.format ",@ ");
                    format_pretty hd_action; Util.format ",@ "
    | false,false -> ()
  end;
  match tl_action with
  | GoDown(m) -> format_cons (if hd_interesting then 0 else equal_hd_count+1) m
  | MarkEqual -> Util.format "...]@]"
  | a -> format_pretty a; Util.format "]@]"

let format_action = format_pretty

(*********************************************************************************)

(* accumulate adds a binding for a tree option to an accumulator *)
let accumulate oldacc k = function
    None -> oldacc
  | Some v -> (k, v) :: oldacc

let the = function None -> assert false | Some x -> x

let combine_conflicts c1 c2 = match (c1,c2) with
    (`NoConflict, `NoConflict) -> `NoConflict
  | _ -> `Conflict

let assert_member v t = 
  if (not (Treeschema.member v t)) then begin 
    Lens.error [`String "Synchronization error: "; `Break ; `Tree v; `Break
               ; `String " does not belong to "; `Break
               ; `Prim (fun () -> Treeschema.format_t t)]
  end 

let rec sync s (oo, ao, bo) = 
  match (oo, ao, bo) with
    | _, None, None       ->
        (MarkEqual, None, None, None)
    | None, None, Some rv -> 
        assert_member rv s;
        (CopyRightToLeft (Adding rv), Some rv, Some rv, Some rv)
    | None, Some lv, None -> 
        assert_member lv s; 
        (CopyLeftToRight (Adding lv), Some lv, Some lv, Some lv)
    | Some arv, None, Some rv ->
        assert_member rv s;
        if Tree.included_in rv arv then 
          (CopyLeftToRight (Deleting rv), None, None, None)
        else 
          (DeleteConflict(arv,rv), oo, ao, bo)
    | Some arv, Some lv, None ->
        assert_member lv s;
        if Tree.included_in lv arv then 
          (CopyRightToLeft (Deleting lv), None, None, None)
        else 
          (DeleteConflict(arv,lv), oo, ao, bo)
    | _, Some lv, Some rv ->
        assert_member lv s;
        assert_member rv s;
        (* BCP [Oct 05]: The following test could give us a nasty
           n^2 behavior in deep (and narrow) trees. *)
        if Tree.equal lv rv then begin
          (MarkEqual, Some lv, Some lv, Some rv)
        (* BCP [Apr 06]: The next tests are potentially nasty too!  And the
           test for Tree.hd_tag is a hack that should be removed, if possible. *)
        end else if Name.Set.is_empty (Name.Set.inter
                                        (Name.Set.remove Tree.hd_tag (Tree.dom lv))
                                        (Name.Set.remove Tree.hd_tag (Tree.dom rv)))
             && oo <> None
             && (Tree.equal (the oo) lv || Tree.equal (the oo) rv) then 
          if Tree.equal (the oo) lv then 
            (CopyRightToLeft (Replacing (lv,rv)), Some rv, Some rv, Some rv)
          else 
            (CopyLeftToRight (Replacing (rv,lv)), Some lv, Some lv, Some lv)
        else if (* The first two conditions are redundant, strictly speaking
                   (they are implied by the last), but they are faster to check *)
                ((Tree.is_cons lv) || (Tree.is_empty_list lv))
             && ((Tree.is_cons rv) || (Tree.is_empty_list rv))
             && Treeschema.is_list s then 
          (* Call the diff3 module to handle list sync: *)
          let ll = Tree.list_from_structure lv in
          let rl = Tree.list_from_structure rv in
          let ol = match oo with
                     None -> []
                   | Some ov ->
                       if Tree.is_list ov then Tree.list_from_structure ov else [] in
          let elt_schema = Treeschema.project Tree.hd_tag s in
          let (a,ol',ll',rl') = D3.sync elt_schema (ol,ll,rl) in
          (ListSync a,
           Some (Tree.structure_from_list ol'),
           Some (Tree.structure_from_list ll'),
           Some (Tree.structure_from_list rl'))
        else
          let lrkids = Name.Set.union (Tree.dom lv) (Tree.dom rv) in
          let acts, arbinds, lbinds, rbinds =            
            Name.Set.fold
              (fun k (actacc, aracc, lacc, racc) ->
                 let tk = Treeschema.project k s in
                 (* Note that tk cannot be empty, since every child k is in *)
                 (* either dom(a) or dom(b), both of which are in  *)
                 (* T, as we just checked. *)
                 let act, o', a', b' =
                   sync 
                     tk
                     ((match oo with None -> None | Some av -> Tree.get av k),
                      (Tree.get lv k),
                      (Tree.get rv k)) in  
                 let aracc = accumulate aracc k o' in
                 let lacc  = accumulate lacc k a' in
                 let racc  = accumulate racc k b' in
                 ((k, act)::actacc, aracc, lacc, racc))
              lrkids 
              ([], [], [], [])   in
          let o',a',b' = 
            (Tree.from_list arbinds),
            (Tree.from_list lbinds),
            (Tree.from_list rbinds)   in
          let a'_in_tdoms = Treeschema.dom_member (Tree.dom a') s in
          let b'_in_tdoms = Treeschema.dom_member (Tree.dom b') s in
          if a'_in_tdoms && b'_in_tdoms then
              (GoDown(Safelist.fold_left
                        (fun acc (k, act) -> Name.Map.add k act acc)
                        Name.Map.empty
                        acts),
               Some o', Some a', Some b')
          else 
            (SchemaConflict(s,lv,rv),oo,ao,bo)

end (* module Internal *)

and D3Args : Diff3.DIFF3ARGS with type elt = Tree.t
= struct
  type elt = Tree.t
  type action = Internal.action
  let has_conflict = Internal.has_conflict
  let format_action = Internal.format_action                       
  let eqv = Tree.equal
  let format = Tree.format_t
  let tostring = Tree.string_of_t
  let sync = Internal.sync
end

and D3 : Diff3.DIFF3RES with type elt = Tree.t =
Diff3.Make(D3Args)

(* Extract top-level definitions from the Internal module and re-export *)
type action = Internal.action
let sync = Internal.sync
let format_action = Internal.format_action
let has_conflict = Internal.has_conflict
let equal = Internal.equal

