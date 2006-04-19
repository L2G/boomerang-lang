(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.ml - Core synchronizer                                  *)
(****************************************************************)
(* $Id *)

let diff3 = Prefs.createBool "diff3" false
  "Use diff3 algorithm to synchronize lists"
  "Use diff3 algorithm to synchronize lists"

let debug = Trace.debug "sync"

type copy_value =
  | Adding of V.t
  | Deleting of V.t
  | Replacing of V.t * V.t

(*********************************************************************************)

module D3Args = struct
  type elt = V.t
  let eqv = V.equal
  let format = V.format_t
  let tostring = V.string_of_t
end
module D3 = Diff3.Make(D3Args)

(*********************************************************************************)

let format_copy s = function
  | Adding v ->
     V.format_msg [`Open_box; `String " Add ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree v; `Close_box]
  | Deleting v ->
     V.format_msg [`Open_box; `String " Delete ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree v; `Close_box]
  | Replacing (vold,vnew) ->
     V.format_msg [`Open_box; `String " Replace ("; `String s; `String ")";
                   `SpaceOrIndent; `Tree vold; `Space; `String "with";
                   `SpaceOrIndent; `Tree vnew; `Close_box]
	
(* accum : oldacc -> key -> val option -> newacc *)
(* accumulate adds a binding for a tree option to an accumulator *)
let accumulate oldacc k = function
    None -> oldacc
  | Some v -> (k, v) :: oldacc

let the = function None -> assert false | Some x -> x

let combine_conflicts c1 c2 = match (c1,c2) with
    (`NoConflict, `NoConflict) -> `NoConflict
  | _ -> `Conflict

let rec sync s oo ao bo log = 
  let assert_member v t = 
    if (not (Schema.member v t)) then begin 
      Lens.error [`String "Synchronization error: "; `Break ; `Tree v; `Break
                 ; `String " does not belong to "; `Break
                 ; `Prim (fun () -> Schema.format_t t)]
    end in    
  let inputs = (oo, ao, bo) in
  match inputs with
    | _, None, None       ->
        (`NoConflict, None, None, None)
    | None, None, Some rv -> 
        assert_member rv s;
        if log then format_copy "<--" (Adding rv);
        (`NoConflict, Some rv, Some rv, Some rv)
    | None, Some lv, None -> 
        assert_member lv s; 
        if log then format_copy "-->" (Adding lv);
        (`NoConflict, Some lv, Some lv, Some lv)
    | Some arv, None, Some rv ->
        assert_member rv s;
        if V.included_in rv arv then begin
          if log then format_copy "-->" (Deleting rv);
          (`NoConflict, None, None, None)
        end else begin
          if log then begin
            Format.printf "DELETE CONFLICT@,  @[";
            V.show_diffs arv rv;
            Format.printf "@]@,"
          end;
          (`Conflict, oo, ao, bo)
        end 
    | Some arv, Some lv, None ->
        assert_member lv s;
        if V.included_in lv arv then begin
          if log then format_copy "<--" (Deleting lv);
          (`NoConflict, None, None, None)
        end else begin
          if log then begin
            Format.printf "DELETE CONFLICT@,  @[";
            V.show_diffs arv lv;
            Format.printf "@]@,"
          end;
          (`Conflict, oo, ao, bo)
        end 
    | _, Some lv, Some rv ->
        assert_member lv s;
        assert_member rv s;
        (* BCP [Oct 05]: The following test could give us a nasty
           n^2 behavior in deep (and narrow) trees. *)
        if V.equal lv rv then begin
          (`NoConflict, Some lv, Some lv, Some rv)
        (* BCP [Apr 06]: The next tests are potentially nasty too!  And the
           test for V.hd_tag is a hack that should be removed, if possible. *)
        end else if Name.Set.is_empty (Name.Set.inter
                                        (Name.Set.remove V.hd_tag (V.dom lv))
                                        (Name.Set.remove V.hd_tag (V.dom rv)))
             && oo <> None
             && (V.equal (the oo) lv || V.equal (the oo) rv) then 
          if V.equal (the oo) lv then begin
            if log then format_copy "<--" (Replacing (lv,rv));
            (`NoConflict, Some rv, Some rv, Some rv)
          end else begin
            if log then format_copy "-->" (Replacing (rv,lv));
            (`NoConflict, Some lv, Some lv, Some lv)
          end 
        else if V.is_list lv && V.is_list rv && Prefs.read diff3 then 
          (* Call the diff3 module to handle list sync: *)
          let ll = V.list_from_structure lv in
          let rl = V.list_from_structure rv in
          let ol = match oo with
                     None -> []
                   | Some ov ->
                       if V.is_list ov then V.list_from_structure ov else [] in
          let elt_schema =
            (* BCP: Following line is bogus -- assumes that all lists are
               homogeneous and just takes the type of the first element as the
               type of all elements.  But what, exactly, should we do instead?? *)
            match Schema.project s V.hd_tag with
              None -> assert false (* Not a list schema? *)
            | Some ss -> ss in
          let (lconflicts,ol',ll',rl') =
            D3.sync
              (fun (subo,suba,subb) -> sync elt_schema subo suba subb log)
              log (ol,ll,rl) in
          (lconflicts,
           Some (V.structure_from_list ol'),
           Some (V.structure_from_list ll'),
           Some (V.structure_from_list rl'))
        else if (V.is_list lv || V.is_list rv) && Prefs.read diff3 then begin
          if log then 
          V.format_msg ([`Open_vbox
            ; `String "List conflict (synchronizing a list with a non-list) "
            ; `String "between"
            ; `Break; `Tree lv; `Space; `String "and"; `Space; `Tree rv
            ; `Close_box] );
          (`Conflict, oo, ao, bo)
        end else
          let lrkids = Name.Set.union (V.dom lv) (V.dom rv) in
          let conflicts, arbinds, lbinds, rbinds =            
            Name.Set.fold
              (fun k (cacc, aracc, lacc, racc) ->
                 let tk = match Schema.project s k with
                     None ->
                       (* Can't happen, since every child k is in        *)
                       (* either dom(a) or dom(b), both of which are in  *)
                       (* T, as we just checked. For debugging, here's a *)
                       (* helpful error message:                         *)
                       Lens.error [`String "synchronization bug: type "
                             ; `Prim (fun () -> Schema.format_t s)
                             ; `String " cannot be projected on "; `String k]
                   | Some tk -> tk   in
                 let c', o', a', b' =
                   sync 
                     tk
                     (match oo with None -> None | Some av -> V.get av k)
                     (V.get lv k)
                     (V.get rv k) log in  
                 let cacc  = combine_conflicts cacc c' in
                 let aracc = accumulate aracc k o' in
                 let lacc  = accumulate lacc k a' in
                 let racc  = accumulate racc k b' in
                 (cacc, aracc, lacc, racc))
              lrkids 
              (`NoConflict, [], [], [])   in
          let o',a',b' = 
            (V.from_list arbinds),
            (V.from_list lbinds),
            (V.from_list rbinds)   in
          let a'_in_tdoms = Schema.dom_member a' s in
          let b'_in_tdoms = Schema.dom_member b' s in
          if a'_in_tdoms && b'_in_tdoms then
            (conflicts, Some o', Some a', Some b')
          else begin
            if log then V.format_msg ([`Open_vbox
                                      ; `String "[SchemaConflict] at type "
                                       ; `Prim (fun () -> Schema.format_t s)
                                       ; `Break; `Tree lv; `Break; `Tree rv
                                       ; `Close_box] );
            (`Conflict, oo, ao, bo)
          end

