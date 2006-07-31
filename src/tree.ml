(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* tree.ml - unordered, unranked trees                   *)
(*********************************************************)
(* $Id$ *)

type t = { map:t Name.Map.t; hash:int}

let hash t0 = t0.hash

let keyword_f f n =   
  let is_keyword = 
    try 
      let _ = Hashtbl.find Lexer.keywords n in 
        true 
    with Not_found -> false in 
    if is_keyword then Printf.sprintf "\"%s\"" n
    else f n

let keyword_whack = keyword_f Misc.whack
let keyword_whack_ident = keyword_f Misc.whack_ident

let rec pm_format m =
  Util.format "{";
  ignore (Name.Map.fold 
             (fun k tk fst ->
               if not fst then Util.format ", ";
               Util.format "%s=" (keyword_whack k);
               pm_format tk.map;
               false)
             m true);
  Util.format "}"

let mk_t = 
  let module M = Memo.Make(
    struct
      let name = "Tree.mk_t"
      type arg = (t Name.Map.t)
      type res = t 
      let format_arg = pm_format
      let format_res _ = Util.format "Tree.t"
      let hash m = Name.Map.fold (fun k tk a -> 379 * (Hashtbl.hash k) + 563 * tk.hash + a) m 0                
      let equal m1 m2 = 
        (m1 == m2) ||
          (let d1 = Name.Map.domain m1 in          
             (Name.Set.equal d1 (Name.Map.domain m2))
             && (Name.Set.for_all (fun n -> (Name.Map.find n m1) == (Name.Map.find n m2)) d1))
      let f m = { map=m; hash=hash m} 
      let init_size = 1543
    end) in 
    M.memoized

let nil_tag = "nil"
let hd_tag = "hd"
let tl_tag = "tl"

(* --------------- creators --------------- *)
let empty = mk_t Name.Map.empty

let set t0 k = function
  | None   -> mk_t (Name.Map.remove k t0.map)
  | Some v -> mk_t (Name.Map.add k v t0.map)

let from_list vl = mk_t (Name.Map.from_list vl)

(* --------------- accessors --------------- *)
let dom t0 = Name.Map.domain t0.map

let is_empty t0 = Name.Set.is_empty (dom t0)

let get t0 k = 
  try Some (Name.Map.find k t0.map)
  with Not_found -> None

(* -------------- values --------------- *)
(* v is a value if it has one child, and that child is [Tree.empty] *)
let is_value t0 =
  let d = dom t0 in 
    (Name.Set.cardinal d = 1) &&
    (match get t0 (Name.Set.choose d) with
         None -> false (* can't happen *)
       | Some tk -> is_empty tk)

let mk_value k = set empty k (Some empty)
      
(* --------------- lists --------------- *)
let cons t1 t2 = from_list [(hd_tag, t1); (tl_tag, t2)]

let cons_dom = Name.Set.add hd_tag (Name.Set.add tl_tag Name.Set.empty)
let nil_dom = Name.Set.add nil_tag Name.Set.empty

let empty_list = mk_value nil_tag

let is_cons t0 = Name.Set.equal (dom t0) (cons_dom)
    
let is_empty_list t0 = Name.Set.equal (dom t0) nil_dom

let rec is_list t0 = 
  is_empty_list t0 || 
    (is_cons t0 &&
        (match get t0 tl_tag with
            None -> false (* can't happen *)
          | Some tl -> is_list tl))

let rec structure_from_list ts = 
  let rec aux acc = function
    | [] -> acc
    | t::rest -> aux (cons t acc) rest in 
  aux empty_list (Safelist.rev ts)

(* -------------- pretty printing --------------- *)
let raw = Prefs.createBool "raw" false "Dump trees in 'raw' form" ""

let rec format_kids m format_rec =
  Util.format "{@[<hv0>";
  Name.Map.iter_with_sep
    (fun k kid -> 
      Util.format "@[<hv1>%s=@ " (keyword_whack_ident k);
      format_rec kid;
      Util.format "@]")
    (fun() -> Util.format ",@ ")
    m;
  Util.format "@]}"

and format_t_pretty t0 =
  let rec format_aux t1 inner = 
    let format_list_member kid =
      if is_value kid then Util.format "{%s}" (keyword_whack_ident (get_value kid))
      else format_aux kid true in
    if is_list t1 then begin
      let rec loop = function
          [] -> ()
        | [kid] -> format_list_member kid
        | kid::rest -> format_list_member kid; Util.format ",@ "; loop rest in
      Util.format "[@[<hv0>";
      loop (list_from_structure t1);
      Util.format "@]]"
    end else begin
      if (is_value t1 && inner) then Util.format "{%s}" (keyword_whack_ident (get_value t1))
      else format_kids t1.map (fun k -> format_aux k true)
    end in
  format_aux t0 false

and format_t_raw t0 =
  Name.Map.dump 
    (fun ks -> ks)
    keyword_whack 
    (fun t1 -> format_kids t1.map format_t_raw)
    is_empty 
    t0.map

and format_t t0 =
  if Prefs.read raw then format_t_raw t0
  else format_t_pretty t0

(* --------------- easy access / building --------------- *)
(* Note that these are all in the same mutual recursion, so that we can use the
   formatting stuff to print error messages in the next few functions! *)

and list_from_structure t0 =
  if not (is_list t0) then
    raise (Error.Harmony_error 
             (fun () -> 
                Util.format "Tree.list_from_structure:@ ";
                format_t t0;
                Util.format "@ is not a list!"));
  let rec loop acc t1' = 
    if is_empty_list t1' then Safelist.rev acc else
      loop ((get_required t1' hd_tag) :: acc) (get_required t1' tl_tag) in
  loop [] t0

and get_required ?(msg="") t0 k = 
  try Name.Map.find k t0.map
  with Not_found -> 
    raise (Error.Harmony_error 
             (fun () -> 
		Util.format "%sget_required %s failed on " 
		(if msg="" then "" else msg ^ ": ")
		(keyword_whack k);
                format_t t0))

and get_value t0 =
  if (is_value t0) then (Name.Set.choose (dom t0))
  else raise 
    (Error.Harmony_error 
       (fun () -> 
          Util.format "Tree.get_value ";
          format_t t0;
          Util.format " is not a value!"))


let list_length t0 = Safelist.length (list_from_structure t0)

let singleton_dom t0 =
  let d = dom t0 in
    if Name.Set.cardinal d <> 1 then
      raise (Error.Harmony_error (fun () -> 
        Util.format "Tree.singleton_dom: tree with several children";
        format_t t0));
    Name.Set.choose d

type desc =
    Tree of (Name.t * desc) list
  | L of desc list
  | Val of Name.t
  | In of t
  | E

let rec from_desc = function
    E -> empty
  | Val k -> mk_value k
  | L vl -> structure_from_list (Safelist.map from_desc vl)
  | Tree l -> from_list (Safelist.map (fun (k,d) -> (k, from_desc d)) l) 
  | In v -> v


(* ----------------------------------------------------------------------
 * Utility functions
 *)

let rec equal t1 t2 = t1 == t2

(*
  (t1 == t2) ||
  (let names = dom t1 in
  Name.Set.equal names (dom t2) &&
  Name.Set.for_all
  (fun n -> equal (get_required t1 n) (get_required t2 n)) 
  names) *)

let rec included_in t1 t2 =
  if t1 == t2 then true else
  let names = dom t1 in
  Name.Set.subset names (dom t2) &&
  Name.Set.for_all
    (fun n -> included_in (get_required t1 n) (get_required t2 n)) 
    names

let rec compare t1 t2 =
  let dt1, dt2 = dom t1, dom t2 in
  let dcmp = Name.Set.compare dt1 dt2 in
  if dcmp <> 0 then dcmp else
  List.fold_left
    (fun acc n ->
      if acc <> 0 then acc else
      compare (get_required t1 n) (get_required t2 n))
    0
    (Name.Set.elements dt1)

let fold f t0 c = Name.Map.fold f t0.map c

let to_list t0 =
  Safelist.rev (fold (fun n k acc -> (n,k) :: acc) t0 [])

let concat t1 t2 =
  let binds = (to_list t1) @ (to_list t2) in
  try
    from_list binds
  with
  | Invalid_argument _ -> 
      raise 
        ( Error.Harmony_error 
            (fun () -> 
               Util.format "Tree.concat: domain collision between the following two trees:";
               format_t t1;
               format_t t2))

let split p t0 =
  let binds1,binds2 =
    fold
      (fun k tk (t1acc,t2acc) ->
        if p k then ((k,tk)::t1acc,t2acc)
        else (t1acc,(k,tk)::t2acc))
      t0 ([],[]) in
    (from_list binds1, from_list binds2)

let string_of_t t0 = Util.format_to_string (fun () -> format_t t0)

let pathchange path m t0 =
  Util.format "%s: %s@,"
    (String.concat "/" (Safelist.rev (Safelist.map keyword_whack path)))
    m;
  Util.format "  @[";
  format_t t0;
  Util.format "@]@,"

let rec show_diffs_inner t1 t2 path =
  let t1_kids = dom t1 in
  let t2_kids = dom t1 in
  let all_kids = Name.Set.union t1_kids t2_kids in
  Name.Set.iter
    (fun k ->
      let path = k::path in
        match (get t1 k, get t2 k) with
            None, None -> assert false
          | Some t1k, None -> pathchange path "deleted value" t1k
          | None, Some t2k -> pathchange path "created with value" t2k
          | Some t1k, Some t2k -> show_diffs_inner t1k t2k path)
    all_kids

let rec show_diffs t1 t2 =
  Util.format "@[<v0>";
  show_diffs_inner t1 t2 [];
  Util.format "@]"

