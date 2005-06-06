
module R = Relation

(* Some relational lenses *)

(* Helper functions *)

type bias = Left | Right | Both

let take_dir d f x r =
  if f x = d then R.insert x r else r

let ( ||~ ) = R.union
let ( &&~ ) = R.inter
let ( --~ ) = R.diff

let rcd_extends r1 r2 =
  List.for_all (fun x -> List.mem x r1) r2

let list_diff l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

let list_inter l1 l2 =
  list_diff l1 (list_diff l1 l2)

let rel_select p rel =
  R.fold
    (fun x r -> if p x then R.insert x r else r)
    rel
    (R.create (R.fields rel))

let rel_join rel1 rel2 =
  let r = R.create (R.fields rel1 @ R.fields rel2) in
  let addall1 rcd r =
    let accum x r =
      let agree = list_inter x rcd in
      let agreeflds = list_inter (fst (List.split x)) (fst (List.split rcd)) in
      if List.length agree = List.length agreeflds then
        R.insert ((list_diff x agree) @ rcd) r
      else
        r
    in
    R.fold accum rel1 r
  in
  R.fold addall1 rel2 r

(* Rename *)

let rename m n =
  let getfun c =
    R.rename m n c
  and putfun a co =
    R.rename n m a
  in
  Lens.native getfun putfun

(* Union *)

let generic_union f =
  let getfun (c1, c2) =
    c1 ||~ c2
  and putfun a co =
    let rev_union (c1, c2) =
      let free_agents = a --~ (c1 ||~ c2) in
      let left = R.fold
        (take_dir Left f) free_agents (R.create (R.fields free_agents)) in
      let right = R.fold
        (take_dir Right f) free_agents (R.create (R.fields free_agents)) in
      let both = R.fold
        (take_dir Both f) free_agents (R.create (R.fields free_agents)) in
      ((a &&~ c1) ||~ left ||~ both, (a &&~ c2) ||~ right ||~ both)
    in
    match co with
    | None -> let empty = R.create (R.fields a) in rev_union (empty, empty)
    | Some(c) -> rev_union c
  in
  Lens.native getfun putfun

(* Intersection *)

let generic_inter f =
  let getfun (c1, c2) =
    c1 &&~ c2
  and putfun a co =
    let rev_inter (c1, c2) =
      let free_agents = (c1 &&~ c2) --~ a in
      let left = R.fold
        (take_dir Left f) free_agents (R.create (R.fields free_agents)) in
      let right = R.fold
        (take_dir Right f) free_agents (R.create (R.fields free_agents)) in
      let both = R.fold
        (take_dir Both f) free_agents (R.create (R.fields free_agents)) in
      ((a ||~ c1) --~ (left ||~ both), (a ||~ c2) --~ (right ||~ both))
    in
    match co with
    | None -> let empty = R.create (R.fields a) in rev_inter (empty, empty)
    | Some(c) -> rev_inter c
  in
  Lens.native getfun putfun

(* Difference *)

let generic_diff f =
  let getfun (c1, c2) =
    c1 --~ c2
  and putfun a co =
    let rev_diff (c1, c2) =
      let free_agents = (c1 --~ c2) --~ a in
      let left = R.fold
        (take_dir Left f) free_agents (R.create (R.fields free_agents)) in
      let right = R.fold
        (take_dir Right f) free_agents (R.create (R.fields free_agents)) in
      let both = R.fold
        (take_dir Both f) free_agents (R.create (R.fields free_agents)) in
      ((a ||~ c1) --~ (left ||~ both), (c2 --~ a) ||~ (right ||~ both))
    in
    match co with
    | None -> let empty = R.create (R.fields a) in rev_diff (empty, empty)
    | Some(c) -> rev_diff c
  in
  Lens.native getfun putfun

(* Selection *)

(* general predicate on records *)
let generic_select p =
  let getfun c =
    rel_select p c
  and putfun a co =
    let c = match co with None -> R.create (R.fields a) | Some(c) -> c in
    let a' = rel_select p c in
    let added = a --~ a' in
    let removed = a' --~ a in
    (c --~ removed) ||~ added
  in
  Lens.native getfun putfun

(* Projection *)

let rcd_extends r1 r2 =
  List.for_all (fun x -> List.mem x r1) r2

let list_diff l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

let project p q d =
  let getfun c =
    R.project p c
  and putfun a co =
    let c = match co with None -> R.create (R.fields a) | Some(c) -> c in
    if R.equal a (R.project p c) then
      c
    else
      let compflds = (list_diff (R.fields c) p) @ q in
      let comp = R.project compflds c in
      let newkeys = R.project q a --~ R.project q c in
      let additions = rel_join newkeys d in
      rel_join a (comp ||~ additions)
  in
  Lens.native getfun putfun

(* Join *)

let generic_join f =
  let getfun (c1, c2) =
    rel_join c1 c2
  and putfun a co =
    let rev_join (c1, c2) =
      let free_agents = (rel_join c1 c2) --~ a in
      let left = R.fold
        (take_dir Left f) free_agents (R.create (R.fields free_agents)) in
      let right = R.fold
        (take_dir Right f) free_agents (R.create (R.fields free_agents)) in
      let both = R.fold
        (take_dir Both f) free_agents (R.create (R.fields free_agents)) in
      let proj1 = R.project (R.fields c1) in
      let proj2 = R.project (R.fields c2) in
      ((proj1 a ||~ c1) --~ proj1 (left ||~ both),
        (proj2 a ||~ c2) --~ proj2 (right ||~ both))
    in
    match co with
    | None -> let empty = R.create (R.fields a) in rev_join (empty, empty)
    | Some(c) -> rev_join c
  in
  Lens.native getfun putfun

