
module Db = Map.Make (String)

(* Functions to lift lenses on relations to lenses on databases *)

let lift_unary lens src dst =
  let getfun c =
    let srctbl = Db.find src c in
    Db.add dst (Lens.get lens srctbl)
      (Db.remove src c)
  and putfun a co =
    let dsttbl = Db.find dst a in
    let newtbl =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl = Db.find src c in
          Lens.put lens dsttbl (Some(srctbl))
    in
    Db.add src newtbl
      (Db.remove dst a)
  in
  Lens.native getfun putfun

let lift_binary lens src1 src2 dst =
  let getfun c =
    let srctbl1 = Db.find src1 c in
    let srctbl2 = Db.find src2 c in
    Db.add dst (Lens.get lens (srctbl1, srctbl2))
      (Db.remove src1
        (Db.remove src2 c))
  and putfun a co =
    let dsttbl = Db.find dst a in
    let newtbl1, newtbl2 =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl1 = Db.find src1 c in
          let srctbl2 = Db.find src2 c in
          Lens.put lens dsttbl (Some((srctbl1, srctbl2)))
    in
    Db.add src1 newtbl1
      (Db.add src2 newtbl2
        (Db.remove dst a))
  in
  Lens.native getfun putfun

(* Lenses *)

let rename m n = lift_unary (Rlens.rename m n)

let union = lift_binary (Rlens.generic_union (fun x -> Rlens.Both))
let unionl = lift_binary (Rlens.generic_union (fun x -> Rlens.Left))
let unionr = lift_binary (Rlens.generic_union (fun x -> Rlens.Right))

let inter = lift_binary (Rlens.generic_inter (fun x -> Rlens.Both))
let interl = lift_binary (Rlens.generic_inter (fun x -> Rlens.Left))
let interr = lift_binary (Rlens.generic_inter (fun x -> Rlens.Right))

let diff = lift_binary (Rlens.generic_diff (fun x -> Rlens.Both))
let diffl = lift_binary (Rlens.generic_diff (fun x -> Rlens.Left))
let diffr = lift_binary (Rlens.generic_diff (fun x -> Rlens.Right))

let select k s =
  lift_unary (Rlens.generic_select (
    fun x -> List.assoc k x = s
  ))
let select_eq k1 k2 =
  lift_unary (Rlens.generic_select (
    fun x -> List.assoc k1 x = List.assoc k2 x
  ))

let project p q d = lift_unary (Rlens.project p q d)

let join = lift_binary (Rlens.generic_join (fun x -> Rlens.Both))
let joinl = lift_binary (Rlens.generic_join (fun x -> Rlens.Left))
let joinr = lift_binary (Rlens.generic_join (fun x -> Rlens.Right))

