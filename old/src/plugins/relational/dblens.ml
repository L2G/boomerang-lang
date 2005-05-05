
module Db = Map.Make (String)

type ('c, 'a) lens = {
  get : 'c -> 'a;
  put : 'a -> 'c -> 'c;
}

(* Functions to lift lenses on relations to lenses on databases *)

let lift_unary lens src dst =
  let getfun c =
    let srctbl = Db.find src c in
    Db.add dst (lens.Rlens.get srctbl)
      (Db.remove src c)
  and putfun a c =
    let dsttbl = Db.find dst a in
    let srctbl = Db.find src c in
    Db.add src (lens.Rlens.put dsttbl srctbl)
      (Db.remove dst a)
  in
  { get = getfun; put = putfun }

let lift_binary lens src1 src2 dst =
  let getfun c =
    let srctbl1 = Db.find src1 c in
    let srctbl2 = Db.find src2 c in
    Db.add dst (lens.Rlens.get (srctbl1, srctbl2))
      (Db.remove src1
        (Db.remove src2 c))
  and putfun a c =
    let dsttbl = Db.find dst a in
    let srctbl1 = Db.find src1 c in
    let srctbl2 = Db.find src2 c in
    let newtbl1, newtbl2 = lens.Rlens.put dsttbl (srctbl1, srctbl2) in
    Db.add src1 newtbl1
      (Db.add src2 newtbl2
        (Db.remove dst a))
  in
  { get = getfun; put = putfun }

(* Lenses *)

let rename m n = lift_unary (Rlens.rename m n)

let union = lift_binary (Rlens.uniongen (fun x -> Rlens.Both))
let unionl = lift_binary (Rlens.uniongen (fun x -> Rlens.Left))
let unionr = lift_binary (Rlens.uniongen (fun x -> Rlens.Right))
let uniongen f = lift_binary (Rlens.uniongen f)

let inter = lift_binary (Rlens.intergen (fun x -> Rlens.Both))
let interl = lift_binary (Rlens.intergen (fun x -> Rlens.Left))
let interr = lift_binary (Rlens.intergen (fun x -> Rlens.Right))
let intergen f = lift_binary (Rlens.intergen f)

let diff = lift_binary (Rlens.diffgen (fun x -> Rlens.Both))
let diffl = lift_binary (Rlens.diffgen (fun x -> Rlens.Left))
let diffr = lift_binary (Rlens.diffgen (fun x -> Rlens.Right))
let diffgen f = lift_binary (Rlens.diffgen f)

let select k s = lift_unary (Rlens.select k s)
let selectgen p = lift_unary (Rlens.selectgen p)

let project p q d = lift_unary (Rlens.project p q d)

let join = lift_binary Rlens.join

