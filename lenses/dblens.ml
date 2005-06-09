
module Db = Map.Make(String)

(* Functions to lift lenses on relations to lenses on databases *)

(*
let mk_lens opname get put =
  let trap_not_found f x y =
    try f x y with
    | Not_found ->
        Lens.error [ `String(opname^":"); `Space; `String("no such table") ]
  in
  Lens.native ((trap_not_found (fun () -> get)) ()) (trap_not_found put)
*)

let table_missing opname tblname =
  Lens.error
    [ `String(opname^":")
    ; `Space; `String("no such table:")
    ; `Space; `String(tblname)
    ]

let table_exists opname tblname =
  Lens.error
    [ `String(opname^":")
    ; `Space; `String("table already exists:")
    ; `Space; `String(tblname)
    ]

let lift_unary opname lens src dst =
  let getfun c =
    let srctbl =
      try Db.find src c with Not_found -> table_missing opname src in
    if dst <> src && Db.mem dst c then table_exists opname dst;
    Db.add dst (Lens.get lens srctbl)
      (Db.remove src c)
  and putfun a co =
    let dsttbl =
      try Db.find dst a with Not_found -> table_missing opname dst in
    let newtbl =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl =
            try Db.find src c with
            | Not_found -> table_missing opname src
          in
          Lens.put lens dsttbl (Some(srctbl))
    in
    if src <> dst && Db.mem src a then table_exists opname src;
    Db.add src newtbl
      (Db.remove dst a)
  in
  Lens.native getfun putfun

let lift_binary opname lens src1 src2 dst =
  let getfun c =
    let srctbl1 =
      try Db.find src1 c with Not_found -> table_missing opname src1 in
    let srctbl2 =
      try Db.find src2 c with Not_found -> table_missing opname src2 in
    if dst <> src1 && dst <> src2 && Db.mem dst c then table_exists opname dst;
    Db.add dst (Lens.get lens (srctbl1, srctbl2))
      (Db.remove src1
        (Db.remove src2 c))
  and putfun a co =
    let dsttbl =
      try Db.find dst a with Not_found -> table_missing opname dst in
    let newtbl1, newtbl2 =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl1 =
            try Db.find src1 c with
            | Not_found -> table_missing opname src1
          and srctbl2 =
            try Db.find src2 c with
            | Not_found -> table_missing opname src2
          in
          Lens.put lens dsttbl (Some((srctbl1, srctbl2)))
    in
    if src1 <> dst && Db.mem src1 a then table_exists opname src1;
    if src2 <> dst && Db.mem src2 a then table_exists opname src2;
    Db.add src1 newtbl1
      (Db.add src2 newtbl2
        (Db.remove dst a))
  in
  Lens.native getfun putfun

(* Lenses *)

let rename m n = lift_unary "rename" (Rlens.rename m n)

let union = lift_binary "union" (Rlens.generic_union (fun x -> Rlens.Both))
let unionl = lift_binary "unionl" (Rlens.generic_union (fun x -> Rlens.Left))
let unionr = lift_binary "unionr" (Rlens.generic_union (fun x -> Rlens.Right))

let inter = lift_binary "inter" (Rlens.generic_inter (fun x -> Rlens.Both))
let interl = lift_binary "interl" (Rlens.generic_inter (fun x -> Rlens.Left))
let interr = lift_binary "interr" (Rlens.generic_inter (fun x -> Rlens.Right))

let diff = lift_binary "diff" (Rlens.generic_diff (fun x -> Rlens.Both))
let diffl = lift_binary "diffl" (Rlens.generic_diff (fun x -> Rlens.Left))
let diffr = lift_binary "diffr" (Rlens.generic_diff (fun x -> Rlens.Right))

let select k s =
  lift_unary "select" (Rlens.generic_select (
    fun x -> List.assoc k x = s
  ))
let select_eq k1 k2 =
  lift_unary "select_eq" (Rlens.generic_select (
    fun x -> List.assoc k1 x = List.assoc k2 x
  ))

let project p q d = lift_unary "project" (Rlens.project p q d)

let join = lift_binary "join" (Rlens.generic_join (fun x -> Rlens.Both))
let joinl = lift_binary "joinl" (Rlens.generic_join (fun x -> Rlens.Left))
let joinr = lift_binary "joinr" (Rlens.generic_join (fun x -> Rlens.Right))

