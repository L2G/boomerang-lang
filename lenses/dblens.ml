
module Database = Map.Make(String)

(* Functions to lift lenses on relations to lenses on databases *)

let table_missing fclname tblname =
  Lens.error
    [ `String(fclname^":")
    ; `Space; `String("no such table:")
    ; `Space; `String(tblname)
    ]

let table_exists fclname tblname =
  Lens.error
    [ `String(fclname^":")
    ; `Space; `String("table already exists:")
    ; `Space; `String(tblname)
    ]

let lift_unary fclname lens src dst =
  let getfun c =
    let srctbl =
      try Database.find src c with Not_found -> table_missing fclname src in
    if dst <> src && Database.mem dst c then table_exists fclname dst;
    Database.add dst (Lens.get lens srctbl)
      (Database.remove src c)
  and putfun a co =
    let dsttbl =
      try Database.find dst a with Not_found -> table_missing fclname dst in
    let newtbl =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl =
            try Database.find src c with
            | Not_found -> table_missing fclname src
          in
          Lens.put lens dsttbl (Some(srctbl))
    in
    if src <> dst && Database.mem src a then table_exists fclname src;
    Database.add src newtbl
      (Database.remove dst a)
  in
  Lens.native getfun putfun

let lift_binary fclname lens src1 src2 dst =
  let getfun c =
    let srctbl1 =
      try Database.find src1 c with Not_found -> table_missing fclname src1 in
    let srctbl2 =
      try Database.find src2 c with Not_found -> table_missing fclname src2 in
    if dst <> src1 && dst <> src2 && Database.mem dst c then table_exists fclname dst;
    Database.add dst (Lens.get lens (srctbl1, srctbl2))
      (Database.remove src1
        (Database.remove src2 c))
  and putfun a co =
    let dsttbl =
      try Database.find dst a with Not_found -> table_missing fclname dst in
    let newtbl1, newtbl2 =
      match co with
      | None -> Lens.put lens dsttbl None
      | Some(c) ->
          let srctbl1 =
            try Database.find src1 c with
            | Not_found -> table_missing fclname src1
          and srctbl2 =
            try Database.find src2 c with
            | Not_found -> table_missing fclname src2
          in
          Lens.put lens dsttbl (Some((srctbl1, srctbl2)))
    in
    if src1 <> dst && Database.mem src1 a then table_exists fclname src1;
    if src2 <> dst && Database.mem src2 a then table_exists fclname src2;
    Database.add src1 newtbl1
      (Database.add src2 newtbl2
        (Database.remove dst a))
  in
  Lens.native getfun putfun

(* Lenses *)

let rename m n = lift_unary "rename" (Rlens.rename m n)

let union b = lift_binary "union" (Rlens.union b)
let inter b = lift_binary "inter" (Rlens.inter b)
let diff b = lift_binary "diff" (Rlens.diff b)

(*
let union = lift_binary "union" (Rlens.union (fun x -> Rlens.Both))
let unionl = lift_binary "unionl" (Rlens.union (fun x -> Rlens.Left))
let unionr = lift_binary "unionr" (Rlens.union (fun x -> Rlens.Right))

let inter = lift_binary "inter" (Rlens.inter (fun x -> Rlens.Both))
let interl = lift_binary "interl" (Rlens.inter (fun x -> Rlens.Left))
let interr = lift_binary "interr" (Rlens.inter (fun x -> Rlens.Right))

let diff = lift_binary "diff" (Rlens.diff (fun x -> Rlens.Both))
let diffl = lift_binary "diffl" (Rlens.diff (fun x -> Rlens.Left))
let diffr = lift_binary "diffr" (Rlens.diff (fun x -> Rlens.Right))
*)

let select p = lift_unary "select" (Rlens.select p)

(*
let select k s =
  lift_unary "select" (Rlens.select (
    fun x -> List.assoc k x = s
  ))
*)
let select_eq k1 k2 =
  lift_unary "select_eq" (Rlens.select (
    fun x -> List.assoc k1 x = List.assoc k2 x
  ))

let project p q d = lift_unary "project" (Rlens.project p q d)

(*
let join = lift_binary "join" (Rlens.join (fun x -> Rlens.Both))
let joinl = lift_binary "joinl" (Rlens.join (fun x -> Rlens.Left))
let joinr = lift_binary "joinr" (Rlens.join (fun x -> Rlens.Right))
*)
let ijoin b = lift_binary "ijoin" (Rlens.ijoin b)

let truefun _ = true

(*
let ojoin dl dr =
  lift_binary "ojoin" (
    Rlens.ojoin dl dr truefun truefun (fun _ -> Rlens.Both))
let ojoinl dl dr =
  lift_binary "ojoinl" (
    Rlens.ojoin dl dr truefun truefun (fun x -> Rlens.Left))
let ojoinr dl dr =
  lift_binary "ojoinr" (
    Rlens.ojoin dl dr truefun truefun (fun x -> Rlens.Right))
*)
let ojoin dl dr pl pr b =
  lift_binary "ojoin" (Rlens.ojoin dl dr pl pr b)

