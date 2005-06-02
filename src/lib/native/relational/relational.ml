
open Lens
open Syntax
open Registry
open Value
(*open Library*)

(* helper function for constructing a Focal type error *)
let focal_type_error msg = 
  raise (Error.Fatal_error (Printf.sprintf "run-time sort error in expression %s" msg))

let lift_unary dblens src dst =
  let getfun c =
    let dbc = Treedb.view_to_db c in
    Treedb.db_to_view ((dblens src dst).Dblens.get dbc)
  and putfun a co =
    match co with
    | None -> focal_type_error "Relational.[some unary lens]"
    | Some(c) ->
      let dba = Treedb.view_to_db a in
      let dbc = Treedb.view_to_db c in
      Treedb.db_to_view ((dblens src dst).Dblens.put dba dbc)
  in
  { get = getfun; put = putfun }

let lift_binary dblens src1 src2 dst =
  let getfun c =
    let dbc = Treedb.view_to_db c in
    Treedb.db_to_view ((dblens src1 src2 dst).Dblens.get dbc)
  and putfun a co =
    match co with
    | None -> focal_type_error "Relational.[some binary lens]"
    | Some(c) ->
      let dba = Treedb.view_to_db a in
      let dbc = Treedb.view_to_db c in
      Treedb.db_to_view ((dblens src1 src2 dst).Dblens.put dba dbc)
  in
  { get = getfun; put = putfun }


(* Union with symmetric put *)

let union = lift_binary Dblens.union

let union_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(union n1 n2 n3)
      | _ -> focal_type_error "Native.Relational.union")
      | _ -> focal_type_error "Native.Relational.union")
      | _ -> focal_type_error "Native.Relational.union")

let () =
  register_native "Native.Relational.union" "name -> name -> lens" union_lib


(* Intersection with symmetric put *)

let inter = lift_binary Dblens.inter

let inter_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(inter n1 n2 n3)
      | _ -> focal_type_error "Native.Relational.inter")
      | _ -> focal_type_error "Native.Relational.inter")
      | _ -> focal_type_error "Native.Relational.inter")

let () =
  register_native "Native.Relational.inter" "name -> name -> lens" inter_lib


(* Difference with symmetric put *)

let diff = lift_binary Dblens.diff

let diff_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(diff n1 n2 n3)
      | _ -> focal_type_error "Native.Relational.diff")
      | _ -> focal_type_error "Native.Relational.diff")
      | _ -> focal_type_error "Native.Relational.diff")

let () =
  register_native "Native.Relational.diff" "name -> name -> lens" diff_lib


(* Select *)

let select k s = lift_unary (Dblens.select k s)

let select_lib =
  F(function N(n1) -> F(function N(n2) ->
    F(function N(n3) -> F(function N(n4) -> L(select n1 n2 n3 n4)
        | _ -> focal_type_error "Native.Relational.select")
        | _ -> focal_type_error "Native.Relational.select")
        | _ -> focal_type_error "Native.Relational.select")
        | _ -> focal_type_error "Native.Relational.select")

let () =
  register_native "Native.Relational.select"
    "name -> name -> name -> name -> lens" select_lib


(* Join *)

let join = lift_binary Dblens.join

let join_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(join n1 n2 n3)
      | _ -> focal_type_error "Native.Relational.join")
      | _ -> focal_type_error "Native.Relational.join")
      | _ -> focal_type_error "Native.Relational.join")

let () =
  register_native "Native.Relational.select"
    "name -> name -> name -> lens" join_lib


