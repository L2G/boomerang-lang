
open Lens
open Library
open Syntax
open Error

let lift_unary dblens src dst =
  let getfun c =
    let dbc = Treedb.view_to_db c in
    Treedb.db_to_view ((dblens src dst).Dblens.get dbc)
  and putfun a co =
    match co with
    | None -> focal_type_error "relational_plugin"
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
    | None -> focal_type_error "relational_plugin"
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
      | _ -> focal_type_error "rel_union")
      | _ -> focal_type_error "rel_union")
      | _ -> focal_type_error "rel_union")

let union_unit_tests =
  [ (* FIXME *)
  ]

let () =
  register_and_test (
    "rel_union",
    T(Arrow(Name, Arrow(Name, Arrow(Name, Lens)))),
    union_lib)
  union_unit_tests


(* Intersection with symmetric put *)

let inter = lift_binary Dblens.inter

let inter_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(inter n1 n2 n3)
      | _ -> focal_type_error "rel_inter")
      | _ -> focal_type_error "rel_inter")
      | _ -> focal_type_error "rel_inter")

let inter_unit_tests =
  [ (* FIXME *)
  ]

let () =
  register_and_test (
    "rel_inter",
    T(Arrow(Name, Arrow(Name, Arrow(Name, Lens)))),
    inter_lib)
  inter_unit_tests


(* Difference with symmetric put *)

let diff = lift_binary Dblens.diff

let diff_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(diff n1 n2 n3)
      | _ -> focal_type_error "rel_diff")
      | _ -> focal_type_error "rel_diff")
      | _ -> focal_type_error "rel_diff")

let diff_unit_tests =
  [ (* FIXME *)
  ]

let () =
  register_and_test (
    "rel_diff",
    T(Arrow(Name, Arrow(Name, Arrow(Name, Lens)))),
    diff_lib)
  diff_unit_tests


(* Select *)

let select k s = lift_unary (Dblens.select k s)

let select_lib =
  F(function N(n1) -> F(function N(n2) ->
    F(function N(n3) -> F(function N(n4) -> L(select n1 n2 n3 n4)
        | _ -> focal_type_error "rel_join")
        | _ -> focal_type_error "rel_join")
        | _ -> focal_type_error "rel_join")
        | _ -> focal_type_error "rel_join")

let select_unit_tests =
  [ (* FIXME *)
  ]

let () =
  register_and_test (
    "rel_select",
    T(Arrow(Name, Arrow(Name, Arrow(Name, Arrow(Name, Lens))))),
    select_lib)
  select_unit_tests


(* Join *)

let join = lift_binary Dblens.join

let join_lib =
  F(function N(n1) -> F(function N(n2) -> F(function N(n3) -> L(join n1 n2 n3)
      | _ -> focal_type_error "rel_join")
      | _ -> focal_type_error "rel_join")
      | _ -> focal_type_error "rel_join")

let join_unit_tests =
  [ (* FIXME *)
  ]

let () =
  register_and_test (
    "rel_join",
    T(Arrow(Name, Arrow(Name, Arrow(Name, Lens)))),
    join_lib)
  join_unit_tests


