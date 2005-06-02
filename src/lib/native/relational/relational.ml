
open Lens
open Syntax
open Registry
open Value

(* helper function for constructing a Focal type error *)
let focal_type_error msg = 
  raise (Error.Fatal_error (Printf.sprintf "run-time sort error in expression %s" msg))


(* Binary relational lenses *)

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

let make_binary_lib name lens =
  F(function N(n1) ->
    F(function N(n2) ->
      F(function N(n3) ->
        L(lens n1 n2 n3)
        | _ -> focal_type_error name)
      | _ -> focal_type_error name)
    | _ -> focal_type_error name)

let register_binary_dblens name dblens =
  register_native name "name -> name -> name -> lens"
    (make_binary_lib name (lift_binary dblens))

(* Union *)
let () = register_binary_dblens "Native.Relational.union" Dblens.union
let () = register_binary_dblens "Native.Relational.unionl" Dblens.unionl
let () = register_binary_dblens "Native.Relational.unionr" Dblens.unionr
(* Intersection *)
let () = register_binary_dblens "Native.Relational.inter" Dblens.inter
let () = register_binary_dblens "Native.Relational.interl" Dblens.interl
let () = register_binary_dblens "Native.Relational.interr" Dblens.interr
(* Difference *)
let () = register_binary_dblens "Native.Relational.diff" Dblens.diff
let () = register_binary_dblens "Native.Relational.diffl" Dblens.diffl
let () = register_binary_dblens "Native.Relational.diffr" Dblens.diffr
(* Join *)
let () = register_binary_dblens "Native.Relational.join" Dblens.join


(* Select *)

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

