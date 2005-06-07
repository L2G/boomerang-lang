
open Syntax
open Registry
open Value

(* View freezing *)
let freeze =
  let getfun c =
    V.new_value (Surveyor.get_writer "meta" c)
  and putfun a co =
    Surveyor.get_reader "meta" (V.get_value a)
  in
  Lens.native getfun putfun

let () = register_native "Native.freeze" "lens" (L(freeze))

(*
(* Bush making *)
let makebush =
  let getfun c =
*)

(* List sorting *)
let listsort =
  let getfun c =
    V.structure_from_list (List.sort compare (V.list_from_structure c))
  in
  let putfun a co =
    match co with
    | None -> a
    | Some(c) -> if a = getfun c then c else a
  in
  Lens.native getfun putfun

let () = register_native "Native.listsort" "lens" (L(listsort))

(* helper function for constructing a Focal type error *)
let focal_type_error msg = 
  raise (Error.Fatal_error (Printf.sprintf "run-time sort error in expression %s" msg))

let lift_to_opt f xo =
  match xo with
  | None -> None
  | Some(x) -> Some(f x)


(* Binary relational lenses *)

let lift_binary dblens src1 src2 dst =
  let getfun c =
    let dbc = Treedb.view_to_db c in
    Treedb.db_to_view (Lens.get (dblens src1 src2 dst) dbc)
  and putfun a co =
    let dba = Treedb.view_to_db a in
    let dbco = lift_to_opt Treedb.view_to_db co in
    Treedb.db_to_view (Lens.put (dblens src1 src2 dst) dba dbco)
  in
  Lens.native getfun putfun

let make_binary_lib fclname lens =
  F(function N(n1) ->
    F(function N(n2) ->
      F(function N(n3) ->
        L(lens n1 n2 n3)
        | _ -> focal_type_error fclname)
      | _ -> focal_type_error fclname)
    | _ -> focal_type_error fclname)

let register_binary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> lens"
    (make_binary_lib fclname (lift_binary dblens))

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
let () = register_binary_dblens "Native.Relational.joinl" Dblens.joinl
let () = register_binary_dblens "Native.Relational.joinr" Dblens.joinr


(* Unary relational lenses *)

let lift_unary dblens src dst =
  let getfun c =
    let dbc = Treedb.view_to_db c in
    Treedb.db_to_view (Lens.get (dblens src dst) dbc)
  and putfun a co =
    let dba = Treedb.view_to_db a in
    let dbco = lift_to_opt Treedb.view_to_db co in
    Treedb.db_to_view (Lens.put (dblens src dst) dba dbco)
  in
  Lens.native getfun putfun

let make_unary_lib fclname lens =
  F(function N(n1) ->
    F(function N(n2) ->
      L(lens n1 n2)
      | _ -> focal_type_error fclname)
    | _ -> focal_type_error fclname)

let register_name_name_unary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> name -> lens" (
    F(function N(n1') ->
      F(function N(n2') ->
        make_unary_lib fclname (lift_unary (dblens n1' n2'))
        | _ -> focal_type_error fclname)
      | _ -> focal_type_error fclname)
  )

let register_view_view_view_unary_dblens fclname dblens =
  register_native fclname "view -> view -> view -> name -> name -> lens" (
    F(function V(v1) ->
      F(function V(v2) ->
        F(function V(v3) ->
          make_unary_lib fclname (lift_unary (dblens v1 v2 v3))
          | _ -> focal_type_error fclname)
        | _ -> focal_type_error fclname)
      | _ -> focal_type_error fclname)
  )

(* Rename *)
let () =
  register_name_name_unary_dblens
    "Native.Relational.rename" Dblens.rename

(* Select *)
let () =
  register_name_name_unary_dblens
    "Native.Relational.select" Dblens.select
let () =
  register_name_name_unary_dblens
    "Native.Relational.select_eq" Dblens.select_eq

(* Project *)
let view_to_string_list v =
  Name.Set.elements (V.dom v)

let () =
  register_view_view_view_unary_dblens
    "Native.Relational.project" (
      fun fldsview keysview dfltview ->
        Dblens.project
          (view_to_string_list fldsview)
          (view_to_string_list keysview)
          (Treedb.view_to_rel dfltview)
    )

