
open Syntax
open Registry
open Value

(* Tree freezing *)
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
    V.structure_from_list (List.sort V.compare (V.list_from_structure c))
  in
  let putfun a co =
    match co with
    | None -> a
    | Some(c) -> if a = getfun c then c else a
  in
  Lens.native getfun putfun

let () = register_native "Native.listsort" "lens" (L(listsort))

let lift_to_opt f xo =
  match xo with
  | None -> None
  | Some(x) -> Some(f x)


(* Binary relational lenses *)

let lift_binary dblens src1 src2 dst =
  let getfun c =
    let dbc = Treedb.tree_to_db c in
    Treedb.db_to_tree (Lens.get (dblens src1 src2 dst) dbc)
  and putfun a co =
    let dba = Treedb.tree_to_db a in
    let dbco = lift_to_opt Treedb.tree_to_db co in
    Treedb.db_to_tree (Lens.put (dblens src1 src2 dst) dba dbco)
  in
  Lens.native getfun putfun

let make_binary_lib fclname lens =
  mk_nfun "name -> name -> lens" fclname 
    (fun n1 -> mk_nfun "name -> lens" fclname
       (fun n2 -> mk_nfun "lens" fclname
	  (fun n3 -> L(lens n1 n2 n3))))

let register_binary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> lens"
    (make_binary_lib fclname (lift_binary dblens))

let register_tree_tree_binary_dblens fclname dblens =
  register_native fclname "tree -> tree -> name -> name -> name -> lens" (
    mk_vfun "tree -> name -> name -> name -> lens" fclname 
      (fun v1 -> mk_vfun "name -> name -> name -> lens" fclname
	 (fun v2 -> make_binary_lib fclname (lift_binary (dblens v1 v2))))
  )

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

(* Outer join *)
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoin" (
    fun dv1 dv2 ->
      Dblens.ojoin (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoinl" (
    fun dv1 dv2 ->
      Dblens.ojoinl (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )
let () =
  register_tree_tree_binary_dblens "Native.Relational.ojoinr" (
    fun dv1 dv2 ->
      Dblens.ojoinr (Treedb.tree_to_rel dv1) (Treedb.tree_to_rel dv2)
  )


(* Unary relational lenses *)

let lift_unary dblens src dst =
  let getfun c =
    let dbc = Treedb.tree_to_db c in
    Treedb.db_to_tree (Lens.get (dblens src dst) dbc)
  and putfun a co =
    let dba = Treedb.tree_to_db a in
    let dbco = lift_to_opt Treedb.tree_to_db co in
    Treedb.db_to_tree (Lens.put (dblens src dst) dba dbco)
  in
  Lens.native getfun putfun

let make_unary_lib fclname lens =
  mk_nfun "name -> lens" fclname
    (fun n1 -> mk_nfun "lens" fclname
       (fun n2 -> L(lens n1 n2)))

let register_name_name_unary_dblens fclname dblens =
  register_native fclname "name -> name -> name -> name -> lens" (
    mk_nfun "name -> name -> name -> lens" fclname 
      (fun n1' -> mk_nfun "name -> name -> lens" fclname
	 (fun n2' -> make_unary_lib fclname (lift_unary (dblens n1' n2'))))
  )

let register_tree_tree_tree_unary_dblens fclname dblens =
  register_native fclname "tree -> tree -> tree -> name -> name -> lens" (
    mk_vfun "tree -> tree -> name -> name -> lens" fclname 
      (fun v1 -> mk_vfun "tree -> name -> name -> lens" fclname
	 (fun v2 -> mk_vfun "name -> name -> lens" fclname
	    (fun v3 -> make_unary_lib fclname (lift_unary (dblens v1 v2 v3)))))
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
let tree_to_string_list v =
  Name.Set.elements (V.dom v)

let () =
  register_tree_tree_tree_unary_dblens
    "Native.Relational.project" (
      fun fldstree keystree dflttree ->
        Dblens.project
          (tree_to_string_list fldstree)
          (tree_to_string_list keystree)
          (Treedb.tree_to_rel dflttree)
    )

