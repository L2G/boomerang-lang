(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* schema.ml - combined schema representation                *)
(*************************************************************)
(* $Id: treeschema.ml 1756 2006-05-31 15:08:03Z bohannon $ *)

type t =
  | T of Treeschema.t
  | D of Dbschema.t

let format_t = function
  | T s -> Treeschema.format_t s
  | D s -> Dbschema.format_t s

let treeschema t = T t
let dbschema d = D d

let treeschema_of i = function
    T ts -> ts
  | D dbs -> raise (Error.Harmony_error (fun() ->
                     Util.format "@[Run-time error at %s:@ expected a tree schema but found a database schema:@ "
                       (Info.string_of_t i);
                     Dbschema.format_t dbs;
                     Util.format "@]"))

let dbschema_of i = function
    D db -> db
  | T ts -> raise (Error.Harmony_error (fun() ->
                     Util.format "@[Run-time error at %s:@ expected a database schema but found a tree schema:@ "
                       (Info.string_of_t i);
                     Treeschema.format_t ts;
                     Util.format "@]"))

let treeschema_of_internal m1 m2 = function
  | T s -> s
  | D s -> raise (Error.Harmony_error
             (fun () -> Util.format "@[%s requires %s but found: @ " m1 m2;
              Dbschema.format_t s;
              Util.format "@]"))

let t1 m f =
  fun s -> f (treeschema_of_internal m "a tree schema" s)

let t2 m f =
  fun s1 s2 -> 
    f (treeschema_of_internal m "tree schemas" s1) 
      (treeschema_of_internal m "tree schemas" s2)

let tfm m f = 
  fun s1 fm -> 
    f (treeschema_of_internal m "tree schemas" s1)
      fm

let tlist m f =
  fun l -> f (Safelist.map (fun s -> treeschema_of_internal m "tree schemas" s) l)

let equivalent s1 s2 =
  match s1, s2 with
  | T ts1, T ts2 -> Treeschema.equivalent ts1 ts2
  | D ds1, D ds2 -> Dbschema.equiv ds1 ds2
  | _, _ -> false

let subschema s1 s2 =
  match s1, s2 with
  | T ts1, T ts2 -> Treeschema.subschema ts1 ts2
  | D ds1, D ds2 -> Dbschema.includes ds2 ds2
  | _, _ -> false

let member v s =
  (* FIXME: member will throw a Harmony_error when v is a tree and s is a
     database schema *)
  match v, s with
  | V.Tree t, T ts -> Treeschema.member t ts
  | V.Db d, D ds -> Dbschema.member d ds
  | _, _ -> false

let mark_tvars = Treeschema.mark_tvars
let finalize = Treeschema.finalize
let update s = t1 "update" (Treeschema.update s)

let mk_any = T (Treeschema.mk_any)
let mk_atom n s = T (t1 "atom schema" (Treeschema.mk_atom n) s)
let mk_cat l = T (tlist "concatenation of schemas" Treeschema.mk_cat l)
let mk_union l = T (tlist "union of schemas" Treeschema.mk_union l)
let mk_var n = T (Treeschema.mk_var n)
let mk_wild ns k b s = T (t1 "wildcard schema" (Treeschema.mk_wild ns k b) s)
let mk_neg s = T (t1 "schema negation" Treeschema.mk_neg s)
let mk_isect l = T (tlist "intersection of schemas" Treeschema.mk_isect l)
let mk_diff s1 s2 = T (t2 "difference of schemas" Treeschema.mk_diff s1 s2)
let mk_nil = T (Treeschema.mk_nil)
let mk_cons s1 s2 = T (t2 "cons" Treeschema.mk_cons s1 s2)
let t_of_tree t = T (Treeschema.t_of_tree t)

let msg = "[INTERNAL ERROR]"

let empty = t1 msg Treeschema.empty
let dom_member ns = t1 msg (Treeschema.dom_member ns)

let lift_option f x =
  match x with
  | None -> None
  | Some y -> Some (f y)

let project n s = lift_option (fun s -> T s) (t1 msg (Treeschema.project n) s)
let project_all s = lift_option (fun s -> T s) (t1 msg (Treeschema.project_all) s)

let inject s1 s2 = T (t2 "inject" Treeschema.inject s1 s2)
let inject_map s1 fm = T (tfm "inject_map" Treeschema.inject_map s1 fm)

let restrict ns s =
  let s1, s2 = t1 msg (Treeschema.restrict ns) s in 
  (T s1, T s2)

