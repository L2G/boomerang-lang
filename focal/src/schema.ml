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

let is_treeschema = function
  | T _ -> true
  | D _ -> false

let is_dbschema = function
  | T _ -> false
  | D _ -> true

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

