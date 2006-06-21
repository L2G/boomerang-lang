(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: v.ml 1756 2006-05-31 15:08:03Z bohannon $ *)

type t =
  | Tree of Tree.t
  | Db of Db.t

let format_t = function
  | Tree t -> Tree.format_t t
  | Db d -> Db.format_t d

let string_of_t = function
  | Tree t -> Tree.string_of_t t
  | Db d -> Db.string_of_t d

let tree_of i = function
    Tree t -> t
  | Db db -> raise (Error.Harmony_error (fun() ->
                     Util.format "@[Run-time error at %s:@ expected a tree but found a database:@ "
                       (Info.string_of_t i);
                     Db.format_t db;
                     Util.format "@]"))

let db_of i = function
    Db db -> db
  | Tree t -> raise (Error.Harmony_error (fun() ->
                     Util.format "@[Run-time error at %s:@ expected a database but found a tree:@ "
                       (Info.string_of_t i);
                     Tree.format_t t;
                     Util.format "@]"))

let equal v w =
  match v, w with
  | Tree t, Tree u -> Tree.equal t u
  | Db d, Db e -> Db.equal d e
  | _, _ -> false

(* ---- random utilities ---- *)
(* TODO: rewrite error message formatting nicely *)

let format_tree_option = function
    None -> Util.format "NONE";
  | Some v -> Tree.format_t v

let format_msg l = 
  let rec loop = function
    | [] -> ()
    | `String s :: r ->
        Util.format "%s" s; loop r
    | `Name k :: r ->
        Util.format "%s" (Misc.whack k); loop r
    | `Break :: r ->
        Util.format "@,"; loop r
    | `Newline :: r -> 
        Util.format "@\n"; loop r
    | `Space :: r ->
        Util.format "@ "; loop r
    | `SpaceOrIndent :: r ->
        Util.format "@;<1 2>"; loop r
    | `V v :: r ->
        format_t v;
        loop r
    | `Db v :: r ->
        Db.format_t v;
        loop r
    | `Tree v :: r ->
        Tree.format_t v;
        loop r
    | `Tree_opt v :: r ->
        format_tree_option v;
        loop r
    | `Prim f ::r -> 
        f ();
        loop r
    | `Open_box :: r ->
        Util.format "@[<hv2>";
        loop r
    | `Open_vbox :: r ->
        Util.format "@[<v2>";
        loop r
    | `Close_box :: r ->
        Util.format "@]";
        loop r
  in
  Util.format "@[<hv0>";
  loop l;
  Util.format "@,@]"

let format_msg_as_string msg = 
  Util.format_to_string (fun () -> format_msg msg)

type msg = [`String of string | `Name of Name.t | `Newline | `Break | `Space 
           | `SpaceOrIndent | `Tree of Tree.t | `Tree_opt of Tree.t option
           | `Db of Db.t | `V of t 
           | `Prim of (unit -> unit) | `Open_box | `Open_vbox | `Close_box ]

let error_msg l = raise (Error.Harmony_error (fun () -> format_msg l))

(* Hashes of trees *)
type thist = t (* hack to avoid cyclic type in Hash functor application *)
module Hash =
  Hashtbl.Make(
    struct
      type t = thist
      let equal = (==)                                (* Use physical equality test *)
      let hash = function
        (* Hash on tag and physical addr *)
        | Tree t -> Hashtbl.hash (1, (Obj.magic t : int))
        | Db db -> Hashtbl.hash (2, (Obj.magic db : int))
    end)

