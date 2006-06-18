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
                     Format.printf "@[Run-time error at %s:@ expected a tree but found a database:@ "
                       (Info.string_of_t i);
                     Db.format_t db;
                     Format.printf "@]"))

let db_of i = function
    Db db -> db
  | Tree t -> raise (Error.Harmony_error (fun() ->
                     Format.printf "@[Run-time error at %s:@ expected a database but found a tree:@ "
                       (Info.string_of_t i);
                     Tree.format_t t;
                     Format.printf "@]"))

let equal v w =
  match v, w with
  | Tree t, Tree u -> Tree.equal t u
  | Db d, Db e -> Db.equal d e
  | _, _ -> false

(* ---- random utilities ---- *)
(* TODO: rewrite error message formatting nicely *)

let format_tree_option = function
    None -> Format.printf "NONE";
  | Some v -> Tree.format_t v

let format_msg l = 
  let rec loop = function
    | [] -> ()
    | `String s :: r ->
        Format.printf "%s" s; loop r
    | `Name k :: r ->
        Format.printf "%s" (Misc.whack k); loop r
    | `Break :: r ->
        Format.printf "@,"; loop r
    | `Newline :: r -> 
        Format.print_newline (); loop r
    | `Space :: r ->
        Format.printf "@ "; loop r
    | `SpaceOrIndent :: r ->
        Format.printf "@;<1 2>"; loop r
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
        Format.printf "@[<hv2>";
        loop r
    | `Open_vbox :: r ->
        Format.printf "@[<v2>";
        loop r
    | `Close_box :: r ->
        Format.printf "@]";
        loop r
  in
  Format.printf "@[<hv0>";
  loop l;
  Format.printf "@,@]"

let format_to_string f =
  let out,flush = Format.get_formatter_output_functions () in
  let buf = Buffer.create 64 in
    Format.set_formatter_output_functions 
      (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
    f ();
    Format.print_flush();
    let s = Buffer.contents buf in
      Format.set_formatter_output_functions out flush;
      s

let format_msg_as_string msg = 
  format_to_string (fun () -> format_msg msg)

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

