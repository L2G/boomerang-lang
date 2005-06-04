(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* name.ml - names and structures built from names *)
(***************************************************)
(* $Id$ *)

type name = string

module NameMap =
  Mapplus.Make(
    struct
      type t = name
      let compare = compare
      let to_string n = n
    end)

type t = name
module Map = NameMap.Map
module Set = NameMap.KeySet

module Hash = Hashtbl.Make(
  struct
    type t = name
    let equal = (==)
    let hash o = Hashtbl.hash (Obj.magic o : int)
  end)
