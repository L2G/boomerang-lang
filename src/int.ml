(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* int.ml - structures of ints                           *)
(*********************************************************)
(* $Id$ *)

type t = int 

module type SET = sig include Set.S end

module M =
  Mapplus.Make(
    struct
      type t = int
      let compare = compare
      let to_string = string_of_int
    end)

module Map = M.Map

module SetSet = 
  Set.Make(
    struct
      type t = M.KeySet.t
      let compare = M.KeySet.compare
    end)

module Set = M.KeySet

module Hash = 
  Hashtbl.Make(
    struct
      type t = int
      let equal = (=)
      let hash = Hashtbl.hash
    end)
