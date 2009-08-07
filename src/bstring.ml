(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* src/bstring.ml                                                             *)
(* Annotated strings                                                          *)
(* $Id$ *)
(******************************************************************************)

module Rx = Brx
module C = Bcost
module W = Bannot.Weight
module T = Btag
module TmAl = Btag.MapAList
module TmA = Btag.MapA
module TmI = Btag.MapInt
module TmImA = Btag.MapIntMapA
module Is = Int.Set

let error s =
  Berror.run_error (Info.M "bstring.ml") (
    fun () -> print_endline s
  )

type t = string * int * int
    (* s, i, j
       where  0 <= i <= j <= String.length s
    *)
type child =
  | TagNode of T.t * a * int option
      (* the [int option] is always [None] in [at] and [Some] in [cat]
      only for chunk annotations *)
  | Leaf of W.t
and a = (child * int) list
    (* [n1, j1; ...; nk, jk] with s, i, j
       where  i <= j1 <= ... <= jk = j
       and  n1 with s, i, j1
       and  ...
       and  nk with s, j(k-1), jk
       and k >= 1
    *)
type at = a * t  (* every component is abstract *)
type attmp = a list * t (* temporary at *)
type cat = Chunk of at

let empty = ("", 0, 0)

let of_string s =
  String.copy s, 0, String.length s

let to_attmp s :attmp =
  [[]], s
    
let to_string (s, i, j) = String.sub s i (j - i)
  
let of_at (_, s) = s
  
let of_attmp (_, s) = s
  
let of_cat (Chunk at) = of_at at
  
let length (s, i, j) = j - i

let dist s1 s2 =
  let m = String.length s1 in
  let n = String.length s2 in
  let aodd = Array.make (succ n) 0 in
  let aeven = Array.make (succ n) 0 in
  for j=0 to n do aeven.(j) <- j done;
  for i=1 to m do
    let apredi = if i mod 2 = 0 then aodd else aeven in
    let ai = if i mod 2 = 0 then aeven else aodd in
    ai.(0) <- i;
    for j = 1 to n do
      ai.(j) <-
        (if String.get s1 (pred i) = String.get s2 (pred j) then
           min apredi.(j-1)
         else
           (fun x -> x)
        ) (min (apredi.(j) + 1) (ai.(j-1) + 1))
    done
  done;
  if m mod 2 = 0 then aeven.(n) else aodd.(n)

let generic_at_print p (a, (s, i, j)) =
  let buf = Buffer.create 16 in
  let add_sub i j =
    Buffer.add_substring buf s i (j - i)
  in
  let add_str = Buffer.add_string buf in
  add_sub (p add_sub add_str a i) j;
  Buffer.contents buf
    
let at_print_flat s =
  let p add _ a i =
    let rec r add a i =
      match a with
      | [] -> i
      | (Leaf _, j)::a ->
          add i j;
          r add a j
      | (TagNode (_, _, _), j)::a ->
          r add a j
    in
    r add a i
  in
  let r = generic_at_print p s in
(*   print_endline ("at_print_flat: \"" ^ r ^ "\""); *)
  r

let cat_print_flat (Chunk at) = at_print_flat at

let at_print_all =
  let p add_sub add_str =
    let rec p a i =
      match a with
      | [] -> i
      | (Leaf w, j)::a ->
          add_str (Printf.sprintf "{:%s:" (W.to_string w));
          add_sub i j;
          add_str "}";
          p a j
      | (TagNode (tag, r, pos), j)::a ->
          let pos =
            match pos with
            | Some x -> "(" ^ string_of_int x ^ ")"
            | None -> ""
          in
          add_str ("<" ^ T.to_string tag ^ pos ^ ":");
          let i = p r i in
          assert (i = j);
          add_str ">";
          p a j
    in
    p
  in
  generic_at_print p
    
let toplevel_chunks (Chunk at) : int TmAl.t =
  let rec f a i (acc:int TmAl.t) =
    match a with
    | [] -> acc
    | (h, j)::a ->
        let acc =
          match h with
          | TagNode (tag, r, Some pos) ->
              TmAl.add tag pos acc
          | TagNode (_, r, None) ->
              f r i acc
          | Leaf _ ->
              acc
        in
        f a j acc
  in
  let a, (s, i, j) = at in
  let tal = TmAl.rev (f a i TmAl.empty) in
(*   print_endline ("+++++toplevel_chunks: \"" ^ to_string (of_at at) ^ "\""); *)
(*   TmAl.print_list ( *)
(*     fun t -> print_endline (T.to_string t ^ ":") *)
(*   ) ( *)
(*     fun l -> Safelist.iter (fun x -> print_int x; print_string " ") l; print_endline "" *)
(*   ) tal; *)
(*   print_endline "-----toplevel_chunks."; *)
  tal

let cat_fold_on_locs step (Chunk (a, _)) acc =
  let rec f a acc =
    Safelist.fold_left (
      fun acc h ->
        match h with
        | TagNode (tag, r, Some p), _ ->
            f r (step tag p acc)
        | TagNode (_, _, None), _ 
            -> assert false
        | Leaf _, _ -> acc
    ) acc a
  in
  f a acc

(* [Bstring.at_to_locs as] returns [len] describing the number of
chunks for each tag. *)
let at_to_locs (a, _) =
  let rec locs a len =
    Safelist.fold_left (
      fun len h ->
        match h with
        | TagNode (tag, r, _), _ ->
            locs r (TmI.incr tag len)
        | _ -> len
    ) len a
  in
  locs a TmI.empty

let match_rx t (s, i, j) =
  Rx.match_sub_string t s i j

let at_to_weight_flat (a, (s, i, j)) =
  let rec f a i l z =
    match a with
    | [] -> i, l, z
    | (Leaf w, j)::a ->
        f a j (((i, j), w)::l) (z + j - i)
    | (TagNode (_, _, _), j)::a ->
        f a j l z
  in
  let i, l, z = f a i [] 0 in
  assert (i = j);
  let aw = Array.make z W.zero in
  let r, _ =
    Safelist.fold_left (
      fun (r, k) ((i, j), w) ->
        for p = z - k - j + i to z - k - 1 do
          aw.(p) <- w
        done;
        to_string (s, i, j) ^ r, (k + j - i)
    ) ("", 0) l
  in
  aw, r

let at_dist at1 at2 =
  let w1, s1 = at_to_weight_flat at1 in
  let w2, s2 = at_to_weight_flat at2 in
  let m = String.length s1 in
  let n = String.length s2 in
  let aodd = Array.make (succ n) 0 in
  let aeven = Array.make (succ n) 0 in
  for j = 1 to n do
    aeven.(j) <- W.succ_int w2.(pred j) aeven.(pred j)
  done;
  for i = 1 to m do
    let apredi = if i mod 2 = 0 then aodd else aeven in
    let ai = if i mod 2 = 0 then aeven else aodd in
    ai.(0) <- W.succ_int w1.(pred i) apredi.(0);
    for j = 1 to n do
      ai.(j) <-
        (if (String.get s1 (pred i) = String.get s2 (pred j))
           && (w1.(pred i) = w2.(pred j))
         then min apredi.(pred j)
         else (fun x -> x)
        ) (min (W.succ_int w1.(pred i) apredi.(j)) (W.succ_int w2.(pred j) ai.(pred j)))
    done
  done;
  if m mod 2 = 0 then aeven.(n) else aodd.(n)

let cat_dist (Chunk at1) (Chunk at2) =
  at_dist at1 at2

let at_crtdel_cost (a, (s, i, j)) =
  let rec f a i (cs, ct) =
    match a with
    | [] -> i, (cs, ct)
    | (Leaf w, j)::a ->
        let c = (+) (W.weight_int w (j - i)) in
        f a j (c cs, c ct)
    | (TagNode (_, n, _), j)::a ->
        let i, (_, ct) = f n i (cs, ct) in
        assert (i = j);
        f a j (cs, ct)
  in
  let i, c = f a i (0, 0) in
  assert (i = j);
  c

let cat_create_cost (Chunk at) = at_crtdel_cost at

let cat_delete_cost (Chunk at) = at_crtdel_cost at

let rec at_to_chunktree create (a, (s, si, sj)) =
  let rec f a i (acc:a) (cur:TmI.t) (g:cat TmImA.t) =
    match a with
    | [] -> Safelist.rev acc, cur, g
    | (h, j)::a ->
        let h, acc, cur, g =
          match h with
          | Leaf _ -> h, acc, cur, g
          | TagNode (tag, r, None) ->
              let pos = TmI.find tag cur in
              let cur = TmI.incr tag cur in
              let r, cur, g = f r i [] cur g in
              let g = TmImA.add tag pos (Chunk (r, (s, i, j))) g in
              TagNode (tag, r, Some pos), acc, cur, g
          | TagNode (_, _, Some _) -> assert false
        in
        f a j ((h, j)::acc) cur g
  in
  let a, _, g = f a si [] TmI.empty TmImA.empty in
  let crtdel cat =
    (if create
    then cat_create_cost cat
    else cat_delete_cost cat),
    cat
  in
  Chunk (a, (s, si, sj)), TmImA.map crtdel g

let pick_pos n l =
  let rec p l a n =
    match l, n with
    | [], _
    | _, 0 -> a
    | h::l, n -> p l h (pred n)
  in
  p (List.tl l) (List.hd l) n

let find_pos n l =
  let rec p l i =
    match l with
    | [] -> assert false
    | h::_ when h = n -> i
    | _::l -> p l (succ i)
  in
  p l 0

let concat_ambiguous_split n r1 r2 s =
  let p = Rx.split_positions r1 r2 (to_string s) in 
  let l = Int.Set.elements p in
  let s, i, j = s in
  let k = i + pick_pos n l in
  (s, i, k), (s, k, j)

let find_concat_split r1 r2 n s =
  let p = Rx.split_positions r1 r2 s in 
  let l = Int.Set.elements p in
  find_pos n l

let concat_split r1 r2 s =
  let p = Rx.split_positions r1 r2 (to_string s) in 
  if Int.Set.cardinal p <> 1
  then error "concat_split: bad split";
  let s, i, j = s in
  let k = i + Int.Set.choose p in
  (s, i, k), (s, k, j)
    
let star_ambiguous_split ns r s =
  let rs = Rx.mk_star r in
  let r_not_empty = not (Rx.match_string r "") in
  let default = if r_not_empty then 0 else 1 in
  let rec loop ns s acc =
    if (length s = 0) && (r_not_empty || (ns = []))
    then Safelist.rev acc
    else (
      let n, ns =
        match ns with
        | [] -> default, []
        | n::ns -> n, ns
      in
      let x, s = concat_ambiguous_split n r rs s in
      loop ns s (x::acc)
    )
  in
  loop ns s []

let find_star_split r ns s =
  let rs = Rx.mk_star r in
  let rec loop ns s z acc =
    match ns with
    | [] ->
        assert (String.length s = 0);
        Safelist.rev acc
    | n::ns ->
        let i = find_concat_split r rs n s in
        let z = z - n in
        let s = String.sub s n z in
        loop ns s z (i::acc)
  in
  let is = loop ns s (String.length s) [] in
(*   print_string "["; *)
(*   Safelist.fold_left ( *)
(*     fun b i -> *)
(*       if b then print_string ", "; *)
(*       print_int i; *)
(*       true *)
(*   ) false is; *)
(*   print_endline "]"; *)
  is
       
let star_split r s =
  let rs = Rx.mk_star r in
  let pos = Is.remove 0 (Rx.split_positions rs rs (to_string s)) in
  let s, i, j1 = s in
  let l, j2 =
    Int.Set.fold (
      fun j (l, k) ->
        let j = i + j in
        (s, k, j)::l, j
    ) pos ([], i)
  in
  assert (j1 = j2);
  Safelist.rev l
    
(* pre: p1 and p2 only touch annotations *)
let do_concat r1 r2 p1 p2 ((a, s):attmp) :attmp =
  (* post: only annotations are touched *)
  let p = Rx.split_positions r1 r2 (to_string s) in 
(*   if Int.Set.cardinal p <> 1 *)
(*   then ( *)
(*     Berror.run_error (Info.M "bstring.ml: do_concat") ( *)
(*       let fmt _ = Rx.format_t in *)
(*       fun () -> Util.format "@[%a@]\n@[%a@]\n@[\"%s\"@]" fmt r1 fmt r2 (to_string s) *)
(*     ) *)
(*   ); *)
  let s, i, j = s in
  let k = i + Int.Set.choose p in
  let a, _ = p1 (a, (s, i, k)) in
  let a, _ = p2 (a, (s, k, j)) in
  a, (s, i, j)

(* pre: p only touch annotations *)
let do_star r p ((a, s):attmp) :attmp =
  (* post: only annotations are touched *)
  let rs = Rx.mk_star r in
  let pos = Is.remove 0 (Rx.split_positions rs rs (to_string s)) in
  let s, i, j1 = s in
  let a, j2 =
    Int.Set.fold (
      fun j (a, k) ->
        let j = i + j in
        let a, _ = p (a, (s, k, j)) in
        a, j
    ) pos (a, i)
  in
  assert (j1 = j2);
  a, (s, i, j1)

let annot_leaf w ((a, (s, i, j)):attmp) :attmp =
  (* only touch leaf annotation *)
  (match a with
   | h::t -> ((Leaf w, j)::h)::t
   | _ -> error "annot_leaf"),
  (s, i, j)

let before_node ((a, s):attmp) :attmp =
  (* only touch node annotation *)
  []::a,
  s

let annot_node n ((a, (s, i, j)):attmp) :attmp =
  (* only touch node annotation *)
  (match a with
   | h::b::q -> ((TagNode (n, h, None), j)::b)::q
   | _ -> error "annot_node"),
  (s, i, j)

(* pre: [a] is a singleton *)
let at_of_attmp ((a, s):attmp) :at =
  let rec r a =
    Safelist.rev_map (
      function
      | Leaf w, j -> Leaf w, j
      | TagNode (n, a, None), j -> TagNode (n, r a, None), j
      | TagNode (_, _, Some _), _ -> assert false
    ) a
  in
  (match a with
   | [x] -> r x
   | _ -> error "at_of_attmp"),
  s
