(* $Id: perstrbij.ml,v 1.1 2004/09/15 14:54:58 schmitta Exp $ *)
(* persistent string bijections *)

(* assumption: for any string not mentioned in the file, the bijection acts *)
(* as the identity. Therefore, what is contained in the file must represent *)
(* a permutation. *)
type t = (string, string) Hashtbl.t 	(* translating left->right *)
      * (string, string) Hashtbl.t	(* translating right->left *)

(* empty bijection *)
let create sz = (Hashtbl.create sz, Hashtbl.create sz)

(* check that both consituent hashtables are bijections *)
let check_bij (l2r,r2l) =
  (* subset true l1 l2 determines whether l2 is a subset of l1 *)
  let rec subset acc l1 = function
      [] -> acc
    | x::rest -> subset (acc && (Safelist.mem x l1)) l1 rest in
  (* checking for permutation: get domain and range; if they are subsets of
     one another then they are equal and thus a bijection. if not injective
     or surjective, domain not a subset of range. *)
  let check_bij_ht ht1 ht2 =
    let get_keys_vals ht = Hashtbl.fold
	(fun k v (kacc,vacc) -> 
	  ((if (Safelist.mem k kacc) then assert false else (k::kacc)),
	   (if (Safelist.mem v vacc) then assert false else (v::vacc)))) ht ([],[]) in
    let k1,v1 = get_keys_vals ht1 in
    let k2,v2 = get_keys_vals ht2 in
    (subset true k1 v2) && (subset true v1 k2) in
  check_bij_ht l2r r2l

(* throw away current mappings *)
let clear (l2r,r2l) =
  Hashtbl.clear l2r;
  Hashtbl.clear r2l

(* the mappings are written out in pairs of lines (recall that Misc.whack *)
(* turns newlines into \n). writing just records the current bindings in *)
(* the appropriate file, without releasing the lock *)
let to_file fname ((l2r,r2l) as bij) =
  assert(check_bij bij);
  let ch = open_out fname in
  Hashtbl.iter
    (fun l r -> 
      output_string ch (Misc.whack l);
      output_string ch "\n";
      output_string ch (Misc.whack r);
      output_string ch "\n") l2r;
  close_out ch

(* read the pairs of lines, unwhack them, and store in table *)
let rec read_from_ch ch (l2r,r2l) =
  try
    let l = Misc.unwhack (input_line ch) in
    (try
      let r = Misc.unwhack (input_line ch) in
      (* by refusing to overwrite existing bindings here, we guarantee the
	 contents of the file represent a bijection *)
      Misc.safe_hash_add l2r l r;
      Misc.safe_hash_add r2l r l;
      read_from_ch ch (l2r,r2l)
    with 
    | End_of_file ->
        raise (Failure "Perstrbij.read_from_ch: odd number of lines in file")
    | Failure s ->
        failwith ("Perstrbij.read_from_ch: "^s))
  with End_of_file -> ()

(* read from a file *)
let from_file fname =
  let ch = open_in fname in
  let bij = create 10 in
  read_from_ch ch bij;
  assert(check_bij bij);
  bij

(* translation functions *)
let l2r (l2r,_) s = Hashtbl.find l2r s
let r2l (_,r2l) s = Hashtbl.find r2l s

(* check for binding *)
let mem (l2r,r2l) (s1,s2) = Hashtbl.mem l2r s1 || Hashtbl.mem r2l s2

(* add bindings *)
let add_1 (l2r,r2l) l r =
  try
    Misc.safe_hash_add l2r l r;
    Misc.safe_hash_add r2l r l
  with
    | Failure s ->
        failwith ("Perstrbij.add: ("^l^" , "^r^"): "^s)

let rec add bij = function
    [] -> assert(check_bij bij)
  | (l,r)::rest -> (add_1 bij l r; add bij rest)

(* remove bindings *)
let del_1 ((l2r,r2l) as bij) l r =
  assert(((not (Hashtbl.mem l2r l)) && (not (Hashtbl.mem r2l r)))
  || ((Hashtbl.mem l2r l && r = Hashtbl.find l2r l) &&
      (Hashtbl.mem r2l r && l = Hashtbl.find r2l r)));
  Hashtbl.remove l2r l;
  Hashtbl.remove r2l r

let rec del bij = function
    [] -> assert(check_bij bij)
  | (l,r)::rest -> (del_1 bij l r; del bij rest)
