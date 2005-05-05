(* $Id: align.ml,v 1.1 2004/09/15 14:54:58 schmitta Exp $ *)
(* aligners are invoked after the lens stack, but before synchronization.
   generally, they are expected not to do much rearrangement of the views on
   which they operate, but rather just to rename some top-level children so
   the synchronizer can operate on them correctly. Of course, aligners just
   look like lenses parameterized by their replica position, so in theory
   they could do more... *)

type pos = Left | Right | Both

type keyfactory = V.t option -> (unit -> Name.t)

(* default key generator, each application really ought to have its own*)
(* keyfactory, but for things we don't plan to align yet, this should be*)
(* fine *)
let default_kf _ =
  let counter = ref 0 in
  (fun () -> incr counter; Printf.sprintf "newkey_%d" !counter)

(* assumes a special string "_" which is not a valid uid for either side and*)
(* is used to represent a uid that needs to be generated *)
let missing_uid = "_"

(* assume this character will not appear in a uid, so we can concatenate *)
(* two uids together to form a pair *)
let uid_sep = '#'

let tr dir bij s =
  match dir with
    Left -> (try Perstrbij.l2r bij s with Not_found -> missing_uid)
  | Right -> (try Perstrbij.r2l bij s with Not_found -> missing_uid)
  | Both -> assert false

let concat dir s s' =
  match dir with
    Left ->
      assert(not(String.contains s uid_sep));
      Printf.sprintf "%s%c%s" s uid_sep s'
  | Right ->
      assert(not(String.contains s' uid_sep));
      Printf.sprintf "%s%c%s" s' uid_sep s
  | Both -> assert false

let uncat s =  
  let words = Misc.splitIntoWords s uid_sep in
  assert((Safelist.length words) = 2);
  match words with
    [s1;s2] -> (s1,s2)
  | _ -> assert false

let split dir s =
  match dir with
    Left -> uncat s
  | Right -> let (s1,s2) = uncat s in (s2,s1)
  | Both -> assert false

(* afin: current file for maintaining bijection
   afout: new file for maintaining bijection
   kgen: new key generator
   pos: is this for the left replica, the right replica, or the archive? *)
let aligner_lens afin afout kgen pos =
  let get vf =
    match pos with
      Both -> vf
    | _ ->
	let lk = Lockfile.must_lock_file afin 5 12 in
	let bij = Perstrbij.from_file afin in
	let ks = Name.Set.elements (V.dom vf) in
	let binds' = 
	  Safelist.map (fun k -> (concat pos k (tr pos bij k), 
				  Some (V.get_required vf k))) ks in
	Lockfile.unlock_file lk;
	V.set_star V.empty binds' 
  in
  let put vf' _ = 
      match pos with
      | Both -> 
          (* we should "unalign" the archive after synchronization, but we need
          to do it after both replicas have been unaligned (and therefore
          have allocated any new uids) *)
	let lk = Lockfile.must_lock_file afout 5 12 in
	let newbij = Perstrbij.from_file afout in
	let kks = Name.Set.elements (V.dom vf') in
	let binds' = Safelist.map
	    (fun kk ->
	      let kl,kr = uncat kk in
	      let kl' = if (kl = missing_uid) then
		tr Right newbij kr else kl in
	      let kr' = if (kr = missing_uid) then
		tr Left newbij kl else kr in
	      (concat Left kl' kr', Some (V.get_required vf' kk)))
	    kks in
	Lockfile.unlock_file lk;
	V.set_star V.empty binds'
    | _ ->
	let lk = Lockfile.must_lock_file afout 5 12 in
	let locks =
(* 	  (if (afin = afout) then []  *)
(* 	  else [Lockfile.must_lock_file afin 5 12]) @ *)
	  [lk] in
	let bij = Perstrbij.from_file afout in
	let kks = Name.Set.elements (V.dom vf') in
	let binds' = Safelist.map
	    (fun kk ->
	      let k,otherk = split pos kk in
	      let newk = if k <> missing_uid then k else kgen () in
	      (* build up new bijection *)
	      begin
		if otherk <> missing_uid then
                  match pos with
                  | Left ->
                      if not (Perstrbij.mem bij (newk,otherk)) then begin
                        Perstrbij.add_1 bij newk otherk
                      end
                  | Right ->
                      if not (Perstrbij.mem bij (otherk,newk)) then begin
                        Perstrbij.add_1 bij otherk newk 
                      end
                  | Both -> assert false
	      end;
	      (newk, Some (V.get_required vf' kk))) kks in
	Perstrbij.to_file afout bij;
	Safelist.iter Lockfile.unlock_file locks;
	V.set_star V.empty binds'
  in
    Lens.native get put



