(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.ml - Core synchronizer                                  *)
(****************************************************************)
(* $Id *)

let debug = Trace.debug "sync"

let wraptrace s f =
  debug (fun() -> Printf.eprintf "Start %s\n%!" s);
  let r = f() in
  debug (fun() -> Printf.eprintf "Done %s\n%!" s);
  r

type copy_value =
  | Adding of V.t
  | Deleting of V.t

type action =
  | SchemaConflict of Type.t * V.t * V.t 
  | MarkEqual
  | DeleteConflict of V.t * V.t
  | CopyLeftToRight of copy_value
  | CopyRightToLeft of copy_value
  | GoDown of action Name.Map.t

let get_action_name = function
    SchemaConflict _  -> "schema conflict"
  | DeleteConflict _  -> "delete conflict"
  | MarkEqual         -> "equal"
  | CopyLeftToRight _ -> "copy left to right"
  | CopyRightToLeft _ -> "copy right to left"
  | GoDown _          -> "go down"

let rec conflict_free = function
    SchemaConflict _ | DeleteConflict _  -> false
  | MarkEqual | CopyLeftToRight _ | CopyRightToLeft _ -> true
  | GoDown(m) -> Name.Map.for_all conflict_free m

let schema_conflict = function
  | SchemaConflict _ -> true
  | _                -> false

let rec find_conflict a = 
  match a with
      SchemaConflict _ | DeleteConflict _  -> Some a
    | MarkEqual | CopyLeftToRight _ | CopyRightToLeft _ -> None
    | GoDown(m) -> Name.Map.fold 
	(fun k a' opt -> 
	   match opt with
	       None -> find_conflict a'
	     | Some a -> opt) m None

let format_copy s = function
  | Adding v -> V.format_msg [ `String s; `Open_box; `String " Add"; `View v; `Close_box ]
  | Deleting v -> V.format_msg [`String s; `Open_box; `String " Delete"; `View v; `Close_box ]
	
let rec format = function
  | SchemaConflict (t,lv,rv) ->
      V.format_msg ([`Open_box; `String "[SchemaConflict] "; `Break; `String (Type.string_of_t t); `Break; `View lv; `View rv; `Close_box] )
  | GoDown(m) ->
      Name.Map.dump (fun ks -> ks) Misc.whack
        (fun x -> format x)
        (fun _ -> false)
        m
  | MarkEqual -> Format.printf "<MARK EQUAL>"
  | DeleteConflict (v0,v) ->
      Format.printf "<DELETE CONFLICT>@,  @[";
      V.show_diffs v0 v;
      Format.printf "@]@,"
  | CopyLeftToRight c -> format_copy "====>" c
  | CopyRightToLeft c -> format_copy "<====" c

let rec format_without_equal = function
  | SchemaConflict (t,lv,rv) -> 
      V.format_msg ([`String "[SchemaConflict] "; `String (Type.string_of_t t); `View lv; `View rv] )
  | GoDown(m) ->
      let prch (n,ch) = 
	let prf() = Format.printf "@["; format_without_equal ch; Format.printf "@]" in
	  Format.printf "@[<hv1>%s =@ " (Misc.whack n);
	  prf();
	  Format.printf "@]"
      in
	Format.printf "{@[<hv0>";
	let binds = Safelist.map (fun k -> (k, Name.Map.find k m))
		      (Name.Set.elements (Name.Map.domain m)) in
	let binds = Safelist.filter (fun (k,e) -> e <> MarkEqual) binds in
	  Misc.iter_with_sep
	    prch
	    (fun()-> Format.print_break 1 0)
	    binds;
	  Format.printf "@]}"
	    (* the following can only be called from the root *)
  | MarkEqual -> Format.printf "<MARK EQUAL>"
  | DeleteConflict (v0,v) ->
      Format.printf "<DELETE CONFLICT>@,  @[";
      V.show_diffs v0 v;
      Format.printf "@]@,"
  | CopyLeftToRight c -> format_copy "====>" c
  | CopyRightToLeft c -> format_copy "<====" c
      
(* accum : oldacc -> key -> val option -> newacc *)
(* accumulate adds a binding for a view option to an accumulator *)
let accumulate oldacc k = function
    None -> oldacc
  | Some v -> (k, v) :: oldacc

(* N.B.: sync builds not only a worklist structure, but also the new views *)
(* that would result; this lets us rely on the invariants maintained by v.ml *)
(* to detect malformed synchronization results (i.e. ones that are not GOOD *)
(* w.r.t. docs/simple.txt *)

let rec sync' (t:Type.t) archo lefto righto =
  debug (fun()->Printf.eprintf "sync'...\n%!");
  let assert_member v t = 
    debug (fun()->Printf.eprintf "assert_member...\n%!");
    if (not (try (Type.member v t)
	     with Not_found -> assert false))
    then
      begin Format.printf "ERROR: \n view\n\t";
	V.format v;
	Format.printf 
	  "\n is not in type\n\t%s.\n" 
	  (Type.string_of_t t);
	raise (Error.Fatal_error ("Synchronization error: view is not a member of the type."))
      end
    else debug (fun()->Printf.eprintf "done assert_member...\n%!");
  in    
    match (archo, lefto, righto) with
      | _   , None, None -> (MarkEqual, None, None, None)
      | None, None, Some rv -> 
	  assert_member rv t; 
	  (CopyRightToLeft (Adding rv), Some rv, Some rv, Some rv)
      | None, Some lv, None -> 
	  assert_member lv t; 
	  (CopyLeftToRight (Adding lv), Some lv, Some lv, Some lv)
      | Some arv, None, Some rv ->
	  assert_member rv t;
          if V.equal arv rv then
            (CopyLeftToRight (Deleting rv), None, None, None)
          else
            (DeleteConflict(arv,rv), Some arv, None, Some rv)
      | Some arv, Some lv, None ->
	  assert_member lv t;
          if V.equal arv lv then
            (CopyRightToLeft (Deleting lv), None, None, None)
          else
            (DeleteConflict(arv,lv), Some arv, Some lv, None)
      | archo, Some lv, Some rv ->
          assert_member lv t;
	  assert_member rv t;
	  if V.equal lv rv then
            (MarkEqual, Some lv, Some lv, Some rv)
          else
	    let lrkids = Name.Set.union (V.dom lv) (V.dom rv) in
	    let allkids = 
	      match archo with
		| None -> lrkids
		| Some a -> Name.Set.union (V.dom a) lrkids
	    in	      
	    let actbinds, arbinds, lbinds, rbinds = 	      
	      Name.Set.fold
		(fun k (actacc, aracc, lacc, racc) ->
		   let tk = 
		     (match 
			wraptrace 
			  (Printf.sprintf "project %s from %s" k (Type.string_of_t t)) 
			  (fun()-> Type.project t k) 
		      with
			  None ->
			    (* can't happen since every child k is either in  *)
			    (* either dom(a) or dom(b), both of which are in  *)
                            (* T, as we just checked. For debugging, here's a *)
                            (* helpful error message                          *)
			    (Format.printf "ERROR:\nty = %s\n cannot be projected on k=%s\n" 
			       (Type.string_of_t t) k;
			     Format.printf "\no = ";
			     (match archo with
				  None -> Format.printf "NONE"
				| Some o -> V.format o);
			     Format.printf "\na = ";
			     V.format lv;
			     Format.printf "\nb = ";
			     V.format rv);
			    assert false
			| Some tk -> tk)
		   in
		   let act, archo', lefto', righto' =
		     sync 
		       tk
		       (match archo with None -> None | Some av -> V.get av k)
		       (V.get lv k)
		       (V.get rv k)
		   in
		   let aracc = accumulate aracc k archo' in
		   let lacc  = accumulate lacc k lefto' in
		   let racc  =  accumulate racc k righto' in
		     ((k, act)::actacc, aracc, lacc, racc ))
		lrkids 
		([], [], [], [])
	    in
	    let acts,o',a',b' = 
	      actbinds,
	      (V.from_list arbinds),
	      (V.from_list lbinds),
	      (V.from_list rbinds)		
	    in
              debug (fun()->Printf.eprintf "here 1...\n%!");
	      let tdoms  = Type.tdoms t in
              debug (fun()->Printf.eprintf "here 2...\n%!");
	      let a'_in_tdoms = Type.vdom_in_tdoms (V.dom a') tdoms in
	      let b'_in_tdoms = Type.vdom_in_tdoms (V.dom b') tdoms in
              debug (fun()->Printf.eprintf "here 3...\n%!");
              if a'_in_tdoms && b'_in_tdoms then
		(GoDown(Safelist.fold_left
			    (fun acc (k, act) -> Name.Map.add k act acc)
			    Name.Map.empty
			    acts),
		 Some o',
		 Some a',
		 Some b')
	      else
		(* return originals in SchemaConflict *)   
		(SchemaConflict(t,lv,rv),archo,lefto,righto)
		    
and sync (t:Type.t) o a b = 
  (* Version for debugging (because, at the moment, output from the
     debug function looks terrible *)
  (*  V.format_msg [`String "sync:"
      ; `String ("\ndelta: " ^ (Tutil.defs2str delta))
      ; `String ("\nty: " ^ (Tutil.t2str ty))
      ; `View_opt o; `View_opt a; `View_opt b];
  *)
  let result = sync' t o a b in
    result

let rec propagate archo lefto righto = function
| SchemaConflict _ -> archo, lefto, righto
| MarkEqual         -> lefto, lefto, righto  (* may be Some a or None *)
| DeleteConflict _  -> archo, lefto, righto
| CopyLeftToRight _ -> lefto, lefto, lefto
| CopyRightToLeft _ -> righto, righto, righto
| GoDown act_map ->
    let arbinds, lbinds, rbinds =
      Name.Map.fold
        (fun k act (aracc, lacc, racc) ->
          let (archo', lefto', righto') =
            match (archo, lefto, righto) with
                (Some ar, Some lv, Some rv) ->
                  propagate (V.get ar k) (V.get lv k) (V.get rv k) act
              | (None, Some lv, Some rv) ->
                  propagate None (V.get lv k) (V.get rv k) act
              | _ -> assert false (* when we build a GoDown, we have either
                                   (some some some) or (none some some) *)
          in
          let aracc = accumulate aracc k archo' in
          let lacc = accumulate lacc k lefto' in
          let racc = accumulate racc k righto' in
          (aracc, lacc, racc))
        act_map
        ([], [], [])
    in
    let f,s,t =
      (V.from_list arbinds),
      (V.from_list lbinds),
      (V.from_list rbinds)
    in
    (Some f,
     Some s,
     Some t)
