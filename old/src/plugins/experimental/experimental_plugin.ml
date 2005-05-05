open Lens
open Syntax
open Library
open Pervasives_plugin

(* eqfork *)
let eqfork_arg = 
  Compiler.compile_focal 
    "let eqfork veq l =
       acond (equal veq) (equal veq)
         id 
         l 
     do eqfork"
let eqfork veq l =
  let r = eqfork_arg in
  let r = (funOfArg r) (V veq) in
  let r = (funOfArg r) (L l) in
    lensOfArg r
let _ = register ("eqfork",
                  T (Arrow (View, Arrow (Lens, Lens))),
                  eqfork_arg)
	  
(* efork *)
let efork_arg = 
  Compiler.compile_focal 
    "let efork = eqfork {}
     do efork"
let efork l =
  let r = efork_arg in
  let r = (funOfArg r) (L l) in
    lensOfArg r
      
let _ = register ("efork",
                  T (Arrow (Lens, Lens)),
                  efork_arg)

(* exfork *)
let exfork_arg = Compiler.compile_focal "let exfork vc va l1 l2 =
                                           acond (equalDom vc) (equalDom va)
                                             l1 l2 
                                         do exfork"
let exfork vc va l1 l2 =
  let r = exfork_arg in
  let r = (funOfArg r) (V vc) in
  let r = (funOfArg r) (V va) in
  let r = (funOfArg r) (L l1) in
  let r = (funOfArg r) (L l2) in
  lensOfArg r
let _ = register ("exfork",
                  T (Arrow (View, Arrow (View, Arrow(Lens, Arrow(Lens,
  Lens))))),
                  exfork_arg)
  
(* Promote *)
let promote_arg = 
  Compiler.compile_focal
    "let promote m n = 
       xfork {m} {m,n} 
         (hoist m; fork {n} id (plunge m))
         id
     do promote"

let promote m n = 
  let r = promote_arg in
  let r = (funOfArg r) (N m) in
  let r = (funOfArg r) (N n) in
    lensOfArg r

let _ = register("promote", T(Arrow(Name, Arrow(Name, Lens))), promote_arg)

(* Promote1 *)
(* I think we need a (limited) binding lens to write promote1 as a
   derived lens *)
let promote1 kk =
  { get = 
      (fun c ->
	 let dom = V.dom c in
	   if (Name.Set.cardinal dom) <> 1 then
             error [`String "Lens.promote1(get): expecting exactly one child"; 
                    `View c];
	   let k = Name.Set.choose dom in
	   let kid = V.get_required c k in
	   let kkid = V.get kid kk in
	   let kid' = V.set kid kk None in
	     V.set (V.set c k (Some kid')) kk kkid);
    put = 
      (fun a c ->
	 let dom = Name.Set.remove kk (V.dom a) in
	   if (Name.Set.cardinal dom) <> 1 then error [
             `String "Lens.promote1(get): expecting exactly one child not named ";
             `Name kk; `View a];
	   let k = Name.Set.choose dom in
	   let kid = V.get_required a k in
	   let kkid = V.get a kk in
	   let kid' = V.set kid kk kkid in
	     V.set (V.set a kk None) k (Some kid')	       
      )
  }

let promote1_arg = 
  F (function N n -> L (promote1 n)
       | _ -> Error.focal_type_error "promote1")
let _ = register ("promote1", T(Arrow(Name, Lens)), promote1_arg)

(* Flatten_step *)
let flatten_step = 
  {get = 
     (fun c -> 
	(* sanity check *)
 	if ((Name.Set.cardinal (V.dom c) <> 2) 
	    || (V.get c V.head_tag = None)
	    || (V.get c V.tail_tag = None))
	then error [`String "flatten_step: expected a view with children: "; 
		    `Name V.head_tag; `String " and "; `Name V.tail_tag];
	let hd,tl = (V.get_required c V.head_tag, V.get_required c V.tail_tag) in
	let rec compute_flatten b1 b2 acc = 
	  let bo = 
	    (* identify a non-empty bush *)
 	    if (not (V.is_empty b1)) then (Some b1)
	    else if (not (V.is_empty b2)) then (Some b2)
	    else None 
	  in
	    match bo with
	      | None -> acc
	      | Some b -> 
		  let k = Name.Set.choose (V.dom b) in
		  let b1', b2' = (V.set b1 k None, V.set b2 k None) in
		  let tk1, tk2 = (V.get b1 k, V.get b2 k) in
		  let tk = 
		    match tk1, tk2 with
		      | Some t1, None    -> V.set (V.set V.empty V.tail_tag (Some V.empty)) V.head_tag (Some t1)
		      | None, Some t2    -> t2
		      | Some t1, Some t2 -> V.set (V.set V.empty V.tail_tag (Some t2)) V.head_tag (Some t1)
		      | _                -> assert false			
		  in
		    compute_flatten b1' b2' (V.set acc k (Some tk))
	in
	  compute_flatten hd tl V.empty
     );
   put = 
     (fun a co -> 
	let c = match co with 
	  | None -> V.empty
	  | Some c -> c in
	let grab_hd_tl v = 
	  match (V.get v V.head_tag,V.get v V.tail_tag) with
	    | Some h,Some t -> h,t
	    | _             -> 
		error [`String "flatten_step (put): expected "; `Name V.head_tag
		      ; `String " and "; `Name V.tail_tag; `String " : "; `View v] 
	in
	let inter =
	  match (V.get c V.head_tag) with
	    | None -> Name.Set.empty
	    | Some h -> Name.Set.inter (V.dom h) (V.dom a) in
	let compute_unflatten =
	  if (V.is_empty a) then a
	  else
 	    if (Name.Set.is_empty inter) then (* or V.is_empty c*)
	      (* grab one kid from abs, put it's hd under *h *)
 	      let k = Name.Set.choose (V.dom a) in
	      let tk = V.get_required a k in
	      let (tkHd,tkTl) = grab_hd_tl tk in
	      let aRest = V.set a k (if V.is_empty tkTl then None else (Some tkTl)) in
		V.set (V.set V.empty V.tail_tag (Some aRest)) V.head_tag 
		  (Some (V.set V.empty k (Some tkHd)))
	    else
	      let rec doHd ks acc = 
		if Name.Set.is_empty ks then acc
		else
		  let k = Name.Set.choose ks in
		  let ak = V.get_required a k in
		  let oldHd,oldTl = grab_hd_tl acc in
		  let akhd,aktl = grab_hd_tl ak in
		  let newHd = V.set oldHd k (Some akhd) in
		  let newTl = V.set oldTl k (if V.is_empty aktl then None else (Some aktl)) in
		    doHd 
		      (Name.Set.remove k ks)
		      (V.set (V.set acc V.tail_tag (Some newTl)) V.head_tag (Some newHd))
	      in
	        doHd inter (V.set (V.set V.empty V.tail_tag (Some a)) V.head_tag (Some V.empty))
	in
	  compute_unflatten 	    
     )
  }

let flatten_step_arg = L flatten_step
let _ = register("flatten_step", T(Lens), flatten_step_arg)

(* TIDY: AAAIIIIEEEEE! *)
let flatten_unique, flatten_unique_all =
  let check_list direction v =
    if (not (V.is_list v)) then
      error [`String "Lens.flatten(";
             `String direction;
             `String "): was expecting a list, got: ";
	     `View v];
    let kids = V.list_from_structure v in
    let _ =
      Safelist.fold_left
        (fun acc kid -> 
          let dom = V.dom kid in
          if Name.Set.is_empty dom then
            error [
              `String "Lens.flatten("; `String direction;
              `String "): one element is the empty view:";
              `View v]
          else
          let i = Name.Set.inter acc dom in
          if Name.Set.is_empty i then
            Name.Set.union acc dom
          else
            let istr =
             String.concat ", " (Safelist.map Misc.whack (Name.Set.elements i))
             in
            error [
              `String "Lens.flatten("; `String direction;
              `String "): some of the list elements ";
	      `String "do not have disjoint domains (multiple occurrences of ";
              `String istr; `String ")"; `View v];)
         Name.Set.empty kids
    in
    kids
  in
  let get v = 
    (
      let kids = check_list "get" v in
      Safelist.fold_left (fun vacc kid -> V.concat vacc kid) V.empty kids)
      in
  let put extraf v' v = 
    (
      let v =
        match v with
        | None -> V.empty_list
        | Some vv -> vv
      in
      let kids = check_list "put" v in
    (* for each element of the original list, see if it is in the new
       abstract view. if so, retain the new version, else discard
       it. remember any leftover elements of the new abstract view's domain *)
      let kids'rev,leftover =
        Safelist.fold_left (
          fun (kids'acc,dom) kid ->
            let inter = Name.Set.inter dom (V.dom kid) in
            if Name.Set.is_empty inter then
              (kids'acc, dom)
            else 
              let kkid' = 
                V.set_star V.empty (
                  Name.Set.fold (
                    fun k acc -> (k,Some (V.get_required v' k))::acc)
                  inter []) in
	  (kkid'::kids'acc, Name.Set.diff dom inter))
	([],V.dom v') kids in
    (* take the remaining elements in some order and put at the end of the
       new concrete list *)
    let kids' = Safelist.rev_append kids'rev (extraf leftover v') in
    V.structure_from_list kids'
    )
    in
  let extra_single leftover v' =
    let leftoverk's = Safelist.sort compare (Name.Set.elements leftover) in
    Safelist.map (fun k -> V.set V.empty k (Some (V.get_required v' k))) leftoverk's
  in
  let extra_all leftover v' =
    if Name.Set.is_empty leftover then [] else
    [Name.Set.fold (fun k -> fun acc -> V.set acc k (Some (V.get_required v' k))) leftover V.empty]
  in
  { get = get; put = put extra_single },
  { get = get; put = put extra_all }

let flatten_unique_arg = L flatten_unique
let _ = register("flatten_unique", T(Lens), flatten_unique_arg)

let flatten_unique_all_arg = L flatten_unique_all
let _ = register("flatten_unique_all", T(Lens), flatten_unique_all_arg)

(* list_fork *)
let list_fork_arg =
  Compiler.compile_focal
    "let list_fork phd lhd ltl =
      xfork {*h} phd
        (hoist *h; lhd)
        (focus *t {}; ltl)
     do list_fork"
let list_fork phd lhd ltl =
  let r = list_fork_arg in
  let r = (Syntax.funOfArg r) (Syntax.P phd) in
  let r = (Syntax.funOfArg r) (Syntax.L lhd) in
  let r = (Syntax.funOfArg r) (Syntax.L ltl) in
  Syntax.lensOfArg r
let _ = Library.register ("list_fork",
                  Syntax.T (Syntax.Arrow (Syntax.Predicate, Syntax.Arrow
                    (Syntax.Lens, Syntax.Arrow (Syntax.Lens, Syntax.Lens)))),
                  list_fork_arg)

(* index *)
let index_arg =
  Compiler.compile_focal
    "let index k = list_map (pivot k); flatten_unique
     do index"
let index k =
  let r = index_arg in
  let r = (funOfArg r) (N k) in
  lensOfArg r
let _ = register ("index", T(Arrow (Name, Lens)), index_arg)

(* CONCATENATIONS *)
(* explode *)
let explode =
  let rec tree_of_string = function
      "" -> []
    | s -> 
	let sh = String.sub s 0 1 and st = String.sub s 1 (String.length s - 1) in
	(V.new_value sh)::(tree_of_string st)
  and string_of_tree = function
      [] -> ""
    | a::q -> 
	if( Name.Set.cardinal (V.dom a)) <> 1 then
	  error [`String "experimental_plugin.explode (put) : expecting exactly one child :";
		  `View a
		];
	let ch = Name.Set.choose (V.dom a) in
	if String.length ch <> 1 then
	  error [`String "experimental_plugin.explode (put) : expecting child with a one-char name";
		  `View a
		];
	ch^(string_of_tree q)
  in
  { get =
    (fun c ->
      if( Name.Set.cardinal (V.dom c)) <> 1 then
	error [`String "experimental_plugin.explode (get) : expecting exactly one child :";
		`View c];
      let k = Name.Set.choose (V.dom c) in
      (* here is the string we have to 'explode' *)
      V.structure_from_list (tree_of_string k)
      );
    put = 
    (fun a _ -> V.new_value (string_of_tree (V.list_from_structure a)))
  }

(* explode library interface *)  
let explode_lib = 
  L ( explode)
    
(* explode - unit tests *)
let explode_unit_tests = 
  [ test_get_eq [] "{\"\"}" "[]";
    test_get_eq [] "{youpi}" "[y o u p i]";
    test_get_eq [] "{\"youpi blam\"}" "[y o u p i {\" \"} b l a m]";
    test_put_eq [] "[y o u p i {\" \"} b l a m]" None "{\"youpi blam\"}";
    test_put_eq [] "[y o u p i]" None "{youpi}";
    test_put_eq [] "[]" None "{\"\"}"
  ] 

let _ = register_and_test ("explode", T Lens, explode_lib) explode_unit_tests

(* implode *)
(* NB implode is precisely the converse lens of explode, the only reason I did not
  write it this way { get c : explode.put c c ; put a, _: explode.get a } is for
  the error outputs to remain consistent *)
let implode =
  let rec tree_of_string = function
      "" -> []
    | s -> 
	let sh = String.sub s 0 1 and st = String.sub s 1 (String.length s - 1) in
	(V.new_value sh)::(tree_of_string st)
  and string_of_tree = function
      [] -> ""
    | a::q -> 
	if( Name.Set.cardinal (V.dom a)) <> 1 then
	  error [`String "experimental_plugin.implode (get) : expecting exactly one child :";
		  `View a
		];
	let ch = Name.Set.choose (V.dom a) in
	if String.length ch <> 1 then
	  error [`String "experimental_plugin.implode (get) : expecting child with a one-char name";
		  `View a
		];
	ch^(string_of_tree q)
  in
  { put =
    (fun a _ ->
      if( Name.Set.cardinal (V.dom a)) <> 1 then
	error [`String "experimental_plugin.implode (put) : expecting exactly one child :";
		`View a];
      let k = Name.Set.choose (V.dom a) in
      (* here is the string we have to 'explode' *)
      V.structure_from_list (tree_of_string k)
      );
    get = 
    (fun c -> V.new_value (string_of_tree (V.list_from_structure c)))
  }

(* implode library interface *)  
let implode_lib = 
  L ( implode)
    
(* implode - unit tests *)
let implode_unit_tests = 
  [ test_put_eq [] "{\"\"}" None "[]";
    test_put_eq [] "{youpi}" None "[y o u p i]";
    test_put_eq [] "{\"youpi blam\"}" None  "[y o u p i {\" \"} b l a m]";
    test_get_eq [] "[y o u p i {\" \"} b l a m]" "{\"youpi blam\"}";
    test_get_eq [] "[y o u p i]" "{youpi}";
    test_get_eq [] "[]" "{\"\"}"
  ] 

let _ = register_and_test ("implode", T Lens, implode_lib) implode_unit_tests

(* HACKS for concat *)
let first_list_empty = (fun v -> 
			  let vl = V.list_from_structure v in
			    (List.length vl = 2) 
			    && (V.list_from_structure (List.hd vl) = []))

let first_list_empty_lib = Sc first_list_empty
let _ = register ("first_list_empty", T(Schema), first_list_empty_lib)

let first_char_space = (fun v -> V.equal (V.head v) (V.new_value " "))
let first_char_space_lib = Sc first_char_space
let _ = register ("first_char_space", T(Schema), first_char_space_lib)

let concat_lib = Compiler.compile_focal 
  "let rec concat = 
     acond first_list_empty first_char_space
	(fork {\"*h\"} (map (rename \"*nil\" \" \")) (map (focus \"*h\" {\"*t\"=[]})))
	(xfork {\"*h\"} {\"*h\", x} (hoist \"*h\"; rename \"*t\" x) id;
	 xfork {x,\"*t\"} {\"*t\"} (rename x \"*h\"; concat; plunge \"*t\") id)
   in concat"
let concat = lensOfArg concat_lib
let _ = register ("concat", T(Lens), concat_lib)
