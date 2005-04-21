(*********************************)
(* PERVASIVES_PLUGIN             *)
(*********************************)

open Lens
open Library
open Syntax
open Error

(* common abbreviations used in unit tests *)
let a = Compiler.compile_view "{a={}}"
let b = Compiler.compile_view "{b={}}"
let empty = Compiler.compile_view "{}" 
let va = V a
let vb = V b
let ve = V empty

(*************)
(* DEBUGGING *)
(*************)

(* these must come first because the Compiler automtically inserts
   tracepoints for recursive Focal lens expressions (and hence, it'd
   better be defined) *)

(* the native implementations of both debugging lenses live in Lens.t *)

(* PROBE *)
let ref_get_probe = ref (fun _ _ _ -> ())
let ref_put_probe = ref (fun _ _ _ _ ->())
let _ = register ("probe",T (Arrow (Name,Lens)), 
		  F (function N n -> L (Lens.probe2 n (!ref_get_probe) (!ref_put_probe))
		       | _ -> focal_type_error "probe"))

(* TRACEPOINT *)
let tracepoint_lib = 
  F (function N n -> F (function L l -> L (Lens.tracepoint n l)
			  | _ -> focal_type_error "tracepoint")
       | _ -> focal_type_error "tracepoint")
    
let _ = register ("tracepoint",T (Arrow (Name,Arrow (Lens,Lens))), tracepoint_lib)

(***********)
(* SCHEMAS *)
(***********)

(* These will eventually disappear (and become TYPES!). 
   For now, they are a little bit ugly... *)

(* ALL *)
let all = (fun _ -> true)
let all_lib = Sc all
let _ = register ("all",T (Schema),all_lib)

(* NONE *)
let none = (fun _ -> false)
let none_lib = Sc none
let _ = register ("none",T (Schema),none_lib)
	  
(* UNION *)
let union s1 s2 v = (s1 v) || (s2 v) 
let union_lib = F (function Sc s1 -> 
		     F (function Sc s2 -> 
			  Sc (union s1 s2)
			  | _ -> focal_type_error "union")
		     | _ -> focal_type_error "union")
let _ = register ("union",T (Arrow (Schema,Arrow (Schema,Schema))),union_lib)

(* MINUS *)	  
let minus s1 s2 v = (s1 v) && (not (s2 v))
let minus_lib = F (function Sc s1 -> 
		     F (function Sc s2 -> 
			  Sc (minus s1 s2)
			  | _ -> focal_type_error "minus")
                     | _ -> focal_type_error "minus")
let _ = register ("minus",T (Arrow (Schema,Arrow (Schema,Schema))), minus_lib)
	  
(* NOT *)
let not_sc = minus all                 (* must mangle name slightly; 'not' is a keyword *)
let not_lib = F (function Sc s1 -> 
		   Sc (not_sc s1)
		   | _ -> focal_type_error "not")
let _ = register ("not", T (Arrow (Schema, Schema)), not_lib)
	  
(* INTER *)
let inter s1 s2 v = (s1 v) && (s2 v)
let inter_lib = F (function Sc s1 -> 
		     F (function Sc s2 -> 
			  Sc (inter s1 s2)
			  | _ -> focal_type_error "inter")
		     | _ -> focal_type_error "inter")
let _ = register ("inter", T (Arrow (Schema, Arrow (Schema, Schema))), inter_lib)
	  
(* EQUAL *)
let equal v1 = V.equal v1
let equal_lib = F (function V v1 -> Sc (equal v1)
		     | _ -> focal_type_error "equal")
let _ = register ("equal",T (Arrow (View,Schema)), equal_lib)

(* HASCHILD *)
let hasChild n v = Name.Set.mem n (V.dom v)
let hasChild_lib = F (function N n -> 
			Sc (hasChild n)
			| _ -> focal_type_error "hasChild")
let _ = register ("hasChild",T (Arrow (Name,Schema)), hasChild_lib)
	  
(* EQUALDOM *)
let equalDom v v1 = Name.Set.equal (V.dom v) (V.dom v1)
let equalDom_lib = F (function V v -> Sc (equalDom v)
			| _ -> focal_type_error "equalDom")

let _ = register ("equalDom",T (Arrow (View,Schema)), equalDom_lib)
	  
(* CHILD *)
let child n s v = 
  match (V.get v n) with
    | None -> false
    | Some vn -> s vn
let child_lib = F (function N n -> 
		     F (function Sc s -> 
			  Sc (child n s)
			  | _ -> focal_type_error "child")
		     | _ -> focal_type_error "child")
let _ = register ("child",T (Arrow (Name,Arrow (Schema,Schema))), child_lib)

(* ISLIST *)
let isList = V.is_list
let isList_lib = Sc (V.is_list)
let _ = register ("isList", T (Schema), isList_lib)

(* ISEMPTYLIST *)
let isEmptyList = V.is_empty_list
let isEmptyList_lib = Sc (isEmptyList)
let _ = register ("isEmptyList", T (Schema), isEmptyList_lib)

(* ISCONS *)
let isCons s1 s2 = inter (child V.head_tag s1) (child V.tail_tag s2)
let isCons_lib = F (function Sc s1 ->
		      F (function Sc s2 ->
			   Sc(isCons s1 s2)
			   | _ -> focal_type_error "isCons")
		      | _ -> focal_type_error "isCons")
let _ = register ("isCons", 
		  T(Arrow (Schema, Arrow(Schema, Schema))), 
		  isCons_lib)

(* ISLISTOF *)
let rec isListOf p v = 
  (isList v)
  && ((isEmptyList v)
      || ((child V.head_tag p v) 
	  && (child V.tail_tag (isListOf p) v)))
let isListOf_lib = F (function Sc p ->
			Sc(isListOf p)
			| _ -> focal_type_error "isListOf")
let _ = register ("isListOf", T(Arrow (Schema, Schema)), isListOf_lib)
		  
(* This schema is a bit of a wart. It is used in list_filter *)  
let rec isList_at_least_one d v = 
  (V.is_list v) 
  && ((child V.head_tag d v) 
      || (child V.tail_tag (isList_at_least_one d) v))
let _ = register ("isList_at_least_one", 
		  T(Arrow (Schema, Schema)), 
		  F (function Sc p -> Sc (isList_at_least_one p)
		       | _ -> focal_type_error "isList_at_least_one"))
	  
(******************)
(* Generic Lenses *)
(******************)

(*** ID ***)
(* id - native interface *)
let id =
  { get = (fun c -> c);
    put = (fun a co -> a)}

(* id - library interface  *)
let id_lib = L id

(* id - unit tests *)
let id_unit_tests =
  [ test_get_eq []  "{}" (*=*) "{}"
  ; test_put_eq []  "{}" None (*=*) "{}"
  ; test_put_eq [] "{a=b}" (Some "{}") (*=*) "{a=b}"
  ]
      
let _ = register_and_test ("id", T Lens, id_lib) 
	  id_unit_tests
	  
(*** CONST ***)
(* const - native interface *)
let const v d =
  { get = (fun c -> v);
    put = (fun a co ->
	     if V.equal a v then
	       match co with
		   None -> d
		 | Some(c) -> c
	     else error [`String "Pervasives_plugin.const(put): abstract view";
			 `View a;
			 `String "is not equal to"; `View (v)]) }
    
(* const - library interface *)
let const_lib = F (function
		       V v -> F (function 
				     V d -> L (const v d)
				   | _ -> focal_type_error "const")
		     | _ -> focal_type_error" const")
		  
(* const - unit tests *)
let const_unit_tests = 
    [ test_get_eq [va;vb] "{}" (*=*) "{a={}}"
    ; test_get_eq [ve;ve] "[1 2 3]" (*=*) "{}"
    ; test_put_eq [va;vb] "{a={}}" None (*=*) "{b={}}"
    ; test_put_eq [va;vb] "{a={}}" (Some "{}") (*=*) "{}"
    ; test_put_fail [va;vb] "{b={}}" (Some "{}")
    ; test_put_fail [va;vb] "{b={}}" None
    ]
      
let _ = register_and_test 
	  ("const", T (Arrow (View, Arrow (View, Lens))), const_lib)
	  const_unit_tests
	  
(*** COMPOSE2 ***)
(* compose2 - native interface *)
let compose2 l1 l2 = 
  let l1 = memoize l1 in
  { get = (fun c -> (l2.get (l1.get c)));
    put = (fun a co -> 
	     match co with
		 None -> l1.put (l2.put a None) None
	       | Some c -> l1.put (l2.put a (Some (l1.get c))) co)}

(* compose2 - library interface *)
let compose2_lib = 
  F (function
	 L l1 -> F (function
			L l2 -> L (compose2 l1 l2)
		      | _ -> focal_type_error "compose2")
       | _ -> focal_type_error "compose2")
    
(* compose2 - unit tests *)
let compose2_unit_tests = 
  let const_ab = L (const a b) in
  let const_ba = L (const b a) in
    [ test_get_eq [id_lib; id_lib] "{a={}}" (*=*) "{a={}}"
    ; test_get_eq [id_lib; const_ab] "{}" (*=*) "{a={}}"
    ; test_get_eq [const_ab;const_ba] "{}" (*=*) "{b={}}"
    ; test_put_eq [id_lib; const_ab] "{a={}}" None (*=*) "{b={}}"
    ; test_put_eq [id_lib; const_ab] "{a={}}" (Some "{}") (*=*) "{}"
    ; test_put_eq [const_ab;const_ba] "{b={}}" None (*=*) "{b={}}"
    ; test_put_eq [const_ab;const_ba] "{b={}}" (Some "{}") (*=*) "{}"
    ]
      
let _ = register_and_test 
	  ("compose2", T (Arrow (Lens,Arrow (Lens, Lens))), compose2_lib)
	  compose2_unit_tests

(********************)
(* Lenses for trees *)
(********************)
(*** MAP ***)
(* map - native interface *)
(* map can be implemented in terms of wmap, but it delays evaluation,
   of the sub-lens which is bad for memoization and causes too 
   much consing... *)
let map ?(log=false) l = 
  { get = (fun c ->
	     let binds =
	       V.fold
		 (fun k ck bindacc ->
		    let ck' = (l.get ck) in
		    ((k,Some ck')::bindacc))
		 c [] in
	     V.create_star binds);
    put =
      (fun a co ->
         let c = match co with None -> V.empty | Some c -> c in
 	 let cbinds =
	   V.fold
	     (fun k vk bindacc ->
(*
              if log && (Prefs.read Config.progress) then begin
                V.format_msg [`Name k];
                Format.print_flush ()
              end;
 *)
		(k,
		 ( match (V.get c k),(V.get a k) with
		     | Some ck, Some ak -> Some ((l.put ak (Some ck)))
		     | Some ck, None -> None
		     | None, Some ak -> Some ((l.put ak None))
		     | _ -> assert false))::bindacc)
  	     a [] in
	 V.create_star cbinds)}

(* map - library interface *)
let map_lib = 
  F (function
	 L l -> L (map l)
       | _ -> focal_type_error "map")

(* map - unit_tests *)
let map_unit_tests = 
  let const_ab = L (const a b) in
    [ test_get_eq [id_lib] "{x={} y=[1 2 3]}" (*=*) "{x={} y=[1 2 3]}"
    ; test_get_eq [const_ab] "{x={} y=[1 2 3]}" (*=*) "{x={a={}} y={a={}}}"
    ; test_put_eq [const_ab] "{x={a={}} y={a={}}}" None (*=*) "{x={b={}} y={b={}}}"
    ; test_put_eq [const_ab] "{x={a={}} y={a={}}}" (Some "{y=[1 2 3]}") (*=*) "{x={b={}} y=[1 2 3]}"
    ]

let _ = register_and_test 
	  ("map",
	   T (Arrow (Lens,Lens)),
	   map_lib)
	  map_unit_tests
	  
(*** WMAP ***)
(* wmap - native interface *)
let wmap ?(log=false) l0 =
  let l =
    let memo = Hashtbl.create 11 in
    (fun k ->
       try Hashtbl.find memo k with
	 | Not_found -> let res = l0 k in Hashtbl.add memo k res; res)
  in
  { get = (fun c ->
	     let binds =
	       V.fold
		 (fun k ck bindacc ->
		    let ck' = ((l k).get ck) in
		    ((k,Some ck')::bindacc))
		 c [] in
	     V.create_star binds);
    put =
      (fun a co ->
	 let c = match co with None -> V.empty | Some c -> c in
 	 let cbinds =
	   V.fold
	     (fun k vk bindacc ->
(*              if log && (Prefs.read Config.progress) then begin
                V.format_msg [`Name k];
                Format.print_flush ()
              end;
 *)
		(k,
		 ( match (V.get c k),(V.get a k) with
		     | Some ck, Some ak -> Some (((l k).put ak (Some ck)))
		     | Some ck, None -> None
		     | None, Some ak -> Some (((l k).put ak None))
		     | _ -> assert false))::bindacc)
  	     a [] in
	 V.create_star cbinds)}

(* wmap - library interface *)
let wmap_lib = 
  F (function
	 M m -> L (wmap m)
       | _ -> focal_type_error "wmap")

(* wmap - unit tests *)
let wmap_unit_tests = 
  let m = M (fun n -> if n = "y" then const a b else id) in
    [ test_get_eq [m] "{}" (*=*) "{}"
    ; test_get_eq [m] "{x={} y=[1 2 3]}" (*=*) "{x={} y={a={}}}"
    ; test_put_eq [m] "{}" None (*=*) "{}"
    ; test_put_eq [m] "{}" (Some "{x={} y=[1 2 3]}") (*=*) "{}"
    ; test_put_eq [m] "{x={} y={a={}}}" None (*=*) "{x={} y={b={}}}"
    ; test_put_eq [m] "{x={a={}}}" (Some "{x={} y=[1 2 3]}") (*=*) "{x={a={}}}"
    ; test_put_eq [m] "{y={a={}}}" (Some"{x={} y=[1 2 3]}") (*=*) "{y=[1 2 3]}"
    ; test_put_eq [m] "{y={a={}} z={c={}}}" (Some "{x={} y=[1 2 3]}") (*=*) "{y=[1 2 3] z={c={}}}"
    ]
      
let _ = register_and_test 
	  ("wmap", T (Arrow (Maparg,Lens)),wmap_lib) 
	  wmap_unit_tests

(* XFORK *)
(* xfork - native interface *)
let xfork pc pa l1 l2 =
  { get = 
      (fun c ->
	 let c1,c2 = V.split pc c in
	 let a1 = l1.get c1 in
	 let a2 = l2.get c2 in
	   if not(Name.Set.for_all pa (V.dom a1)) then
	     error [`String "Pervasives_plugin.xfork(get): l1 yielded a child not ";
         	    `String "satisfying pa"; 
		    `View a1];
	   if not(Name.Set.for_all (fun k -> not (pa k)) (V.dom a2)) then
	     error [`String "Pervasives_plugin.xfork(get): l2 yielded a child satisfying pa";
		    `View a2];
	   V.concat a1 a2);
    put = (fun a co -> 
	     let co1,co2 =
	       match co with None -> None,None
		 | Some c -> let c1,c2 = V.split pc c in (Some c1,Some c2) in
	     let a1, a2 = V.split pa a in
	     let c1' = l1.put a1 co1 in
	     let c2' = l2.put a2 co2 in
	       if not(Name.Set.for_all pc (V.dom c1')) then
		 error [`String "Pervasives_plugin.xfork(put): l1 yielded a child ";
			`String "not satisfying pc"; 
			`View c1'];
	       if not(Name.Set.for_all (fun k -> not (pc k)) (V.dom c2')) then
		 error [`String "Pervasives_plugin.xfork(put): l2 yielded a child ";
			`String "satisfying pc"; 
			`View c2'];
	       V.concat c1' c2')}

(* xfork - library interface *)
let xfork_lib = 
  F(function P pc -> F (
      function P pa -> F (
	function L l1 -> F (
	  function L l2 -> L (xfork pc pa l1 l2)
	    | _ -> focal_type_error "xfork")
	  | _ -> focal_type_error "xfork")
	| _ -> focal_type_error "xfork")
      | _ -> focal_type_error "xfork")

(* xfork - unit tests *)
let xfork_unit_tests = 
  let pc = P (Prd.s "x") in
  let pa = P (Prd.s "a") in
  let const_ax = L (const a (Compiler.compile_view "{x={}}")) in
  let const_ba = L (const b a) in
    [ test_get_eq [pc;pa;const_ax;const_ba] "{x={} y={}}" (*=*) "{a={} b={}}"
    ; test_get_eq [pc;pa;const_ax;const_ba] "{x={}}" (*=*) "{a={} b={}}"
    ; test_get_eq [pc;pa;const_ax;id_lib] "{x={} y={}}" (*=*) "{a={} y={}}"
    ; test_get_eq [pc;pa;const_ax;const_ba] "{}" (*=*) "{a={} b={}}"
    ; test_get_eq [pc;pa;const_ax;id_lib] "{}" (*=*) "{a={}}"
    ; test_put_eq [pc;pa;const_ax;const_ba] "{a={} b={}}" (Some "{}") (*=*) "{}"
    ; test_put_eq [pc;pa;const_ax;const_ba] "{a={} b={}}" (Some "{b={}}") (*=*) "{b={}}"
    ; test_put_eq [pc;pa;const_ax;const_ba] "{a={} b={}}" None (*=*) "{a={} x={}}"
    ; test_get_fail [pc;pa;const_ba;id_lib] "{}"
    ; test_put_fail [pc;pa;const_ax;const_ba] "{}" None
    ; test_put_fail [pc;pa;const_ax;const_ba] "{a={}}" None
    ]
      
let _ = register_and_test
	  ("xfork",
	   T (Arrow (Predicate,
		     Arrow (Predicate,
			    Arrow (Lens,
				   Arrow (Lens,Lens))))),
	   xfork_lib)
	  xfork_unit_tests
	  
(* HOIST *)
(* hoist - native interface *)
let hoist k=
  { get = 
      (fun c ->
	 if (Name.Set.cardinal (V.dom c)) <> 1 then
	   error [`String "Pervasives_plugin.hoist: expecting exactly one child (named ";
		  `Name k; 
		  `String ")"; 
		  `View c];
	 if (Name.Set.choose (V.dom c)) <> k then
	       error [`String "Pervasives_plugin.hoist: child should be named "; 
		      `Name k; 
		      `View c];
	 V.get_required c k);
    put = 
      (fun a _ -> 
	 V.set V.empty k (Some a)) }

(* hoist - library interface *)
let hoist_lib = 
  F (function 
	 N k -> L (hoist k)
       | _ -> focal_type_error "hoist")

(* hoist - unit tests *)
let hoist_unit_tests = 
  [ test_get_eq [N "n"] "{n={a={}}}" (*=*) "{a={}}"
  ; test_put_eq [N "n"] "{a={}}" None (*=*) "{n={a={}}}"
  ; test_put_eq [N "n"] "{a={}}" (Some "{}") (*=*) "{n={a={}}}"
  ; test_put_eq [N "n"] "{}" None (*=*) "{n={}}"
  ; test_get_fail [N "n"] (*=*) "{a={}}"
  ]

let _ = register_and_test 
	  ("hoist", 
	   T (Arrow (Name,Lens)), 
	   hoist_lib)
	  hoist_unit_tests

(* PLUNGE *)
(* plunge - native interface *)
let plunge k =
  { get = 
      (fun c -> V.set V.empty k (Some c));
    put = (fun a _ -> 
	     if (Name.Set.cardinal (V.dom a)) <> 1 then
	       error [`String "Pervasives_plugin.plunge(put): expecting exactly one child"; 
		      `View a];
	     if (Name.Set.choose (V.dom a)) <> k then
	       error [`String "Pervasives_plugin.plunge(put): child should be named ";
		      `Name k; `View a];
	     V.get_required a k)}

(* plunge - library interface *)
let plunge_lib = 
  F (function
	 N k -> L (plunge k)
       | _ -> focal_type_error "plunge")

(* plunge - unit tests *)
let plunge_unit_tests = 
  [ test_get_eq [N "n"]"{a={}}" (*=*) "{n={a={}}}"
  ; test_get_eq [N "n"] "{}" (*=*) "{n={}}"
  ; test_put_eq [N "n"] "{n={a={}}}" None (*=*) "{a={}}"
  ; test_put_eq [N "n"] "{n={a={}}}" (Some "{}") (*=*) "{a={}}"
  ; test_put_eq [N "n"] "{n={}}" (Some "{a={}}") (*=*) "{}"
  ; test_put_fail [N "n"] "{a={}}" None
  ; test_put_fail [N "n"] "{a={}}" (Some "{}")
  ]

let _ = register_and_test 
	  ("plunge", 
	   T (Arrow (Name,Lens)), 
	   plunge_lib)
	  plunge_unit_tests
	  
(******************)
(* Copy and Merge *)
(******************)

(* COPY *)
(* copy - native interface *)
let copy m n =
  { get = 
      (fun c -> 
	 let child =
	   try V.get_required c m
	   with V.Illformed(_,_) -> 
	     error [`String "Pervasives_plugin.copy: expecting one child named ";
		    `Name m; 
		    `String ")"; 
		    `View c] in
	   V.set c n (Some child)) ;
    put = 
      (fun a _ -> 
	 if (try V.equal (V.get_required a m) (V.get_required a n)
	     with V.Illformed(_,_) -> 
	       error [`String "Pervasives_plugin.copy: expecting two children named ";
		      `Name m; 
		      `String "and";
		      `Name n; 
		      `View a])
	 then V.set a n None
	 else 
	   error [`String "Pervasives_plugin.copy: expecting two equal children named ";
		  `Name m; 
		  `String " and ";
		  `Name n;
		  `View a]) }

(* copy - library interface *)
let copy_lib =
  F (function 
	 N m -> F (function 
		       N n -> L (copy m n)
		     | _ -> focal_type_error "copy")
       | _ -> focal_type_error "copy")

(* copy - unit tests *)
let copy_unit_tests = 
  [ test_get_eq [N "x";N "y"] "{x={a={}}}" (*=*) "{x={a={}} y={a={}}}"
  ; test_put_eq [N "x";N "y"] "{x={a={}} y={a={}}}" None (*=*) "{x={a={}}}"
  ; test_put_eq [N "x";N "y"] "{x={a={}} y={a={}}}" (Some "{x={a={}}}") (*=*) "{x={a={}}}"
  ; test_put_eq [N "x";N "y"] "{x={b={}} y={b={}}}" (Some "{x={a={}}}") (*=*) "{x={b={}}}"
  ; test_get_fail [N "x"; N "y"] "{}"
  ; test_put_fail  [N "x"; N "y"] "{x={a={}}}" None
  ; test_put_fail  [N "x"; N "y"] "{x={a={}} y={b={}}}" None
  ]

let _ = register_and_test 
	  ("copy",
	   T (Arrow (Name, Arrow (Name, Lens))),
	   copy_lib)
	  copy_unit_tests

(* MERGE *)
(* merge - native interface *)
let merge m n =
  { get = (fun c -> V.set c n None) ;
    put = 
      (fun a ->
	 function
	   | None -> 
	       (try V.set a n (Some (V.get_required a m))
		with V.Illformed(_,_) -> 
		  error
		  [`String "Pervasives_plugin.merge(put): expecting a child named ";
		   `Name m])
	   | Some c ->
	       let cmo,cno = (V.get c m), (V.get c n) in
	       let eqCmCn = 
		 match cmo,cno with
		   | None, None -> true
		   | None, Some _ -> false
		   | Some _, None -> false
		   | Some cm, Some cn -> V.equal cm cn
	       in
		 if (eqCmCn) 
		 then V.set a n (V.get a m)
		 else V.set a n cno)
  }

(* merge - library interface *)
let merge_lib =
  F (function 
	 N m -> F (function 
		       N n -> L (merge m n)
		     | _ -> focal_type_error "merge")
       | _ -> focal_type_error "merge")
    
(* merge - unit tests *)
let merge_unit_tests =   
  [ test_get_eq [N "x";N "y"] "{x={a={}} y={a={}}}" (*=*) "{x={a={}}}"
  ; test_get_eq [N "x";N "y"] "{x={a={}} y={b={}}}" (*=*) "{x={a={}}}"
  ; test_get_eq [N "x";N "y"] "{x={b={}} y={b={}}}" (*=*) "{x={b={}}}"
  ; test_put_eq [N "x";N "y"] "{x={a={}}}" None (*=*) "{x={a={}} y={a={}}}"
  ; test_put_eq [N "x";N "y"] "{x={a={}}}" (Some "{x={a={}} y={b={}}}") (*=*) "{x={a={}} y={b={}}}"
  ; test_put_fail [N "x"; N "y"] "{}" None
  ]

let _ = register_and_test 
	  ("merge",
	   T (Arrow (Name, Arrow (Name, Lens))),
	   merge_lib)
	  merge_unit_tests

(****************)
(* Conditionals *)
(****************)

(* OMEGA *) 
(* omega - native interface *)
(* always throws an exception - useful for writing f21 and f12 arguments
 * to cond -- will always produce MISSING *)
let omega = 
  { get = (fun c -> error [`String "Pervasives_plugin.error(get)"]);
    put = (fun a co -> error [`String "Pervasives_plugin.error(out)"])}

(* error - library interface *)
let omega_lib = L omega

(* error - unit tests *)
let omega_unit_tests = 
  [ test_get_fail [] "{}"
  ; test_put_fail [] "{}" None
  ]
    
let _ = register_and_test ("omega", T (Lens), omega_lib) omega_unit_tests
	    
(* COND *)
(* cond - native interface *)
let cond c a1 a2 l21 l12 lt lf =
  let catchfail l cv =
    try Some (l.get cv)
    with _ -> None in
  { get = (fun cv ->
	     if c cv then lt.get cv 
	     else lf.get cv);
    put = (fun a co -> 
	     if a1 a then
	       if a2 a then
		 match co with 
		   | None -> lf.put a co
		   | Some cv -> 
		       if c cv 
		       then lt.put a co
		       else lf.put a co
	       else 
		 match co with 
		   | None -> lt.put a co
		   | Some cv -> 
		       if c cv
		       then lt.put a co
		       else lt.put a (catchfail l21 cv)
	     else
	       if a2 a then
		 match co with 
		   | None -> lf.put a co
		   | Some cv -> 
		       if c cv
		       then lf.put a (catchfail l12 cv)
		       else lf.put a co
	       else error [
		 `String "cond (put): the abstract view does not satisfy a1 or a2:";
		 `View a ]
	  )}  

(* cond - library interface *)
let cond_lib = 
  F (function Sc c -> F (
       function Sc a1 -> F (
	 function Sc a2 -> F (
	   function L l21 -> F (
	     function L l12 -> F (
	       function L lt -> F (
		 function L lf -> L (cond c a1 a2 l21 l12 lt lf)
		   | _ -> focal_type_error "cond")
		 | _ -> focal_type_error "cond")
	       | _ -> focal_type_error "cond")
	     | _ -> focal_type_error "cond")
	   | _ -> focal_type_error "cond")
	 | _ -> focal_type_error "cond")
       | _ -> focal_type_error "cond")

(* cond - unit tests *)
let cond_unit_tests = 
  let const_aa = L (const a a) in
  let const_bb = L (const b b) in
  let sc_a = Sc (equalDom a) in
  let sc_b = Sc (equalDom b) in
  let omega = L (Lens.native (fun _ -> error []) (fun _ _ -> error [])) in
    [ test_get_eq [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb]"{a=[1]}" (*=*) "{a={}}"
    ; test_get_eq [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb]"{b=[2]}" (*=*) "{b={}}"
    ; test_put_eq [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb]"{a={}}" (Some "{a=[1]}") (*=*) "{a=[1]}"
    ; test_put_eq [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb] "{a={}}" None (*=*) "{a={}}"
    ; test_put_eq [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb] "{a={}}" (Some "{b=[1]}") (*=*) "{a={}}"
    ; test_put_fail [sc_a;sc_a;sc_b;omega;omega;const_aa;const_bb]"{}" None
    ] (* should add tests that stress the conversion functions f21 and f22, overlap in a1, a2*)

let _ = 
  register_and_test 
    ("cond",
     T (Arrow (Schema, 
	       Arrow (Schema, 
		      Arrow (Schema, 
			     Arrow (Lens, 
				    Arrow (Lens, 
					   Arrow (Lens, 
						  Arrow 
						    (Lens,Lens)))))))),
     cond_lib)
    cond_unit_tests
    
(* ACOND *)
(* acond - library interface *)
let acond_lib = 
  Compiler.compile_focal     
     "let acond pc pa = 
       cond pc pa (not pa)
         omega omega
     do acond"

(* acond - native interface *)
let acond c a lt lf= 
  let r = acond_lib in
  let r = (funOfArg r) (Sc c) in
  let r = (funOfArg r) (Sc a) in
  let r = (funOfArg r) (L lt) in
  let r = (funOfArg r) (L lf) in
    lensOfArg r

(* acond - unit tests *)
let acond_unit_tests =
  let sc_a = Sc (equalDom a) in
  let sc_b = Sc (equalDom b) in
  let const_ba = L (const b a) in
  let const_ab = L (const a b) in
    [ test_get_eq [sc_a;sc_b;const_ba;const_ab] "{a=[1]}" (*=*) "{b={}}"
    ; test_get_eq [sc_a;sc_b;const_ba;const_ab] "{}" (*=*) "{a={}}"
    ; test_put_eq [sc_a;sc_b;const_ba;const_ab] "{b={}}" None (*=*) "{a={}}"
    ; test_put_eq [sc_a;sc_b;const_ba;const_ab] "{b={}}" (Some "{a=[1]}") (*=*) "{a=[1]}"
    ]
	
let _ = register_and_test
	  ("acond",
	   T (Arrow (Schema, Arrow (Schema,
				    Arrow (Lens, Arrow (Lens,Lens))))),
	   acond_lib)
	  acond_unit_tests
	  
(* CCOND *)
(* ccond - library interface *)
let ccond_lib = Compiler.compile_focal 
	      "let ccond pc lt lf =
                cond pc all all id id lt lf
               do ccond"

(* ccond - native interface *)
let ccond c lt lf =
  let r = ccond_lib in
  let r = (funOfArg r) (Sc c) in
  let r = (funOfArg r) (L lt) in
  let r = (funOfArg r) (L lf) in
    lensOfArg r

(* ccond - unit tests *)
let ccond_unit_tests = 
  let sc_a = Sc (equalDom a) in
  let lt = L (const b a) in
  let lf = L (compose2 (const V.empty V.empty) (plunge "b")) in
    [ test_get_eq [sc_a;lt;lf] "{a=[1]}" (*=*) "{b={}}"
    ; test_get_eq [sc_a;lt;lf] "{c=[2]}" (*=*) "{b={}}"
    ; test_put_eq [sc_a;lt;lf] "{b={}}" None (*=*) "{}"
    ; test_put_eq [sc_a;lt;lf] "{b={}}" (Some "{c=[2]}") (*=*) "{c=[2]}"
    ; test_put_eq [sc_a;lt;lf] "{b={}}" (Some "{a=[3]}") (*=*) "{a=[3]}"
    ; test_put_fail [sc_a;lt;lf] "{a={}}" None
    ]
      
let _ = register_and_test 
	  ("ccond",
	   T (Arrow (Schema, Arrow (Lens, Arrow (Lens,Lens)))),
	   ccond_lib)
	  ccond_unit_tests
	  
(*************)
(* DATABASES *)
(*************)

(* PIVOT *)
(* pivot - native interface *)
let pivot k =
  { get = 
      (fun c ->
	 let ck =
	   try V.get_required c k
	   with V.Illformed(_,_) -> 
	     error [`String "pivot: the following view should have ";
		    `String "exactly one child named "; 
		    `Name k; 
		    `View c] in
	 let ckv = V.get_value ck in
	   V.set V.empty ckv (Some (V.set c k None)));
    put = 
      (fun a _ ->
	 if (Name.Set.cardinal (V.dom a)) <> 1 then
	   error [`String "pivot: the following view should have ";
		  `String "exactly one child"; 
		  `View a]
	 else
	   let ak = Name.Set.choose (V.dom a) in
	   let w = try V.get_required a ak with Not_found -> assert false in
	     if V.get w k <> None then
	       error [`String "Pivot: child ";
		      `Name k;
		      `String "of this view should not exist: "; 
		      `View w]
	     else
	       V.set w k (Some (V.new_value ak))
      )} 

(* pivot - library interface *)
let pivot_lib = 
  F (function
	 N k -> L (pivot k)
       | _ -> focal_type_error "pivot")

(* pivot - unit tests *)
let pivot_unit_tests = 
  [ test_get_eq [N "k"] "{k={x={}} a=[1] b=[2]}" (*=*) "{x={a=[1] b=[2]}}"
  ; test_put_eq [N "k"] "{x={a=[1] b=[2]}}" None (*=*) "{k={x={}} a=[1] b=[2]}"
  ; test_get_fail [N "k"] "{}"
  ; test_put_fail [N "k"] "{x={} y={}}" None
  ; test_put_fail [N "k"] "{x={k=[0] a=[1]}}" None
  ]

let _ = register_and_test 
	  ("pivot", 
	   T (Arrow (Name, Lens)), 
	   pivot_lib) 
	  pivot_unit_tests
	  
(* JOIN *)
(* Dan Spoonhower's outer join *)
(* disclaimer: written down very quickly and directly.  trying to get
   it correct so using lots of lets and explicit match statements for
   readability.  let's clean it up later -nate
*)
(* join - native interface *)
let join m1 m2 = 
  { get = 
      (fun c ->
	 try
	   let rec compute_join b1 b2 acc = 
	     let bo = 
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
			 | None, None       -> assert false			     
			 | Some t1, None    -> V.set V.empty m1 (Some t1)
			 | None, Some t2    -> V.set V.empty m2 (Some t2)
			 | Some t1, Some t2 -> V.set (V.set V.empty m2 (Some t2)) m1 (Some t1)
		     in
		       compute_join b1' b2' (V.set acc k (Some tk))
	   in
	   let tm1, tm2 = (V.get_required c m1, V.get_required c m2) in	 
	     compute_join tm1 tm2 V.empty
	 with V.Illformed(_,_) ->
	   error [`String "Pervasives_plugin.join(get): expected view with children: "; 
		  `Name m1; 
		  `String " and "; 
		  `Name m2]
      );
    put = 
      (fun a co ->
	 let init = V.set (V.set V.empty m2 (Some V.empty)) m1 (Some V.empty) in
	 let rec compute_unjoin b acc =
	   if (V.is_empty b) then acc
	   else
	     try
	       let k = Name.Set.choose (V.dom b) in
	       let tk = V.get_required b k in
	       let b' = (V.set b k None) in
	       let cm1,cm2 = (V.get_required acc m1,V.get_required acc m2) in
	       let acc' = 
		 match (V.get tk m1, V.get tk m2) with
		   | None, None       -> 
		       error [`String "Pervasives_plugin.join(put): illformed abstract view"]
		   | Some t1, None    -> 
		       V.set acc m1 (Some (V.set cm1 k (Some t1)))
		   | None, Some t2    -> 
		       V.set acc m2 (Some (V.set cm2 k (Some t2)))
		   | Some t1, Some t2 -> 
		       V.set 
			 (V.set acc m2 (Some (V.set cm2 k (Some t2)))) 
			 m1 (Some (V.set cm1 k (Some t1)))
	       in
		 compute_unjoin b' acc'
	     with Not_found ->
	       error [`String "Pervasives_plugin.join(put): the impossible happened"] in
	   compute_unjoin a init 
      )
  }

(* join - library interface *)
let join_lib = 
  F (function N n1 -> 
       F (function N n2 -> L (join n1 n2)
	    | _ -> focal_type_error "join")
       | _ -> focal_type_error "join")

(* join - unit tests *)
let join_unit_tests = 
  [ test_get_eq [N "x";N "y"] "{x={} y={}}" (*=*) "{}" 
  ; test_get_eq [N "x";N "y"] "{x={1={} 2={}} y = {2={}}}" (*=*) "{1={x={}} 2={x={} y={}}}"
  ; test_get_eq [N "x";N "y"] "{x={1=[a] 2=[b]} y = {2=[c]}}" (*=*) "{1={x=[a]} 2={x=[b] y=[c]}}"
  ; test_put_eq [N "x";N "y"] "{}" None (*=*) "{x={} y={}}"
  ; test_put_eq [N "x";N "y"] "{}" (Some "{x=[1] y=[2]}") (*=*) "{x={} y={}}"
  ; test_put_eq [N "x";N "y"] "{a={x=[1] y=[3]} b={x=[2]} c={y=[4]}}" None (*=*) "{x={a=[1] b=[2]} y={a=[3] c=[4]}}"
  ; test_get_fail [N "x";N "y"] "{}"
  ; test_get_fail [N "x";N "y"] "{x=[]}"
  ]

let _ = register_and_test 
	  ("join", 
	   T (Arrow (Name, Arrow (Name,Lens))), 
	   join_lib)
	  join_unit_tests

(* FLATTEN *)
(* flatten - native interface *)
let flatten =   
  let rec get = function 
      c -> 
	if V.is_empty_list c then V.empty_list
	else 
	  (* Error handling in case of ill-formed list *)
	  let head = V.get_required c V.head_tag in 
	  let c' = V.get_required c V.tail_tag in
	    (* List of labels pointing toward trees *)
	    if Name.Set.cardinal (V.dom head) = 1 then 
	      let c_list = V.to_list head in
	      let (k,d) = Safelist.hd c_list in
	      let a_rec = get c' in		
	      let a = if (V.is_empty_list a_rec) then V.empty else a_rec in (* removes spurious *nil labels in output *)
	      let childk = V.get a k in
		begin
		  match childk with
		      None -> 
			V.set a k (Some (V.cons d V.empty_list))
		    | Some s -> 
			let a' = V.set a k None in
			  V.set a' k (Some (V.cons d s))
		end 
	    else error [`String "Pervasives_plugin.flatten(get): expected a view with exactly one child: "; 
			`View head]
  in
  let listify v =
    let v_list = V.to_list v in
    let listifystep (k,child) =
      List.map (function x -> (k,x)) (V.list_from_structure child)
	(*
	  | _ -> error [`String "flatten: expected a view with exactly one child: "; `View t]
	*)
    in
      Safelist.map (function (k,x) -> 
		      V.set V.empty k (Some x)) 
	(Safelist.flatten (Safelist.map listifystep v_list)) in
  let rec put a = function
      None -> V.structure_from_list (listify a)
    | Some c -> 
	if V.is_empty_list c then V.structure_from_list (listify a)
	else 
	  let head = V.get_required c V.head_tag in
	  (* Error handling in case of ill-formed list *)
	  let c' = V.get_required c V.tail_tag in
	    (* List of labels pointing toward trees *)
	    if Name.Set.cardinal (V.dom head) = 1 then 
	      let c_list = V.to_list head in
	      let (k,d) = Safelist.hd c_list in
		match V.get a k with
		    None -> put a (Some c')
		  | Some ds -> 
		      (* Error handling in case of ill-formed list *)
		      let d' = V.get_required ds V.head_tag in 
		      let s = V.get_required ds V.tail_tag in
			if V.is_empty_list s then
			  V.cons 
			    (V.from_list [k,d']) 
			    (put (V.set a k None) (Some c'))
			else 
			  V.cons 
			    (V.from_list [k,d']) 
			    (put (V.set (V.set a k None) k (Some s)) (Some c'))
	    else error [`String "Pervasives_plugin.flatten(put): expected a view with exactly one child: "; 
			`View head]
  in
    {get = get ;
     put = put }

(* flatten - library interface *)
let flatten_lib =  L flatten

(* flatten - unit tests *)
let flatten_unit_tests = 
  [ test_get_eq [] "[]" (*=*) "[]"
  ; test_get_eq [] "[{k1={a={}}} {k2={b={}}}]" (*=*) "{k1=[{a={}}] k2=[{b={}}]}"
  ; test_get_eq [] "[{a=[1]} {b=[2]} {a=[3]}]" (*=*) "{a=[[1] [3]] b=[[2]]}"
  ; test_put_eq [] "{a=[[1] [3]] b=[[2]]}" (Some "[{a=[1]} {b=[2]} {a=[3]}]") (*=*) "[{a=[1]} {b=[2]} {a=[3]}]"
  ; test_put_eq [] "{a=[[1] [3]] b=[[2]]}" (Some "[{b=[1]} {a=[2]}]") (*=*) "[{b=[2]} {a=[1]} {a=[3]}]"
  ; test_put_eq [] "{a=[[1]] b=[[2]]}" (Some "[{b=[1]} {a=[2]} {a=[3]}]") (*=*) "[{b=[2]} {a=[1]}]"
  ; test_get_fail [] "[{a={} b={}}]"
  ; test_put_fail [] "{a=[{}]}" (Some "[{a={} b={}}]")
    (* ; test_put_fail [] "{}" (Some "[{a={}} {b={}}]") -- succeeds but should fail? *)
  ] 
    (* I don't see a way to write a unit test that checks the behavior
       of listify. Is listify meant to be deterministic? It certainly
       IS deterministic (OCaml and the implementation of V will pick
       an order), but at this level, the order that elements are
       picked from the bush and inserted into the list is unspecified,
       correct? This makes it difficult to test uses of flatten that
       call listify w/o defining some equality up-to order in list *)

let _ = register_and_test 
	  ("flatten", 
	   T (Lens), 
	   flatten_lib) 
	  flatten_unit_tests

(***********************)
(* DERIVED TREE LENSES *)
(***********************)

(* FORK *)
(* fork - library interface *)
let fork_lib = Compiler.compile_focal "do function p -> xfork p p"

(* fork - native interface *)
let fork p l1 l2 =
  let r = fork_lib in
  let r = (funOfArg r) (P p) in
  let r = (funOfArg r) (L l1) in
  let r = (funOfArg r) (L l2) in
    lensOfArg r

(* fork - unit tests *)
let fork_unit_tests = 
  let px = P (Prd.s "x") in
  let x = (Compiler.compile_view "{x={}}") in
  let const_bx = L (const b x) in
  let const_bb = L (const b b) in
    [ test_get_fail [px;const_bx;id_lib] "{x={}}"
    ; test_put_fail [px;const_bb;id_lib] "{b={}}" (Some "{x={}}")
    ]
 
let _ = 
  register_and_test 
    ("fork",T (Arrow (Predicate, Arrow (Lens, Arrow (Lens,Lens)))), fork_lib)
    fork_unit_tests


(* MAPP *)
(* mapp - library interface *)
let mapp_lib = 
  Compiler.compile_focal 
    "let mapp p l = fork p (map l) id
     do mapp"

(* mapp - native interface *)
let mapp p l = 
  let r = mapp_lib in
  let r = (funOfArg r) (P p) in
  let r = (funOfArg r) (L l) in
    lensOfArg r

(* mapp - unit_tests *)
let mapp_unit_tests = 
  let pa = P (Prd.s "a") in
  let const_bb = L (const b b) in
  [ test_get_eq [pa;const_bb] "{}" (*=*) "{}"
  ; test_get_eq [pa;const_bb] "{a={} b=[2]}" (*=*) "{a={b={}} b=[2]}"
  ; test_put_eq [pa;const_bb] "{a={b={}} b=[2]}" None (*=*) "{a={b={}} b=[2]}"
  ; test_put_eq [pa;const_bb] "{a={b={}} b=[2]}" (Some "{a=[1] b=[3]}") (*=*) "{a=[1] b=[2]}"
  ]

let _ = register_and_test
	  ("mapp", T(Arrow(Predicate, Arrow (Lens,Lens))), mapp_lib)
	  mapp_unit_tests
 
(* FILTER *)
(* filter - library interface *)
let filter_lib = 
  Compiler.compile_focal 
    "let filter p d = fork p id (const {} d) do filter"

(* filter - native interface *)
let filter p d =
  let r = filter_lib in
  let r = (funOfArg r) (P p) in
  let r = (funOfArg r) (V d) in
    lensOfArg r

(* filter - unit tests *)
let filter_unit_tests = 
  let px = P (Prd.s "x") in
  let vy = V (Compiler.compile_view "{y={}}") in
  [ test_get_eq [px;vy] "{}" (*=*) "{}"
  ; test_get_eq [px;vy] "{y={} x={}}" (*=*) "{x={}}" 
  ; test_put_eq [px;vy] "{}" None (*=*) "{y={}}"
  ; test_put_eq [px;vy] "{}" (Some "{y=[1]}") "{y=[1]}"
  ]
    
let _ = register_and_test
	  ("filter", T (Arrow (Predicate, Arrow (View, Lens))), filter_lib)
	  filter_unit_tests
	  
(* PRUNE *)
(* prune - library interface *)
let prune_lib = 
  Compiler.compile_focal 
    "let prune n d = fork {n} (const {} {n=d}) id do prune"
    
(* prune - native interface *)
let prune n d =
  let r = prune_lib in
  let r = (funOfArg r) (N n) in
  let r = (funOfArg r) (V d) in
    lensOfArg r

(* prune - unit tests *)
let prune_unit_tests = 
  let vel = V (Compiler.compile_view "[]") in
  [ test_get_eq [N "a";vel] "{}" (*=*) "{}"
  ; test_get_eq [N "a";vel] "{a=[1] b=[2]}" (*=*) "{b=[2]}"
  ; test_put_eq [N "a";vel] "{b=[2]}" None (*=*) "{a=[] b=[2]}"
  ; test_put_eq [N "a";vel] "{b=[2]}" (Some "{a=[1] b=[3]}") (*=*) "{a=[1] b=[2]}"
  ; test_put_fail [N "a";vel] "{a={}}" None
  ]

let _ = register_and_test
	  ("prune", T (Arrow (Name, Arrow (View, Lens))), prune_lib)
	  prune_unit_tests
	  
(* ADD *)
(* add - library interface *)
let add_lib = 
  Compiler.compile_focal 
    "let add n v = xfork {} {n} (const v {} ; plunge n) id do add"

(* add - native interface *)
let add n v =
  let r = add_lib in
  let r = (funOfArg r) (N n) in
  let r = (funOfArg r) (V v) in
    lensOfArg r

(* add - unit tests *)
let add_unit_tests = 
  let vel = V (Compiler.compile_view "[]") in
    [ test_get_eq [N "a";vel] "{}" (*=*) "{a=[]}"
    ; test_get_eq [N "a";vel] "{b={}}" (*=*) "{a=[] b={}}"
    ; test_put_eq [N "a";vel] "{a=[] b=[1 2 3]}" None (*=*) "{b=[1 2 3]}"
    ; test_put_eq [N "a";vel] "{a=[] b=[1 2 3]}" (Some "{}") (*=*) "{b=[1 2 3]}"
    ; test_get_fail [N "a"; vel] "{a={}}"
    ; test_put_fail [N "a"; vel] "{}" None
    ; test_put_fail [N "a"; vel] "{a={}}" (Some "{a={}}")
    ]

let _ = register_and_test 
	  ("add", T (Arrow (Name, Arrow (View,Lens))), add_lib)
	  add_unit_tests

(* FOCUS *)
(* focus - library interface *)
let focus_lib = 
  Compiler.compile_focal 
    "let focus n d = filter {n} d ; hoist n do focus"

(* focus - native interface *)
let focus n d =
  let r = focus_lib in
  let r = (funOfArg r) (N n) in
  let r = (funOfArg r) (V d) in
    lensOfArg r

(* focus - unit tests *)
let focus_unit_tests = 
  [ test_get_eq [N "a"; ve] "{a=[1 2 3] b=[4 5 6]}" (*=*) "[1 2 3]"
  ; test_put_eq [N "a"; ve] "{}" None (*=*) "{a={}}"
  ; test_put_eq [N "a"; ve] "[11 12 13]" (Some "{a=[1 2 3] b=[4 5 6]}") (*=*) "{a=[11 12 13] b=[4 5 6]}"
  ; test_get_fail [N "a"; ve] "{}" 
  ; test_get_fail [N "a"; ve] "{b={}}" 
    (* this test succeeds, which is funny b/c co is illegal
       ; test_put_fail [N "a"; ve] "[11 12 13]" (Some "{b=[4 5 6]}") 
    *)
  ]  

let _ = register_and_test 
	  ("focus", T (Arrow (Name, Arrow (View,Lens))), focus_lib)
	  focus_unit_tests

(* HOIST_NONUNIQUE *)
(* hoist_nonunique - library interface *)
let hoist_nonunique_lib = 
  Compiler.compile_focal 
    "let hoist_nonunique n p = xfork {n} p (hoist n) id do hoist_nonunique"
    
(* hoist_nonunique - native interface *)
let hoist_nonunique n p =
  let r = hoist_nonunique_lib in
  let r = (funOfArg r) (N n) in
  let r = (funOfArg r) (P p) in
    lensOfArg r      

(* hoist_nonunique - unit tests *)
let hoist_nonunique_unit_tests = 
  let pa = P (Prd.s "a") in
    [ test_get_eq [N "x";pa] "{x={a=[]} y=[1 2 3]}" (*=*) "{a=[] y=[1 2 3]}"
    ; test_put_eq [N "x";pa] "{a=[1 2 3] y=[4 5 6]}" None (*=*) "{x={a=[1 2 3]} y=[4 5 6]}"
    ; test_put_eq [N "x";pa] "{a=[1 2 3] y=[4 5 6]}" (Some "{x={a=[]}}") (*=*) "{x={a=[1 2 3]} y=[4 5 6]}"
    ; test_get_fail [N "x";pa] "{}"
    ]

let _ = register_and_test 
	  ("hoist_nonunique",T (Arrow (Name, Arrow (Predicate, Lens))), hoist_nonunique_lib)
	  hoist_nonunique_unit_tests

(* RENAME *)
(* rename - library interface *)
let rename_lib = 
  Compiler.compile_focal 
    "let rename m n = xfork {m} {n} (hoist m;plunge n) id do rename"

(* rename - native interface *)
let rename m n =
  let r = rename_lib in
  let r = (funOfArg r) (N m) in 
  let r = (funOfArg r) (N n) in 
    lensOfArg r

(* rename - unit tests *)
let rename_unit_tests = 
  [ test_get_eq [N "x"; N "y"] "{x=[1]}" (*=*) "{y=[1]}"
  ; test_get_eq [N "x"; N "y"] "{x=[1] z=[2]}" (*=*) "{y=[1] z=[2]}"
  ; test_put_eq [N "x"; N "y"] "{y=[1]}" None (*=*) "{x=[1]}"
  ; test_put_eq [N "x"; N "y"] "{y=[1] z=[2]}" None (*=*) "{x=[1] z=[2]}"
  ; test_get_fail [N "x"; N "y"] "{}"
  ; test_get_fail [N "x"; N "y"] "{y={}}"
  ; test_put_fail [N "x"; N "y"] "{}" None
  ; test_put_fail [N "x"; N "y"] "{x={}}" None
  ]

let _ = register_and_test 
	  ("rename", T (Arrow (Name, Arrow (Name, Lens))), rename_lib)
	  rename_unit_tests

(* RENAME_IF_PRESENT *)
(* rename_if_present - library interface *)  
let rename_if_present_lib = 
  Compiler.compile_focal 
    "let rename_if_present m n = 
       acond (hasChild m) (hasChild n) 
         (rename m n) 
         id
     do rename_if_present"

(* rename_if_present - native interface *)  
let rename_if_present n m =
  let r = rename_if_present_lib in
  let r = (funOfArg r) (N m) in 
  let r = (funOfArg r) (N n) in 
    lensOfArg r

(* rename_if_present - unit tests *)
let rename_if_present_unit_tests = 
[ test_get_eq [N "x";N "y"] "{}" (*=*) "{}"
; test_get_eq [N "x";N "y"] "{x=[1]}" (*=*) "{y=[1]}"
; test_get_eq [N "x";N "y"] "{x=[1] z=[2]}" (*=*) "{y=[1] z=[2]}"
; test_get_eq [N "x";N "y"] "{y=[1]}" (*=*) "{y=[1]}"
; test_put_eq [N "x";N "y"] "{y=[1]}" None (*=*) "{x=[1]}"
; test_put_eq [N "x";N "y"] "{y=[1] z=[2]}" None (*=*) "{x=[1] z=[2]}"
; test_put_eq [N "x";N "y"] "{x=[1]}" None (*=*) "{x=[1]}"
]

let _ = register_and_test 
	  ("rename_if_present",T (Arrow (Name, Arrow (Name, Lens))), rename_if_present_lib)
	  rename_if_present_unit_tests

(***********************)
(* DERIVED LIST LENSES *)
(***********************)

(* HD *)
(* hd - library interface *)
let hd_lib = 
  Compiler.compile_focal 
    "let hd d = focus *h {*t=d} do hd"

(* hd - native interface *)
let hd d =
  let r = hd_lib in
  let r = (funOfArg r) (V d) in
    lensOfArg r

(* hd - unit tests *)
let hd_unit_tests = 
  let vel = V (Compiler.compile_view "[]") in
  [ test_get_eq [vel] "[1]" (*=*) "{1={}}"
  ; test_put_eq [vel] "{1={}}" None (*=*) "[1]"
  ; test_put_eq [vel] "{1={}}" (Some "[2 3]") (*=*) "[1 3]"
  ; test_get_fail [vel] "[]"
  ]  
  
let _ = register_and_test 
	  ("hd", T (Arrow (View,Lens)), hd_lib)
	  hd_unit_tests

(* TL *)
(* tl - library interface *)
let tl_lib = 
  Compiler.compile_focal 
    "let tl d = focus *t {*h=d} 
     do tl"

(* tl - native interface *)
let tl d =
  let r = tl_lib in
  let r = (funOfArg r) (V d) in
    lensOfArg r

(* tl - unit tests *)
let tl_unit_tests = 
  [ test_get_eq [ve] "[1 2 3]" (*=*) "[2 3]"
  ; test_put_eq [ve] "[2 3]" None (*=*) "[{} 2 3]"
  ; test_put_eq [ve] "[2 3]" (Some "[1]") (*=*) "[1 2 3]"
  ; test_get_fail [ve] "[]"
  ]

let _ = register_and_test 
	  ("tl", T (Arrow (View,Lens)), tl_lib)
	  tl_unit_tests

(* LIST_MAP *)
(* list_map - library interface *)
let list_map_lib = 
  Compiler.compile_focal 
    "let list_map l =
       let rec aux = wmap < *h -> l *t -> aux > in aux
     do list_map"

(* list_map - native interface *)
let list_map l= 
  let r = list_map_lib in
  let r = (funOfArg r) (L l) in
    lensOfArg r

(* list_map - unit tests *)
let list_map_unit_tests = 
  let const_bb = L (const b b) in
    [ test_get_eq [const_bb] "[]" (*=*) "[]"
    ; test_get_eq [const_bb] "[1 2 3]" (*=*) "[b b b]"
    ; test_put_eq [const_bb] "[]" None (*=*) "[]"
    ; test_put_eq [const_bb] "[b b b]" (Some "[1 2]") (*=*) "[1 2 b]"
    ]

let _ = register_and_test
	  ("list_map", T (Arrow (Lens, Lens)), list_map_lib)
  	  list_map_unit_tests
	  
(* LIST_FILTER *)
(* list_filter - library interface *)
let list_filter_lib = 
  Compiler.compile_focal 
    "(* simple list filter. its PUT direction is not the same as list_filter, *)
     (*  below. Its GET direction is fine, and we use it as f_21 in the       *)
     (* instance of cond below                                                *)

     let old_list_filter D E = 
       let rec l = 
       ccond (isCons E (isListOf (union D E))) 
         (tl {error}; l)
         (wmap < *t -> l >)
       in l

    let append v = 
       let rec l = 
         acond isEmptyList (isCons all (isEmptyList))
           (const v {})
           (wmap <*t -> l>)
       in l

     let list_filter D E default_D =
       let rec l = 
         cond (isListOf E) (isEmptyList) (isCons D (isListOf D))
            (old_list_filter E D)
            (append [default_D])
            (const [] [])
            (inner_filter)
       and inner_filter =
         ccond (isCons E (isList_at_least_one D))
         (tl {error}; inner_filter)
         (wmap < *t -> l>)
       in l

     do list_filter"

(* list_filter - native interface *)
let list_filter d e default_D = 
  let r = list_filter_lib in
  let r = (funOfArg r) (Sc d) in
  let r = (funOfArg r) (Sc e) in
  let r = (funOfArg r) (V default_D) in
    lensOfArg r

let list_filter_unit_tests = 
  let da = Sc (equalDom a) in
  let db = Sc (equalDom b) in
    [ test_get_eq [da;db;va] "[]" (*=*) "[]"
    ; test_get_eq [da;db;va] "[{a=1} {a=2} {b=3} {a=4}]" (*=*) "[{a=1} {a=2} {a=4}]"
    ; test_put_eq [da;db;va] "[{a=1} {a=2} {a=3}]" None (*=*) "[{a=1} {a=2} {a=3}]"
    ; test_put_eq [da;db;va] "[{a=1} {a=2} {a=3}]" (Some "[{a=1} {a=2} {b=3} {a=4}]") (*=*) "[{a=1} {a=2} {b=3} {a=3}]"
    ; test_put_eq [da;db;va] "[{a=1} {a=2} {a=3}]" (Some "[{b=3}]") (*=*) "[{b=3} {a=1} {a=2} {a=3}]"
    ; test_put_eq [da;db;va] "[{a=1} {a=2} {a=3}]" (Some "[a {b=3}]") (*=*) "[{a=1} {b=3} {a=2} {a=3}]"
    ; test_put_eq [da;db;va] "[{a=1}]" (Some "[a a {b=3}]") (*=*) "[{a=1} {b=3}]"
    ]
      
let _ = register_and_test
	  ("list_filter", T(Arrow(Schema,Arrow(Schema, Arrow(View, Lens)))), list_filter_lib)
	  list_filter_unit_tests

(**************)
(* PREDICATES *)
(**************)
(* NEG *)
(*   : complements a set of names *)
(* neg - native interface *)
let neg p = Prd.neg p

(* neg - library interface *)
let neg_lib = F (function P p -> P (neg p)
		   | _ -> focal_type_error "neg")
		
let _ = register ("neg",T (Arrow (Predicate,Predicate)), neg_lib)
