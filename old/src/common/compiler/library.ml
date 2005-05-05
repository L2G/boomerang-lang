(*****************)
(* FOCAL LIBRARY *)
(*****************)

open Syntax
open Error 

(*------------------------------------------------------------------------*)
(* GLOBAL DEFINITIONS *)

(* backpatch horribleness: we need Compiler.compile_view, 
 * but don't want to introduce circular dependency *)
let compile_view_impl = ref (fun s -> V.empty)
let compile_view s = !compile_view_impl s
let compile_view_opt so = 
  match so with 
    | None -> None 
    | Some s -> Some (compile_view s)
	
(* library: stores lens names, types, and implementations *)
let library = ref []
let get_lens_library () = ! library

(*------------------------------------------------------------------------*)
(* UNIT TESTS *)

(* type of the unit test suite *)
type test_suite = unit -> unit
  
(* the tests (initially empty)   *)
let unit_tests : test_suite ref = ref (fun () -> ())
let get_tests () = ! unit_tests
		     
(* unit test descriptions *)
type test_desc = 
  | GetTest of 
      (Syntax.arg list) 
      * V.t 
      * (V.t -> V.t -> bool) * string 
      * V.t
  | PutTest of 
      (Syntax.arg list) 
      * V.t * (V.t option) 
      * (V.t -> V.t -> bool) * string 
      * V.t
  | GetFailTest of 
      (Syntax.arg list) 
      * V.t 
  | PutFailTest of 
      (Syntax.arg list) 
      * V.t * (V.t option)

(* some common ways to describe unit tests *)
(* e.g., describe an equality test where views are compiled from strings *)
let test_get_eq args c a = 
  try
    GetTest(args, compile_view c, V.equal, "=", compile_view a)
  with
      (V.Illformed(msg, vs)) ->
	let _ = (Format.printf "ERROR compiling test %s" msg) in
	let _ = (List.map V.format vs) in
	  assert false
	    
let test_put_eq args a co c = 
  try
    PutTest(args, compile_view a, compile_view_opt co, V.equal, "=", compile_view c)
  with
      (V.Illformed(msg, vs)) ->
	let _ = (Format.printf "ERROR compiling test %s" msg) in
	let _ = (List.map V.format vs) in
	  assert false

let test_get_fail args c = 
  try
    GetFailTest(args, compile_view c)
  with
      (V.Illformed(msg, vs)) ->
	let _ = (Format.printf "ERROR compiling test %s" msg) in
	let _ = (List.map V.format vs) in
	  assert false

let test_put_fail args a co = 
  try
    PutFailTest(args, compile_view a, compile_view_opt co)
  with
      (V.Illformed(msg, vs)) ->
	let _ = (Format.printf "ERROR compiling test %s" msg) in
	let _ = (List.map V.format vs) in
	  assert false

(* compute_lens_opt
 * applies a Focal expression to a list of args, yielding a lens option 
 *)
let rec compute_lens_opt (e:Syntax.arg) (args:Syntax.arg list) : (Lens.t option) = 
  match (e,args) with 
    | (F f, h::t) -> compute_lens_opt (f h) t
    | (L l, [])   -> Some l 	
    | _           -> None

(* compute_lens
 * applies a Focal expression to a list of args, yielding a lens or throwing an exception *)
let rec compute_lens (e:Syntax.arg) (args:Syntax.arg list) : (Lens.t) = 
  match (compute_lens_opt e args) with 
    | Some l -> l
    | None   -> raise (Run_error (("Arguments do not yield a lens"),bogusInfo))

(* compute_test - convert a test description to a () -> () function *)
let rec compute_test name exp test other_tests = 
  try
    match test with 
      | GetTest (args,c,cmp,cmp_str,a) ->
	  let l = compute_lens exp args in	   
	  let a' = Lens.get l c in
	    (fun () ->
	       other_tests ();
	       if (not (cmp a a')) then
		 (V.format_msg 
		    [ `String "--- GET TEST ("
		    ; `String cmp_str
		    ; `String ") FAILED for "
		    ; `String name
		    ; `String " ---"
		    ;  `String "\nc = "
		    ; `View c
		    ; `String "(get c) = "
		    ; `View a'
		    ; `String ("SPECIFICATION: \na= ")
		    ; `View a
		    ];
		  Format.print_newline ()))
      | PutTest (args,a,co,cmp, cmp_str, c) ->
	    let l = compute_lens exp args in	   
	    let c' = Lens.put l a co in 
	    let co_msg = match co with 
		None -> `String "MISSING"
	      | Some c -> `View c in
	      (fun () ->
		 other_tests ();
		 if not (cmp c c') then		 
		 (V.format_msg 
		    [ `String "--- PUT TEST ("
		    ; `String cmp_str
		    ; `String ") FAILED for "
		    ; `String name
		    ; `String " ---"
		    ;  `String "\na = "
		    ; `View a
		    ;  `String "co = "
		    ; `View_opt co
		    ; `String "(put a co) = "
		    ; `View c'
		    ; `String ("SPECIFICATION: \nc= ")
		    ; `View c
		    ];
		  Format.print_newline ()))
      | GetFailTest (args,c) ->
	  let l = compute_lens exp args in
	    (fun () -> 
	       other_tests ();
	       (try
		  let a = Lens.get l c in
		    V.format_msg
		      [ `String ("--- GET succeeded for " ^ name ^ "; should have FAILED ---")
		      ; `String "\nc = "
		      ; `View c
		      ; `String "(get c) = "
		      ; `View a
		      ];
		    Format.print_newline ()
		  with V.Error _ -> ()))
	| PutFailTest (args,a,co) ->
	    let l = compute_lens exp args in
	      (fun () -> 
		 other_tests ();				  
		 (try
		    let c = Lens.put l a co in
		      V.format_msg
			[ `String ("--- PUT succeeded for " ^ name ^ "; should have FAILED ---")
			; `String "\na = "
			; `View a
			; `String "co = "
			; `View_opt co
			; `String "(put a co) = "
			; `View c
			];
		      Format.print_newline ()
		  with V.Error _ -> ()))
  with e -> (V.format_msg [ `String "Error computing unit tests for "
			  ; `String name]; 
	     raise e)

    
(*------------------------------------------------------------------------*)
(* LIBRARY REGISTRATION INTERFACE *)

let abort_on_test_failure = true

(* Register a lens with tests *)
let rec register_and_test ((name,typ,exp):(string*typeschema*arg)) tests =   
  try
    unit_tests := List.fold_right (compute_test name exp) tests (get_tests ());
    library := (name,typ,exp)::(!library)
  with e -> 
    begin
      if (abort_on_test_failure) then
	raise e
      else
	Format.print_string ("FAILURE in Library.register_and_test:\n\t"^(Printexc.to_string e))
    end
	

(* Register a lens without tests *)       
let register x = register_and_test x []

(* Lookup an optionally-defined lens from the library *)		   
let lookup x = 
  Safelist.fold_right 
    (fun (name,typ,exp) prev_match -> 
       if (name = x) then (Some (typ,exp)) 
       else prev_match) 
    (!library)
    None 
    
(* Lookup a particular lens from the library, throwing an exception if 
   it is not there *)
let lookup_required x = 
  match (lookup x) with
    | Some a -> a
    | None -> 
	raise (Run_error (("Missing required definition: " ^ x),bogusInfo))
