
(* ----------------------------------------------------------------------- *)
(* Basic Definitions *)

(* Type of lenses *)
type ('a, 'b) t = { 
  get: 'a -> 'b;
  put: 'b -> 'a option -> 'a
}

(* External functions that extract the components of a lens *)
let get l c = l.get c
let put l a co = l.put a co

(* Convert two native functions to a lens *)
let native g p = {get = g; put = p}
		   
(* ----------------------------------------------------------------------- *)
(* Debugging support *)           

type stackframe =
    GetFrame of string * V.t
  | PutFrame of string * V.t * V.t option

let dumpframe fr =
  let m = match fr with
      GetFrame (s,c) -> [`String (s ^ " (get)"); `View c]
    | PutFrame (s,a,co) ->
        [`String (s ^ " (put)"); `View a; `String "into"; `View_opt co] in
    [`Break; `String "FRAME "] @ m
      
let stack = Misc.dynamic_var []

let error e =
  let curstack = Misc.dynamic_lookup stack in
  let st =
    if curstack = [] then []
    else
      [`Break
      ; `String
	(Misc.color "-------------------------------------------------" 
	   Misc.Yellow ~bold:true)
      ;`Break
      ; `String (Misc.color "STACK DUMP:" Misc.Yellow ~bold:true)
      ;`Break
      ]
      @ (Safelist.flatten_map dumpframe curstack) in
    raise 
      (Error.Native_error
	 (V.format_msg_as_string (e @ st)))
      
let trap_errors_in f x =
  try
    f x 
  with 
      (Error.Compile_error(i,fn,s)) -> 
	error ([`String (Error.string_of_file_info fn i)
	       ;`String s])
    | (Error.Native_error(s)) 
    | (Error.Fatal_error(s))
      -> error ([`String s])

let probe2 name callget callput =
  { get = (fun c -> callget name c (Misc.dynamic_lookup stack);
      c);
    put = (fun a co ->callput name a co (Misc.dynamic_lookup stack);
      a) }

let tracepoint s l =
  let with_frame fr f =
    Misc.dynamic_bind
      stack (fr :: (Misc.dynamic_lookup stack))
      (trap_errors_in f)
  in
    { get = 
	(fun a -> with_frame (GetFrame(s,a)) (fun () -> l.get a));
      put = 
	(fun a co -> with_frame (PutFrame(s, a, co)) (fun () -> l.put a co))
    }

(* memoization stuff *)
let memoize_lens l = 
  let memotable = V.Hash.create 1 in	
    (* We use memo information in both directions -- to
       short-circuit a get when we see it for the second time, and
       also to avoid computing the put when we can see what its
       result must be from the GetPut law *)
    native 
      (fun c -> 
	 try
	   V.Hash.find memotable c
	 with Not_found -> begin
	   let a = get l c in
	     V.Hash.add memotable c a;
	     a
	 end)
      (fun a co -> 
	 match co with
	     None -> put l a None
	   | Some c ->
	       try
		 let a' = V.Hash.find memotable c in
		   if a' == a then c else put l a co
	       with Not_found -> put l a co)
      

(* (\* ----------------------------------------------------------------------- *\) *)
(* (\* Recursive lenses *\) *)

(* (\* We might be able to improve performance by doing the hashing eagerly *)
(*    instead of waiting till lookup time... *\) *)
(* let definitions = Hashtbl.create 1 *)

(* (\* Note that we do a bit of memoization here... *\) *)
(* let define ?(hashtable=definitions) n l = *)
(*   if Hashtbl.mem hashtable n then *)
(*     error [`String "A lens named "; `String n; `String " is already defined"] *)
(*   else Hashtbl.add hashtable n (Value.memoize_lens l) *)


(* (\* A convenient way to create a recursive lens without giving it a meaningful name *\) *)
(* let nextid = ref 0 *)
(* let fix f = *)
(*   let n = "fix" ^ (string_of_int !nextid) in *)
(*   nextid := !nextid + 1; *)
(*   let l = f n in *)
(*   define n l; *)
(*   l *)
