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

let coerce_lens (desc : string) (l : ('a, 'b) t) f f' g g' : ('c, 'd) t =
  let i = Info.M desc in
  let getfun c = f i (get l (f' i c)) in
  let putfun a co = g i (put l (g' i a) (Misc.map_option (g' i) co)) in
  native getfun putfun

let tree i t = V.Tree t
let db i t = V.Db t

let v_of_tree l = coerce_lens "v_of_tree" l tree V.tree_of tree V.tree_of
let v_of_db l = coerce_lens "v_of_db" l db V.db_of db V.db_of
let tree_of_v l = coerce_lens "tree_of_v" l V.tree_of tree V.tree_of tree
let db_of_v l = coerce_lens "db_of_v" l V.db_of db V.db_of db

type stackframe =
    GetFrame of string * V.t
  | PutFrame of string * V.t * V.t option

let dumpframe fr =
  (* FIXME *)
  let m = match fr with
      GetFrame (s,c) -> [`String (s ^ " (get)") (*; `Tree c*)]
    | PutFrame (s,a,co) ->
        [`String (s ^ " (put)"); (*`Tree a;*) `String "into" (*; `Tree_opt co*)] in
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
    raise (Error.Harmony_error
             (fun () -> V.format_msg (e @ st)))

let probe2 name callget callput =
  { get = (fun c -> callget name c (Misc.dynamic_lookup stack);
      c);
    put = (fun a co ->callput name a co (Misc.dynamic_lookup stack);
      a) }

let tracepoint s l =
  let with_frame fr f =
    Misc.dynamic_bind
      stack (fr :: (Misc.dynamic_lookup stack))
      f
  in
    { get = 
        (fun a -> with_frame (GetFrame(s,a)) (fun () -> l.get a));
      put = 
        (fun a co -> with_frame (PutFrame(s, a, co)) (fun () -> l.put a co))
    }

(* memoization stuff *)
let hits = ref 0
let misses = ref 0
let rate () = 100.0 *. (float_of_int !hits) /. (float_of_int !misses)
let memoize_lens l = 
  let memotable = V.Hash.create 1 in        
    (* We use memo information in both directions -- to
       short-circuit a get when we see it for the second time, and
       also to avoid computing the put when we can see what its
       result must be from the GetPut law *)
    native 
      (fun c -> 
         try
           let r = V.Hash.find memotable c in
           let _ = incr hits in
             r
         with Not_found -> begin
           incr misses;
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
