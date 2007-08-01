(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* pvalue.ml : run-time values                               *)
(*************************************************************)
(* $Id$ *)

(* environments *)
module Renv = Env.Make(struct
  type t = string
  let compare = Pervasives.compare
  let to_string s = s
end)
let lookup i e s = match Renv.lookup e s with
    None -> Error.simple_error (Printf.sprintf "%s: %s not found" (Info.string_of_t i) s)     
  | Some v -> v

(* module imports and abbreviations *)
module L = Rlenses
module CN = L.Canonizer
module DL = L.DLens
module StL = L.StLens
module RS = Rstring

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* sorts *)
(* the base sorts are string, regexp, and the four kinds of lenses;
   the single constructor on sorts is for functions *)
type s = 
    SString      (* strings *)
    | SRegexp    (* regular expressions *)
    | SCanonizer (* canonizers *)
    | SDLens     (* D-lenses *)
    | SStLens    (* Streaming lenses *)
    | SFunction of s * s option  (* functions and arg sort; optionally a return-sort *)

(* infix notation for writing function sorts *)
let ( ^> ) s1 s2 = SFunction(s1,Some s2)

(* run-time values; correspond to each sort *)
type t = 
      S of Info.t * RS.t 
    | R of Info.t * L.r
    | CN of Info.t * CN.t
    | DL of Info.t * DL.t
    | StL of Info.t * StL.t
    | F of Info.t * s * (Info.t -> t -> t)

(* mk_dummy: s -> t
 * 
 * make a dummy run-time value from a sort.
 * 
 * note that in the function case, the constructed function is a
 * constant raising an exception when the result type is not known 
 *)
let rec mk_dummy = 
  let i = Info.M "dummy" in function
      SString -> S(i,RS.empty)
    | SRegexp -> R(i,L.rx_str false RS.empty) 
    | SCanonizer -> CN(i,DL.canonizer_of_t i (DL.copy i (L.rx_epsilon)))
    | SDLens -> DL(i,(DL.copy i (L.rx_epsilon)))
    | SStLens -> assert false (* to be done, if usefull*) 
    | SFunction(s1,Some s2) -> F(i,s1,(fun _ _ -> mk_dummy s2))
    | SFunction(s1,None) -> 
        F(i,s1,(fun _ _ -> 
          raise (Error.Harmony_error (fun () -> 
            Util.format "Pvalue.mk_dummy from unknown function type"))))

(* info_of_t : t -> Info.t 
 *
 * [info_of_t t] returns the lexing info associated to a run-time value 
 *)
let info_of_t = function 
  | S(i,_) -> i
  | R(i,_) -> i
  | CN(i,_) -> i
  | DL(i,_) -> i
  | StL(i,_) -> i
  | F(i,_,_) -> i

(* sort_of_t : t -> s
 * 
 * [sort_of_t t] returns the sort of a run-time value; note that for
 * functions, we only compute the argument type 
 *)
let rec sort_of_t = function
    S(_) -> SString
  | R(_) -> SRegexp
  | CN(_) -> SCanonizer
  | DL(_) -> SDLens
  | StL(_) -> SStLens
  | F(i,s1,f) -> SFunction(s1,None) 

(* string_of_sort : s -> string
 *
 * [string_of_sort s] produces a string representing [s] 
 *)
let rec string_of_sort = function
    SString -> "string"
  | SRegexp -> "regexp"
  | SCanonizer -> "canonizer"
  | SDLens -> "dlens"
  | SStLens -> "streaming lens"
  | SFunction(s1,None) -> sprintf "(%s -> ?)" (string_of_sort s1) 
  | SFunction(s1,Some s2) -> sprintf "(%s -> %s)" (string_of_sort s1) (string_of_sort s2)

let is_lens_sort = function
    SDLens -> true
  | _ -> false

let get_lens_sorts = function
  | S(_,s)   -> let r = L.rx_str false s in r,r
  | R(_,r)   -> r,r 
  | DL(_,dl) -> DL.ctype dl, DL.atype dl
  | StL(_, stl) -> StL.ctype stl, StL.atype stl
  | v -> 
      raise (Error.Harmony_error (fun () -> 
        Util.format "Pvalue.get_lens_sorts: cannot get lens sorts from %s" 
          (string_of_sort (sort_of_t v))))

(* subsort : s -> s -> bool
 * 
 * [subsort s1 s2] returns [true] iff an [s1] can 
 * be converted into a [s2] 
 *)
let rec subsort u v = match u,v with        
  (* subsorts of regexp *)
  | SString,SRegexp -> true

  (* subsorts of D-lenses *)
  | SString,SDLens -> true
  | SRegexp,SDLens -> true

  (* subsorts of functions *)
  | SFunction(s11,Some s12),SFunction(s21,Some s22) -> 
      let b1 = subsort s21 s11  in
      let b2 = subsort s12 s22 in 
        (b1 && b2)
  | SFunction(s11,_),SFunction(s21,_) -> 
      (* if we don't know the return type, be conservative *)
      false

  (* otherwise, only identical sorts *)
  | _ -> u=v

let join s1 s2 = 
  if subsort s1 s2 then Some s2
  else if subsort s2 s1 then Some s1 
  else None

(* --------- conversions between run-time values ---------- *)
let conversion_error i s1 v1 = 
  Error.simple_error 
    (sprintf "%s: expected %s, but found %s" 
        (Info.string_of_t i) 
        (string_of_sort s1)
        (string_of_sort (sort_of_t v1)))

(* get_s: t -> Info.t -> string
 * 
 * [get_s v i] returns the string that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.
*)
let get_s v i = match v with
    S(_,s) -> s
  | _ -> conversion_error i SString v

(* get_r: t -> Info.t -> L.rx.t
 * 
 * [get_r v i] returns the regexp that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_r v i = match v with
    R(_,r) -> r
  | S(_,s) -> L.rx_str false s
  | _ -> conversion_error i SRegexp v

(* get_dl: t -> Info.t -> L.DLens.t
 * 
 * [get_dl v i] returns the D-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_dl v i = match v with 
  | S(_,s) -> DL.copy i (L.rx_str false s)
  | R(_,r) -> DL.copy i r
  | DL(_,dl) -> dl
  | _ -> conversion_error i SDLens v

(* get_stl: t -> Info.t -> L.StLens.t
 * 
 * [get_stl v i] returns the Streaming-lens that [v] represents, or
 * throws an exception if [v] is a run-time value representing a
 * different sort. [i] is used to report errors.  
 *)
let get_stl v i = match v with 
  | StL(_,stl) -> stl
  | _ -> conversion_error i SStLens v

(* get_cn: t -> Info.t -> CN.t
 * 
 * [get_cn v i] returns the canonizer that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_cn v i = match v with 
  | CN(_,cn) -> cn
  | _ -> conversion_error i SCanonizer v


(* get_f: t -> Info.t -> (t -> t) 
 * 
 * [get_f v i] returns the [t -> t] function that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_f v i = match v with
  | F(_,_,f) -> f
  | _ ->
      Error.simple_error 
        (sprintf "%s: expected function, but found %s" 
            (Info.string_of_t i) 
            (string_of_sort (sort_of_t v)))

(* --------- constructors for functions on run-time values ---------- *)

(* mk_sfun: Info.t -> (RS.t -> t) -> (t -> t)
 * 
 * [mk_sfun i f] takes a [RS.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SString]. [i] is
 * used to report errors when the argument has a different sort. 
 *)
let mk_sfun i f = F(i,SString,(fun i v -> f i (get_s v i)))

(* mk_rfun: Info.t -> (L.rx.t -> t) -> (t -> t)
 * 
 * [mk_rfun i f] takes a [L.rx.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be a [SRegexp]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_rfun i f = F(i,SRegexp,(fun i v -> f i (get_r v i)))

(* mk_dlfun: Info.t -> (L.DLens.t -> t) -> (t -> t)
 * 
 * [mk_dlfun i f] takes a [L.DLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SDLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_dlfun i f = F(i,SDLens,(fun i v -> f i (get_dl v i)))

(* mk_stlfun: Info.t -> (L.StLens.t -> t) -> (t -> t)
 * 
 * [mk_dlfun i f] takes a [L.DLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SDLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_slfun i f = F(i,SStLens,(fun i v -> f i (get_stl v i)))

(* mk_cnfun: Info.t -> (CN.t -> t) -> (t -> t)
 * 
 * [mk_cnfun i f] takes a [CN.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SCanonizer]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_cnfun i f = F(i,SCanonizer,(fun i v -> f i (get_cn v i)))
