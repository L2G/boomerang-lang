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
module RL = L.RLens
module KL = L.KLens
module SL = L.SLens
module CL = L.CLens
module RS = Rstring

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* uids *)
let uid_cell = ref 0 
let uid () = 
  incr uid_cell;
  !uid_cell

(* sorts *)
(* the base sorts are string, regexp, and the four kinds of lenses;
   the single constructor on sorts is for functions *)
type s = 
    SString      (* strings *)
    | SRegexp    (* regular expressions *)
    | SCanonizer (* canonizers *)
    | SRLens     (* R-lenses *)
    | SKLens     (* K-lenses *)
    | SSLens     (* S-lenses *)
    | SCLens     (* C-lenses : "classic" lenses *)
    | SFunction of s * s option  (* functions and arg sort; optionally a return-sort *)

(* infix notation for writing function sorts *)
let ( ^> ) s1 s2 = SFunction(s1,Some s2)

(* run-time values; correspond to each sort *)
type t = 
      S of Info.t * int * RS.t 
    | R of Info.t * int * L.r
    | CN of Info.t * int * CN.t
    | RL of Info.t * int * RL.t
    | KL of Info.t * int * KL.t
    | SL of Info.t * int * SL.t
    | CL of Info.t * int * CL.t
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
      SString -> S(i,0,RS.empty)
    | SRegexp -> R(i,0,L.rx_str false RS.empty) 
    | SCanonizer -> CN(i,0,CL.canonizer_of_t (CL.copy i (L.rx_epsilon)))
    | SRLens -> RL(i,0,RL.t_of_slens (SL.t_of_clens (CL.copy i (L.rx_epsilon))))
    | SCLens -> CL(i,0,CL.copy i (L.rx_epsilon))
    | SKLens -> KL(i,0,(KL.t_of_clens (CL.copy i (L.rx_epsilon))))
    | SSLens -> SL(i,0,(SL.t_of_clens (CL.copy i (L.rx_epsilon))))
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
  | S(i,_,_) -> i
  | R(i,_,_) -> i
  | CN(i,_,_) -> i
  | RL(i,_,_) -> i
  | CL(i,_,_) -> i
  | KL(i,_,_) -> i
  | SL(i,_,_) -> i
  | F(i,_,_) -> i

(* uid_of_t : t -> Uid.t 
 *
 * [uid_of_t t] returns the uid associated to a run-time value 
 *)
let uid_of_t = function 
  | S(_,u,_) -> u
  | R(_,u,_) -> u
  | CN(_,u,_) -> u
  | RL(_,u,_) -> u
  | CL(_,u,_) -> u
  | KL(_,u,_) -> u
  | SL(_,u,_) -> u
  | F(_) -> 
      raise (Error.Harmony_error (fun () -> 
        Util.format "Pvalue.uid_of_t with function type"))
(* sort_of_t : t -> s
 * 
 * [sort_of_t t] returns the sort of a run-time value; note that for
 * functions, we only compute the argument type 
 *)
let rec sort_of_t = function
    S(_) -> SString
  | R(_) -> SRegexp
  | CN(_) -> SCanonizer
  | RL(_) -> SRLens
  | CL(_) -> SCLens
  | KL(_) -> SKLens
  | SL(_) -> SSLens
  | F(i,s1,f) -> SFunction(s1,None) 

(* string_of_sort : s -> string
 *
 * [string_of_sort s] produces a string representing [s] 
 *)
let rec string_of_sort = function
    SString -> "string"
  | SRegexp -> "regexp"
  | SCanonizer -> "canonizer"
  | SRLens -> "rlens"
  | SKLens -> "klens"
  | SSLens -> "slens"
  | SCLens -> "clens"
  | SFunction(s1,None) -> sprintf "(%s -> ?)" (string_of_sort s1) 
  | SFunction(s1,Some s2) -> sprintf "(%s -> %s)" (string_of_sort s1) (string_of_sort s2)

let is_lens_sort = function
    SRLens | SKLens | SSLens | SCLens -> true
  | _ -> false

let get_lens_sorts = function
  | S(_,_,s)   -> let r = L.rx_str false s in r,r
  | R(_,_,r)   -> r,r 
  | RL(_,_,rl) -> RL.ctype rl, RL.atype rl
  | KL(_,_,kl) -> KL.ctype kl, KL.atype kl
  | SL(_,_,sl) -> SL.ctype sl, SL.atype sl
  | CL(_,_,cl) -> CL.ctype cl, CL.atype cl
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

  (* subsorts of R-lenses *)
  | SString,SRLens -> true
  | SRegexp,SRLens -> true
  | SSLens,SRLens -> true
  | SCLens,SRLens -> true

  (* subsorts of K-lenses *)
  | SString,SKLens -> true
  | SRegexp,SKLens -> true
  | SCLens,SKLens -> true

  (* subsorts of S-lenses *)  
  | SString,SSLens -> true
  | SRegexp,SSLens -> true
  | SCLens,SSLens -> true

  | SString,SCLens -> true
  | SRegexp,SCLens -> true

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
    S(_,_,s) -> s
  | _ -> conversion_error i SString v

(* get_r: t -> Info.t -> L.rx.t
 * 
 * [get_r v i] returns the regexp that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_r v i = match v with
    R(_,_,r) -> r
  | S(_,_,s) -> L.rx_str false s
  | _ -> conversion_error i SRegexp v

(* get_rl: t -> Info.t -> L.PLens.t
 * 
 * [get_rl v i] returns the R-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_rl v i = match v with 
  | S(_,_,s) -> RL.t_of_slens (SL.t_of_clens (CL.copy i (L.rx_str false s)))
  | R(_,_,r) -> RL.t_of_slens (SL.t_of_clens (CL.copy i r))
  | RL(_,_,rl) -> rl
  | SL(_,_,sl) -> RL.t_of_slens sl
  | CL(_,_,cl) -> RL.t_of_clens cl
  | _ -> conversion_error i SRLens v

(* get_cn: t -> Info.t -> CN.t
 * 
 * [get_cn v i] returns the canonizer that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_cn v i = match v with 
  | CN(_,_,cn) -> cn
  | _ -> conversion_error i SCanonizer v

(* get_kl: t -> Info.t -> L.CLens.t
 * 
 * [get_kl v i] returns the K-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_kl v i = match v with 
  | S(_,_,s)  -> KL.t_of_clens (CL.copy i (L.rx_str false s))
  | R(_,_,r)  -> KL.t_of_clens (CL.copy i r)
  | KL(_,_,kl) -> kl
  | CL(_,_,cl) -> KL.t_of_clens cl
  | _ -> conversion_error i SKLens v

(* get_cl: t -> Info.t -> L.CLens.t
 * 
 * [get_sl v i] returns the B-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_sl v i = match v with 
  | S(_,_,s)  -> SL.t_of_clens (CL.copy i (L.rx_str false s))
  | R(_,_,r)  -> SL.t_of_clens (CL.copy i r)
  | SL(_,_,sl) -> sl
  | CL(_,_,cl) -> SL.t_of_clens cl
  | _ -> conversion_error i SSLens v

(* get_cl: t -> Info.t -> L.CLens.t
 * 
 * [get_rl v i] returns the B-lens that [v] represents, or throws an
 * exception if [v] is a run-time value representing a different
 * sort. [i] is used to report errors.  
 *)
let get_cl v i = match v with 
  | S(_,_,s) -> CL.copy i (L.rx_str false s)
  | R(_,_,r) -> CL.copy i r
  | CL(_,_,cl) -> cl
  | _ -> conversion_error i SCLens v

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

(* mk_rlfun: Info.t -> (L.RLens.t -> t) -> (t -> t)
 * 
 * [mk_rlfun i f] takes a [L.RLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SRLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_rlfun i f = F(i,SRLens,(fun i v -> f i (get_rl v i)))

(* mk_slfun: Info.t -> (L.SSLens.t -> t) -> (t -> t)
 * 
 * [mk_slfun i f] takes a [L.SSLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SSLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_slfun i f = F(i,SSLens,(fun i v -> f i (get_sl v i)))

(* mk_klfun: Info.t -> (L.KLens.t -> t) -> (t -> t)
 * 
 * [mk_klfun i f] takes a [L.KLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SKLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_klfun i f = F(i,SKLens,(fun i v -> f i (get_kl v i)))

(* mk_clfun: Info.t -> (L.CLens.t -> t) -> (t -> t)
 * 
 * [mk_clfun i f] takes a [L.CLens.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SCLens]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_clfun i f = F(i,SCLens,(fun i v -> f i (get_cl v i)))


(* mk_cnfun: Info.t -> (CN.t -> t) -> (t -> t)
 * 
 * [mk_cnfun i f] takes a [CN.t -> t] function and yields a [t -> t]
 * function that expects its argument to actually be an [SCanonizer]. [i] is
 * used to report errors when the argument has a different sort.
 *)
let mk_cnfun i f = F(i,SCanonizer,(fun i v -> f i (get_cn v i)))
