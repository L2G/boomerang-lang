open Bident
module G = Bregistry
module V = Bvalue

(* signature *)
module type CEnvSig = 
sig
  type t 
  type v
  val empty : Qid.t -> t
  val get_ev : t -> G.REnv.t
  val set_ev : t -> G.REnv.t -> t
  val get_ctx : t -> Qid.t list
  val set_ctx : t -> Qid.t list -> t
  val get_mod : t -> Qid.t 
  val set_mod : t -> Qid.t -> t
  val lookup : t -> Qid.t -> v option
  val lookup_type : t -> Qid.t -> (Qid.t * G.tspec) option
  val lookup_con : t -> Qid.t -> (Qid.t * G.tspec) option
  val update : t -> Qid.t -> v -> t
  val update_list : t -> (Qid.t * v) list -> t
  val update_type : t -> Id.t list -> Qid.t -> G.tcon list -> t
  val fold : (Qid.t -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

(* compilation environment *)
module CEnv : CEnvSig with type v = G.rv = 
struct
  type t = (Qid.t list * Qid.t) * G.REnv.t
  type v = G.rv

  let empty m = (([],m), G.REnv.empty ())

  (* accessors / setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_mod cev = let ((_,m),_) = cev in m
  let set_mod cev m = let ((os,_),ev) = cev in ((os,m),ev)
  let get_ctx cev = let ((os,_),_) = cev in os
  let set_ctx cev os = let ((_,m),ev) = cev in ((os,m),ev)

  (* lookup from cev, then from library *)
  let lookup_generic lookup_fun lookup_library_fun cev q = 
    let ev = get_ev cev in 
    let ctx = get_ctx cev in 
    let rec aux nctx q2 = match lookup_fun ev q2 with
      | Some r -> Some r
      | None -> begin  match nctx with
          | [] -> None
          | o::orest -> aux orest (Qid.t_dot_t o q)
        end in 
    match aux ctx q with
      | Some r -> Some r
      | None -> lookup_library_fun ctx q
          
  let lookup cev q = 
    lookup_generic 
      G.REnv.lookup 
      G.lookup_library_ctx 
      cev q 

  let lookup_type cev q = 
    lookup_generic 
      G.REnv.lookup_type
      G.lookup_type_library_ctx 
      cev q 

  let lookup_con cev q = 
    lookup_generic 
      G.REnv.lookup_con
      G.lookup_con_library_ctx 
      cev q 

  let update cev q rv = 
    set_ev cev (G.REnv.update (get_ev cev) q rv)

  let update_list cev qs = 
    Safelist.fold_left 
      (fun cev (q,sv) -> update cev q sv)
      cev qs
            
  let update_type cev svars q cl = 
    set_ev cev (G.REnv.update_type (get_ev cev) svars q cl)

  let fold f cev a = G.REnv.fold f (get_ev cev) a
end
type cenv = CEnv.t

(* sort checking environment *)
module SCEnv : CEnvSig with type v = G.rs = 
struct
  type t = CEnv.t
  type v = G.rs

  let dummy_value = V.Unt (Info.M "dummy value")
  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_mod = CEnv.get_mod
  let set_mod = CEnv.set_mod
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx

  let lookup sev q = 
    match CEnv.lookup sev q with 
    | None -> None
    | Some (s,_) -> Some s
  let lookup_type = CEnv.lookup_type
  let lookup_con = CEnv.lookup_con 
  let update sev q s = CEnv.update sev q (s,dummy_value)
  let update_list sev qs = 
    Safelist.fold_left
      (fun sev (q,s) -> update sev q s)
      sev qs
  let update_type sev svars q cs = CEnv.update_type sev svars q cs
  let fold f sev a = CEnv.fold (fun q (s,_) a -> f q s a) sev a
end