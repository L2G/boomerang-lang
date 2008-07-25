open Bident

module type CEnvSig = 
sig
  type t 
  type v
  val empty : Qid.t -> t
  val get_ev : t -> Bregistry.REnv.t
  val set_ev : t -> Bregistry.REnv.t -> t
  val get_ctx : t -> Qid.t list
  val set_ctx : t -> Qid.t list -> t
  val get_mod : t -> Qid.t 
  val set_mod : t -> Qid.t -> t
  val lookup : t -> Qid.t -> v option
  val lookup_type : t -> Qid.t -> (Qid.t * Bregistry.tspec) option
  val lookup_con : t -> Qid.t -> (Qid.t * Bregistry.tspec) option
  val update : t -> Qid.t -> v -> t
  val update_list : t -> (Qid.t * v) list -> t
  val update_type : t -> Id.t list -> Qid.t -> Bregistry.tcon list -> t
  val fold : (Qid.t -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

(* environment mapping names to (possibly typed) values *)
module CEnv : CEnvSig with type v = Bregistry.rv

(* environment mapping names to types *)
module SCEnv : CEnvSig with type v = Bregistry.rs

