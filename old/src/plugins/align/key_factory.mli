

val register_keyfactory :  
  Align.keyfactory
  -> (Surveyor.encoding_key * Surveyor.type_desc) -> unit
(** [register_keyfactory keyf pair] registers [keyf] with the surveyor,
    and associates it with [pair]. *)

val get_keyfactory : 
  (Surveyor.encoding_key * Surveyor.type_desc)
     -> Align.keyfactory
(** [get_keyfactory pair] gets the keyfactory that's associated with
    [pair], or Align.default_kf if there is no entry for [pair]. *)
