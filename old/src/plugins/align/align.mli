type keyfactory = V.t option -> (unit -> Name.t)
type pos = Left | Right | Both

val default_kf : keyfactory
val aligner_lens : string -> string -> (unit -> Name.t) -> pos -> Lens.t
